#' Query arguments for one page of the data endpoint
#' @noRd
subs_args_ <- function(start, limit, query = NULL, fields = NULL) {
  args <- list(limit = sprintf("%d", limit))
  if (start > 0) {
    args$start <- sprintf("%d", start)
  }
  if (!is.null(query)) {
    args$query <- query
  }
  if (!is.null(fields)) {
    args$fields <- fields
  }
  args
}

#' @importFrom RcppSimdJson fparse
#' @noRd
parse_subs_page_ <- function(json) {
  res <- fparse(json, max_simplify_lvl = "data_frame")
  list(count = res$count, results = res$results)
}

#' @noRd
fetch_subs_page_ <- function(uid, start, limit, query = NULL, fields = NULL) {
  path <- paste0("api/v2/assets/", uid, "/data.json")
  res <- xget(path = path, args = subs_args_(start, limit, query, fields))
  parse_subs_page_(res)
}

#' @importFrom crul AsyncQueue HttpRequest
#' @importFrom dplyr case_when
#' @importFrom rlang abort
#' @noRd
fetch_subs_async_ <- function(
  uid,
  start,
  limit,
  query = NULL,
  fields = NULL,
  n_retry = 3L
) {
  headers <- list(
    Authorization = paste("Token", Sys.getenv("KOBOTOOLBOX_TOKEN")),
    `User-Agent` = user_agent_()
  )
  url <- file.path(
    Sys.getenv("KOBOTOOLBOX_URL"),
    "api/v2/assets",
    uid,
    "data.json"
  )
  reqs <- lapply(seq_along(start), function(i) {
    req <- HttpRequest$new(url, headers = headers)
    req$retry(
      "get",
      query = subs_args_(start[i], limit[i], query, fields),
      times = n_retry,
      retry_only_on = c(500, 503),
      terminate_on = 404
    )
  })
  size <- sum(limit)
  sleep <- case_when(
    size > 30000 ~ 0.25,
    size > 15000 ~ 0.2,
    size > 5000 ~ 0.15,
    size > 1000 ~ 0.1,
    .default = 0.05
  )
  res <- AsyncQueue$new(.list = reqs, bucket_size = 15L, sleep = sleep)
  res$request()
  cond <- res$status_code() >= 300L
  if (any(cond)) {
    msg <- res$content()[cond]
    abort(error_msg(msg[[1]]), call = NULL)
  }
  lapply(res$parse(encoding = "UTF-8"), parse_subs_page_)
}

#' @noRd
fetch_subs_pages_ <- function(uid, start, limit, query = NULL, fields = NULL) {
  if (length(start) == 1L) {
    return(list(fetch_subs_page_(uid, start, limit, query, fields)))
  }
  fetch_subs_async_(uid, start, limit, query, fields)
}

#' Pages needed to cover the records a first round of requests missed
#'
#' @param start numeric, offset of each page already requested.
#' @param got numeric, number of records each page returned.
#' @param total numeric, number of records reported by the server.
#' @param chunk numeric, size of the pages to request.
#'
#' @returns A data.frame with columns `start` and `limit`, one row per page.
#' @noRd
plan_missing_pages_ <- function(start, got, total, chunk) {
  o <- order(start)
  from <- start[o] + got[o]
  to <- pmin(c(start[o][-1], total), total)
  gap <- from < to
  if (!any(gap) || chunk < 1) {
    return(data.frame(start = numeric(), limit = numeric()))
  }
  from <- from[gap]
  to <- to[gap]
  s <- unlist(Map(seq, from, to - 1, MoreArgs = list(by = chunk)))
  end <- rep(to, times = ceiling((to - from) / chunk))
  data.frame(start = s, limit = pmin(chunk, end - s))
}

#' Download submissions page by page, adapting to the server page limit
#'
#' Servers cap the number of records per request (`MAX_API_PAGE_SIZE`) and
#' return fewer records than asked without an error. Pages that come back
#' short reveal that limit. The missing records are then requested with
#' pages of that size, and the limit is kept for the rest of the session.
#'
#' @importFrom data.table rbindlist
#' @importFrom tibble as_tibble tibble
#' @importFrom cli cli_inform
#' @noRd
get_subs_pages_ <- function(
  uid,
  size = NULL,
  page_size = api_max_limit_(),
  query = NULL,
  fields = NULL
) {
  page_size <- min(page_size, page_cap_get_())
  if (is.null(size) || size <= page_size) {
    start <- 0
    limit <- page_size
  } else {
    start <- seq(0, size - 1, by = page_size)
    limit <- pmin(page_size, size - start)
  }
  pages <- fetch_subs_pages_(uid, start, limit, query, fields)

  got <- vapply(pages, function(p) NROW(p$results), numeric(1))
  count <- unlist(lapply(pages, `[[`, "count"))
  total <- max(c(count, sum(got)))
  short <- got < pmin(limit, total - start)
  cap <- max(got)
  chunk <- page_size
  if (any(short) && cap > 0 && cap < max(limit[short])) {
    chunk <- cap
    page_cap_set_(cap)
    cli_inform(c(
      i = "The server returns at most {cap} submissions per request.",
      " " = "robotoolbox uses pages of {cap} for the rest of the session."
    ))
  }

  missing <- plan_missing_pages_(start, got, total, chunk)
  if (nrow(missing) > 0) {
    more <- fetch_subs_pages_(uid, missing$start, missing$limit, query, fields)
    pages <- c(pages, more)
    start <- c(start, missing$start)
  }

  results <- lapply(pages[order(start)], `[[`, "results")
  results <- Filter(function(x) NROW(x) > 0, results)
  if (length(results) == 0L) {
    return(tibble())
  }
  if (length(results) == 1L) {
    return(as_tibble(results[[1]]))
  }
  dt2tibble(rbindlist(results, fill = TRUE))
}

#' @noRd
get_subs_fields_ <- function(uid, fields) {
  res <- get_subs_pages_(uid, fields = fields)
  if (nrow(res) == 0L) {
    return(NULL)
  }
  res
}

#' Fetch every page of a listing endpoint
#'
#' Follows the pages with `start`/`limit` until the server reports no `next`
#' page or `n_max` records have been collected.
#'
#' @returns A list with the `results` element of each page, and the `count`
#'   reported by the server (if any) as attribute.
#' @importFrom RcppSimdJson fparse
#' @noRd
xget_all_ <- function(
  path,
  args = list(),
  n_max = Inf,
  simplify = "list",
  page_size = 1000L
) {
  pages <- list()
  n <- 0
  count <- NULL
  repeat {
    args$limit <- sprintf("%d", min(page_size, n_max - n))
    if (n > 0) {
      args$start <- sprintf("%d", n)
    }
    res <- fparse(xget(path = path, args = args), max_simplify_lvl = simplify)
    count <- count %||% res$count
    got <- NROW(res$results)
    if (got == 0) {
      break
    }
    pages[[length(pages) + 1L]] <- res$results
    n <- n + got
    if (is.null(res$`next`) || n >= n_max) {
      break
    }
  }
  attr(pages, "count") <- count
  pages
}
