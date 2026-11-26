#' @noRd
assert_url <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    return(FALSE)
  }
  host <- paste0(
    "(localhost|[0-9]{1,3}([.][0-9]{1,3}){3}",
    "|([a-z0-9]([a-z0-9-]*[a-z0-9])?[.])+[a-z]{2,63})"
  )
  path <- "(/[a-z0-9._~%!$&'()*+,;=:@-]*)*"
  pattern <- paste0("^(https?://)?", host, "(:[0-9]{1,5})?", path, "$")
  if (!grepl(pattern, x, ignore.case = TRUE)) {
    return(FALSE)
  }
  host_port <- sub("^(https?://)?([^/]*).*$", "\\2", x, ignore.case = TRUE)
  !grepl(":", host_port, fixed = TRUE) ||
    as.integer(sub("^.*:", "", host_port)) <= 65535L
}

#' @noRd
assert_uid <- function(x) {
  uid_pattern <- "^a[A-Za-z0-9]{21}$"
  grepl(pattern = uid_pattern, x)
}

#' @noRd
assert_version_uid <- function(x) {
  vuid_pattern <- "^v[A-Za-z0-9]{21}$"
  grepl(pattern = vuid_pattern, x)
}

#' @noRd
assert_token <- function(x) {
  token_pattern <- "^[0-9a-f]{40}$"
  grepl(pattern = token_pattern, x)
}

#' Resolve an asset argument to a kobo_asset object
#' @importFrom rlang abort
#' @noRd
as_kobo_asset_ <- function(x) {
  if (inherits(x, "kobo_asset")) {
    return(x)
  }
  if (is.character(x) && length(x) == 1 && assert_uid(x)) {
    return(kobo_asset(x))
  }
  abort(
    "'asset' must be a 'kobo_asset' object or a valid asset uid",
    call = NULL
  )
}

#' Maximum number of records per request for the /data endpoint
#' @noRd
api_max_limit_ <- function() {
  val <- trimws(Sys.getenv("KOBOTOOLBOX_PAGE_SIZE"))
  ## A variable set to an empty string counts as not set
  if (!nzchar(val)) {
    return(1000L)
  }
  size <- suppressWarnings(as.integer(val))
  if (is.na(size) || size < 1L) {
    abort(
      c(
        "The `KOBOTOOLBOX_PAGE_SIZE` environment variable must be a positive integer.",
        x = paste0("Its value is \"", val, "\".")
      ),
      call = NULL
    )
  }
  size
}

#' @importFrom RcppSimdJson is_valid_json
#' @noRd
validate_query_ <- function(query) {
  if (is.null(query)) {
    return(invisible(NULL))
  }
  if (!is.character(query) || length(query) != 1 || is.na(query)) {
    abort("`query` must be a single character string", call = NULL)
  }
  is_object <- grepl("^[[:space:]]*[{]", query) && is_valid_json(query)
  if (!is_object) {
    abort(
      c(
        "`query` must be a valid JSON object string",
        i = 'Example: \'{"field_name": "value"}\''
      ),
      call = NULL
    )
  }
  invisible(NULL)
}

#' @importFrom rlang warn
#' @noRd
validate_fields_ <- function(fields) {
  if (is.null(fields)) {
    return(invisible(NULL))
  }
  if (!is.character(fields) || length(fields) == 0) {
    abort("`fields` must be a non-empty character vector", call = NULL)
  }
  if (anyNA(fields) || !all(nzchar(fields))) {
    abort("`fields` must not hold missing or empty names", call = NULL)
  }
  invisible(NULL)
}

#' Names of the fields for the server
#'
#' The server names a question inside a group by its path, such as
#' `household/age`, and returns nothing for the bare name. Each field is sent
#' under its name and under the path it has in the deployed form. A path is
#' accepted for a question outside repeat groups, and a field inside a repeat
#' group cannot be selected on its own.
#' @returns A list with `names`, the question names of the fields, and `api`,
#'   the names to send to the server.
#' @noRd
fields_api_ <- function(fields, asset) {
  if (is.null(fields)) {
    return(list(names = NULL, api = NULL))
  }
  form <- kobo_form(asset)
  known <- data.frame(name = character(), xpath = character())
  if (all(c("name", "xpath") %in% names(form))) {
    in_main <- repeat_scope_(form) == "main"
    known <- form[!is.na(form$name) & !is.na(form$xpath) & in_main, c("name", "xpath")]
  }
  is_path <- grepl("/", fields, fixed = TRUE)
  bad <- fields[is_path & !fields %in% known$xpath]
  if (length(bad) > 0L) {
    abort(
      c(
        paste0(
          "Nested field selection is not supported: ",
          paste0("`", bad, "`", collapse = ", ")
        ),
        i = "Use the repeat group name to fetch the entire group.",
        i = 'Example: fields = c("name", "my_repeat_group")'
      ),
      call = NULL
    )
  }
  nm <- fields
  nm[is_path] <- known$name[match(fields[is_path], known$xpath)]
  list(
    names = unique(nm),
    api = unique(c(fields, known$xpath[known$name %in% nm]))
  )
}

#' @noRd
clean_urls <- function(x) {
  sub("/+$", "", x)
}

#' URL of the server, without trailing slash
#' @noRd
server_url_ <- function() {
  clean_urls(Sys.getenv("KOBOTOOLBOX_URL"))
}

#' Full URL of an API path
#'
#' `file.path()` is not used because it drops a trailing slash on Windows.
#' @noRd
api_url_ <- function(path) {
  paste0(server_url_(), "/", path)
}

#' HTTP status codes worth a retry
#'
#' 429 is rate limiting. A single request waits for the number of seconds
#' given in `Retry-After`. Requests sent in batches wait a short random time.
#' @noRd
retry_codes_ <- c(429L, 500L, 502L, 503L, 504L)

#' @noRd
#' @importFrom utils packageVersion
user_agent_ <- function() {
  robotoolbox_version <- packageVersion("robotoolbox")
  os <- Sys.info()[["sysname"]]
  os_version <- paste(Sys.info()[["release"]], Sys.info()[["version"]])
  r_version <- paste0(
    R.version$major,
    ".",
    R.version$minor,
    ifelse(R.version$status == "", "", paste0("-", R.version$status))
  )
  header <- paste0(
    "robotoolbox/",
    robotoolbox_version,
    " (",
    os,
    "/",
    os_version,
    "; ",
    "R/",
    r_version,
    ")"
  )
  header
}

#' @noRd
#' @importFrom RcppSimdJson fparse
error_msg <- function(x) {
  check_uid_info <- "Check your uid and/or credentials and try again"
  check_token_info <- "Check again your token in `kobo_setup` is correct or use `kobo_token`"
  check_cred_info <- "Check again your username or password"
  check_default <- "Check that you have the right server and if it's working"
  msg <- tryCatch(
    fparse(rawToChar(x))$detail,
    error = function(e) NULL
  )
  if (!is.character(msg) || length(msg) != 1L) {
    return(setNames(check_uid_info, "i"))
  }
  if (grepl("not found", msg, ignore.case = TRUE)) {
    c("uid (project) not found in this account/server", i = check_uid_info)
  } else if (grepl("invalid token", msg, ignore.case = TRUE)) {
    c("Invalid API token", i = check_token_info)
  } else if (grepl("invalid username/password", msg, ignore.case = TRUE)) {
    c("Invalid username or password", i = check_cred_info)
  } else {
    c(msg, i = check_default)
  }
}

#' Abort with the message of a failed request
#'
#' The condition has the class `robotoolbox_http_<status>`, so callers can
#' handle one status, such as a 404 from the data endpoint.
#' @importFrom rlang abort
#' @noRd
abort_http_ <- function(content, status) {
  abort(
    error_msg(content),
    class = paste0("robotoolbox_http_", status),
    call = NULL
  )
}

#' Send a batch of requests at the same time
#'
#' The only function that sends async requests, so tests replace it.
#' `crul::AsyncVaried` runs the retry options of each request.
#' @returns A list with the `status` and the `content` of each response, in
#'   the order of `reqs`.
#' @importFrom crul AsyncVaried
#' @noRd
async_send_ <- function(reqs) {
  batch <- AsyncVaried$new(.list = reqs)
  batch$request()
  list(status = batch$status_code(), content = batch$content())
}

#' Send requests in batches and handle each response as its batch arrives
#'
#' The download stops at the first batch with a failed request, with the
#' class of its HTTP status, and the later batches are not sent.
#' @param reqs list of `crul::HttpRequest`.
#' @param handle function of the content of a response and the index of its
#'   request, called once per response. The raw contents of a batch are
#'   released once handled.
#' @param tick function called with the handled responses of each batch, or
#'   `NULL`.
#' @returns The list of handled responses, in the order of `reqs`.
#' @noRd
async_fetch_batches_ <- function(
  reqs,
  handle = function(content, i) content,
  bucket_size = 15L,
  sleep = 0.05,
  tick = NULL
) {
  n <- length(reqs)
  if (n == 0L) {
    return(list())
  }
  out <- vector("list", n)
  starts <- seq.int(1L, n, by = bucket_size)
  for (k in seq_along(starts)) {
    idx <- seq.int(starts[k], min(starts[k] + bucket_size - 1L, n))
    res <- async_send_(reqs[idx])
    bad <- which(res$status >= 300L | res$status < 200L)
    if (length(bad) > 0L) {
      abort_http_(res$content[[bad[1L]]], res$status[bad[1L]])
    }
    batch <- lapply(seq_along(idx), function(j) handle(res$content[[j]], idx[j]))
    out[idx] <- batch
    if (is.function(tick)) {
      tick(batch)
    }
    if (k < length(starts) && sleep > 0) {
      Sys.sleep(sleep)
    }
  }
  out
}

#' Progress bar counting the handled responses
#'
#' The bar belongs to the calling function and ends with it. It shows in an
#' interactive terminal only, so the stage messages of
#' `cli::cli_progress_step()` stay the output of scripts and reports.
#' @param size function giving the number of units of a batch.
#' @returns A function to pass as `tick`, which advances the bar. Its `done`
#'   attribute is a function that ends the bar.
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#' @noRd
progress_ticker_ <- function(name, total = NA, size = length, .envir = parent.frame()) {
  if (is.null(total) || is.na(total) || total <= 0) {
    total <- NA
  }
  id <- cli_progress_bar(name, total = total, current = FALSE,
                         auto_terminate = FALSE, .envir = .envir)
  done <- 0
  tick <- function(batch) {
    done <<- done + size(batch)
    ## Past a stale total, as an outdated submission count, the bar counts on
    if (!is.na(total) && done > total) {
      total <<- NA
      cli_progress_update(set = done, total = NA, id = id, force = TRUE, .envir = .envir)
    } else {
      cli_progress_update(set = done, id = id, force = TRUE, .envir = .envir)
    }
  }
  attr(tick, "done") <- function() {
    try(cli_progress_done(id = id, .envir = .envir), silent = TRUE)
    invisible(NULL)
  }
  tick
}

#' End the bar of a `progress_ticker_()`, if any
#' @noRd
progress_done_ <- function(tick) {
  if (is.function(tick)) {
    attr(tick, "done")()
  }
  invisible(NULL)
}

#' Number of submissions in pages handled by `parse_subs_page_()`
#' @noRd
n_subs_ <- function(pages) {
  sum(vapply(pages, function(p) NROW(p$results), numeric(1)))
}

#' Abort when no server is configured
#' @noRd
check_setup_ <- function() {
  if (!nzchar(Sys.getenv("KOBOTOOLBOX_URL"))) {
    abort(c(
      "KoboToolbox server URL is not configured.",
      i = paste(
        "Run `kobo_setup(url = , token = )` or set the",
        "`KOBOTOOLBOX_URL` environment variable."
      )
    ), call = NULL)
  }
}

#' GET a JSON endpoint of the server
#'
#' @param raw logical, return the body as raw bytes instead of a string.
#'   `RcppSimdJson::fparse()` reads raw bytes directly, which skips building
#'   a large string.
#' @importFrom rlang abort
#' @noRd
xget <- function(path, args = list(), n_retry = 3L, raw = FALSE, ...) {
  check_setup_()
  headers <- list(
    Authorization = paste("Token", Sys.getenv("KOBOTOOLBOX_TOKEN")),
    `User-Agent` = user_agent_()
  )
  cli <- HttpClient$new(
    server_url_(),
    headers = headers,
    opts = list(...)
  )
  res <- cli$retry(
    "get",
    path = path,
    query = args,
    times = n_retry,
    retry_only_on = retry_codes_,
    terminate_on = 404
  )
  if (res$status_code >= 300) {
    abort_http_(res$content, res$status_code)
  }
  res$raise_for_ct_json()
  if (isTRUE(raw)) {
    return(res$content)
  }
  res$parse("UTF-8")
}

#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr transmute
#' @importFrom cli cli_warn
#' @noRd
get_asset_list_ <- function(limit = 100L) {
  pages <- xget_all_(
    "api/v2/assets/",
    args = list(ordering = asset_ordering_),
    n_max = limit,
    simplify = "data_frame"
  )
  if (length(pages) == 0L) {
    return(tibble(
      uid = character(),
      name = character(),
      asset_type = character(),
      owner_username = character(),
      date_created = character(),
      date_modified = character(),
      deployed = logical(),
      submissions = integer(),
      created_by = character()
    ))
  }
  res <- distinct_assets_(pages)
  expected <- min(parse_api_count_(attr(pages, "count")), limit)
  if (!is.na(expected) && nrow(res) < expected) {
    cli_warn(c(
      "The server listed {nrow(res)} of {expected} assets.",
      i = "Pages overlapped because several assets sort identically."
    ))
  }
  if (!"created_by" %in% names(res)) {
    res$created_by <- NA_character_
  }
  transmute(
    res,
    .data$uid,
    .data$name,
    .data$asset_type,
    owner_username = .data$owner__username,
    date_created = parse_kobo_datetime(.data$date_created),
    date_modified = parse_kobo_datetime(.data$date_modified),
    deployed = .data$deployment__active,
    submissions = .data$deployment__submission_count,
    created_by = as.character(.data$created_by)
  )
}

#' @importFrom tibble tibble
#' @importFrom cli cli_inform
#' @importFrom rlang try_fetch
#' @noRd
get_asset_minimal_list_ <- function(limit = 100L) {
  args <- list(ordering = asset_ordering_)
  pages <- try_fetch(
    xget_all_(
      "api/v2/assets/minimal-list/",
      args = args,
      n_max = limit,
      simplify = "data_frame"
    ),
    robotoolbox_http_404 = function(e) {
      cli_inform(c(
        i = "The minimal asset listing needs KoboToolbox KPI 2.026.21 or later.",
        " " = "Using the full asset listing instead."
      ))
      xget_all_("api/v2/assets/", args = args, n_max = limit, simplify = "data_frame")
    }
  )
  if (length(pages) == 0L) {
    return(tibble(
      uid = character(),
      name = character(),
      deployment_status = character()
    ))
  }
  distinct_assets_(pages)[c("uid", "name", "deployment_status")]
}

#' Sort order for asset listings
#'
#' The default order (deployment status, then `-date_modified`) has ties, for
#' example after a server migration touches many assets at once, and tied rows
#' can move between pages. `uid` is not an allowed ordering field, so further
#' fields break the ties.
#' @noRd
asset_ordering_ <- "-date_modified,name,owner__username,asset_type,date_deployed"

#' @importFrom purrr list_rbind
#' @importFrom tibble as_tibble
#' @noRd
distinct_assets_ <- function(pages) {
  ## A page can give the submission counts as strings, which would not bind
  pages <- lapply(pages, function(p) {
    n <- p[["deployment__submission_count"]]
    if (!is.null(n)) {
      p[["deployment__submission_count"]] <-
        as.integer(vapply(n, parse_api_count_, numeric(1), USE.NAMES = FALSE))
    }
    p
  })
  res <- as_tibble(list_rbind(pages))
  res[!duplicated(res$uid), ]
}

#' @importFrom tibble as_tibble
#' @noRd
dt2tibble <- function(x) {
  x <- as_tibble(x)
  attr(x, ".internal.selfref") <- NULL
  x
}

#' @noRd
format_fields_ <- function(fields) {
  if (is.null(fields)) {
    return(NULL)
  }
  if (!"__version__" %in% fields) {
    fields <- c(fields, "__version__")
  }
  paste0('["', paste(fields, collapse = '","'), '"]')
}

#' Path of an asset, or of one of its versions
#'
#' The path ends with a slash. Without it the server answers with a redirect,
#' which doubles the requests.
#' @noRd
asset_path_ <- function(uid, version = NULL) {
  if (is.null(version)) {
    return(paste0("api/v2/assets/", uid, "/"))
  }
  paste0("api/v2/assets/", uid, "/versions/", version, "/")
}

#' @noRd
get_filtered_count_ <- function(uid, query) {
  path <- paste0("api/v2/assets/", uid, "/data.json")
  args <- list(limit = 1, query = query)
  res <- xget(path = path, args = args, raw = TRUE)
  parsed <- fparse(res, max_simplify_lvl = "list")
  if (is.null(parsed$count)) {
    return(0L)
  }
  as.integer(parse_api_count_(parsed$count))
}

#' @noRd
get_audit_url_ <- function(uid) {
  empty <- tibble(`_id` = integer(), download_url = character())
  res <- get_subs_fields_(uid, '["_attachments", "formhub/uuid", "_uuid"]')
  if (is.null(res)) {
    return(empty)
  }
  res <- unnest(res[c("_id", "_attachments")], "_attachments")
  if (!"media_file_basename" %in% names(res)) {
    return(empty)
  }
  res |>
    filter(.data$media_file_basename %in% "audit.csv") |>
    select("_id", "download_url")
}

#' @importFrom bit64 integer64
#' @noRd
empty_audit_ <- function() {
  int64 <- integer64()
  time <- as.POSIXct(numeric(), origin = "1970-01-01")
  tibble(`_id` = integer(), event = character(), node = character(),
         name = character(), start_int = int64, end_int = int64,
         start = time, end = time, latitude = numeric(),
         longitude = numeric(), accuracy = numeric(),
         `old-value` = character(), `new-value` = character(),
         user = character(), `change-reason` = character())
}

#' @importFrom RcppSimdJson fparse
#' @noRd
get_attachment_url_ <- function(uid) {
  res <- get_subs_fields_(uid, '["_attachments", "formhub/uuid", "_uuid"]')
  if (is.null(res)) {
    return(tibble(`_id` = integer()))
  }
  unnest(res[c("_id", "_attachments")], "_attachments")
}


#' @noRd
map_chr2 <- function(x, key) {
  vapply(x, function(l) {
    v <- l[[key]]
    if (length(v) == 0L) NA_character_ else as.character(v[[1L]])
  }, character(1))
}

#' Whether an asset version is deployed
#'
#' `date_deployed` is a date for a deployed version and `false` otherwise.
#' A missing value is not a deployment.
#' @noRd
is_deployed_version_ <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L) {
    return(FALSE)
  }
  !is.na(x) & nzchar(x) & toupper(x) != "FALSE"
}

#' @noRd
parse_kobo_datetime <- function(x) {
  x <- sub("Z$", "+0000", as.character(x))
  x <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x)
  has_tz <- grepl("T.*[+-][0-9]{4}$", x)
  out <- .POSIXct(rep(NA_real_, length(x)), tz = "GMT")
  out[has_tz] <- as.POSIXct(x[has_tz], format = "%Y-%m-%dT%H:%M:%OS%z", tz = "GMT")
  out[!has_tz] <- as.POSIXct(x[!has_tz], format = "%Y-%m-%dT%H:%M:%OS", tz = "GMT")
  as.character(round(out, "secs"))
}

#' @noRd
is_list_cols <- function(x) {
  which(vapply(x, is.list, logical(1)))
}

#' @noRd
is_list_cols_names <- function(x) {
  names(is_list_cols(x))
}

#' @noRd
drop_nulls <- function(x) {
  x <- lapply(x, function(node) {
    if (is.list(node)) {
      drop_nulls(node)
    } else {
      node
    }
  })
  x[vapply(x, is.null, logical(1))] <- NULL
  x
}

#' Transliterate to ASCII
#'
#' Same as `stri_trans_general(x, "latin-ascii")`, which leaves ASCII strings
#' unchanged, so only the other strings are transliterated.
#' @importFrom stringi stri_trans_general stri_enc_isascii
#' @noRd
latin_ascii_ <- function(x) {
  if (!is.character(x)) {
    return(stri_trans_general(x, "latin-ascii"))
  }
  idx <- which(!stri_enc_isascii(x))
  if (length(idx) > 0) {
    x[idx] <- stri_trans_general(x[idx], "latin-ascii")
  }
  x
}

#' @noRd
null2char <- function(x) {
  if (is.null(x)) {
    return("")
  }
  x
}

#' @importFrom stringi stri_sort
#' @importFrom stats na.omit
#' @noRd
col2choices <- function(x, form, col, question = col) {
  idx <- which(form$name %in% question & grepl("select_multiple", form$type, fixed = TRUE))
  uv <- unique(unlist(lapply(form_choices_(form)[idx], `[[`, "value_name")))
  ## Without choices in the form, as with choices from a file, the codes
  ## found in the answers are used
  if (length(uv) == 0L) {
    uv <- unique(unlist(strsplit(as.character(x[[col]]), "\\s+")))
    uv <- trimws(na.omit(uv))
    uv <- uv[nzchar(uv)]
  }
  stri_sort(uv, na_last = TRUE, locale = "en_US", numeric = TRUE)
}

#' Choices of each row of a form
#'
#' A form has no `choices` column when it has no choice list, for example
#' when its only select questions read their choices from a file.
#' @noRd
form_choices_ <- function(form) {
  form[["choices"]] %||% vector("list", nrow(form))
}

#' @noRd
hide_token <- function(x) {
  if (!nzchar(x)) {
    return("<unset>")
  }
  a <- paste(rep("x", 35), collapse = "")
  b <- substr(x, 36, nchar(x))
  paste(c(a, b), collapse = "")
}

#' Dummy columns of select_multiple questions
#'
#' A column that already has the name of a dummy column, such as a text
#' question `pet_other` next to the choice `other` of `pet`, is renamed with a
#' numeric suffix. The renames are returned in the `kobo_displaced` attribute,
#' as question names named by the new column names.
#' @importFrom data.table chmatch
#' @importFrom rlang names_inform_repair
#' @noRd
fast_dummy_cols <- function(x, form, cols, sep, sm_choices = NULL) {
  ## Columns are set on a plain list, much faster than on a tibble
  attrs <- attributes(x)
  x <- unclass(x)
  displaced <- character()
  for (col in cols) {
    ## The answers of a select_multiple question renamed by the dummy column
    ## of another one are under its new name
    src <- if (col %in% displaced) names(displaced)[match(col, displaced)] else col
    y <- as.character(x[[src]])
    uv <- sm_choices[[col]] %||% col2choices(x[src], form, src, question = col)
    new_names <- paste0(col, sep, uv)
    nm <- names(x)
    if (any(new_names %in% nm)) {
      old_cols <- intersect(new_names, nm)
      taken <- union(nm, new_names)
      new_cols <- make.unique(c(taken, old_cols), sep = "_")[-seq_along(taken)]
      names_inform_repair(old_cols, new_cols)
      for (i in seq_along(old_cols)) {
        x[[new_cols[i]]] <- x[[old_cols[i]]]
      }
      x[old_cols] <- NULL
      questions <- ifelse(old_cols %in% names(displaced), displaced[old_cols], old_cols)
      displaced <- c(displaced[!names(displaced) %in% old_cols],
                     setNames(questions, new_cols))
    }
    na_idx <- is.na(y)
    init <- ifelse(na_idx, NA_integer_, 0L)
    for (nm in new_names) {
      x[[nm]] <- init
    }
    non_na_idx <- which(!na_idx)
    if (length(non_na_idx) > 0) {
      splits <- strsplit(y[non_na_idx], " ", fixed = TRUE)
      rids <- rep.int(non_na_idx, lengths(splits))
      vals <- unlist(splits, use.names = FALSE)
      midx <- chmatch(vals, uv)
      valid <- !is.na(midx)
      if (any(valid)) {
        rids_by_col <- split(rids[valid], midx[valid])
        for (k in names(rids_by_col)) {
          x[[new_names[as.integer(k)]]][rids_by_col[[k]]] <- 1L
        }
      }
    }
  }
  attrs$names <- names(x)
  attrs$kobo_displaced <- if (length(displaced) > 0) displaced
  attributes(x) <- attrs
  x
}

#' Question name of each column
#'
#' A column derived from a question present in `x`, such as a select_multiple
#' dummy column or the WKT of a geotrace, is no question and gets `NA`, even
#' when a question has the same name. A column renamed
#' because a dummy column took its name keeps its question name in the
#' `kobo_question` attribute.
#' @param sm_parent question names of the derived columns, named by column
#'   name. They are the select_multiple dummy columns of `sm_parents_()` and
#'   the spatial columns of `geo_parents_()`.
#' @noRd
col_questions_ <- function(x, sm_parent = character()) {
  nm <- names(x)
  q <- vapply(seq_along(nm), function(i) {
    attr(x[[i]], "kobo_question", exact = TRUE) %||% nm[i]
  }, character(1))
  parent <- unname(sm_parent[nm])
  q[!is.na(parent) & parent %in% nm] <- NA_character_
  q
}

#' Record the question name of the renamed columns
#' @noRd
set_displaced_ <- function(x, displaced) {
  for (n in intersect(names(displaced), names(x))) {
    attr(x[[n]], "kobo_question") <- displaced[[n]]
  }
  x
}

#' Question of each select_multiple dummy column, named by column name
#' @noRd
sm_parents_ <- function(form, sep = "_") {
  if (!"choices" %in% names(form)) {
    return(setNames(character(), character()))
  }
  i <- which(grepl("select_multiple", form$type, fixed = TRUE) & !is.na(form$name))
  codes <- lapply(form$choices[i], function(ch) unique(ch$value_name))
  parent <- rep.int(form$name[i], lengths(codes))
  if (length(parent) == 0L) {
    return(setNames(character(), character()))
  }
  res <- setNames(parent, paste0(parent, sep, unlist(codes, use.names = FALSE)))
  res[!duplicated(names(res))]
}

#' Column order when a question was renamed by a dummy column
#'
#' The renamed column takes the position of its question, and the dummy
#' column stays with the other choices of its select_multiple question.
#' @noRd
displaced_order_ <- function(cn, displaced, form, sep) {
  ex <- kobo_form_extra_(form, sep)
  is_q <- ex$value_name == "" & ex$name %in% displaced
  ord <- ex$cnames
  ord[is_q] <- names(displaced)[match(ex$name[is_q], displaced)]
  ord <- unique(ord)
  ord <- ord[ord %in% c(cn, names(displaced))]
  c(ord, setdiff(cn, ord))
}

#' Choice codes of each select_multiple question, sorted
#'
#' Same codes as `col2choices()` for a question of the form, computed once
#' for all questions.
#' @importFrom stringi stri_sort
#' @noRd
form_sm_choices_ <- function(form) {
  is_sm <- grepl("select_multiple", form$type, fixed = TRUE)
  idx <- split(which(is_sm), factor(form$name[is_sm], levels = unique(form$name[is_sm])))
  choices <- form_choices_(form)
  res <- lapply(idx, function(i) {
    uv <- unique(unlist(lapply(choices[i], `[[`, "value_name")))
    stri_sort(uv, na_last = TRUE, locale = "en_US", numeric = TRUE)
  })
  ## A question without codes is left to `col2choices()`, which reads the data
  res[lengths(res) > 0L]
}

#' @importFrom stringi stri_detect_regex
#' @noRd
dummy_from_form_ <- function(x, form, sep, sm_choices = NULL) {
  bool <- stri_detect_regex(form$type, "select_multiple")
  nm <- unique(form$name[bool])
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    x <- fast_dummy_cols(x, form, nm, sep, sm_choices)
  } else {
    x
  }
  x
}

#' Merge the columns of a question whose group changed during data collection
#'
#' A question moved to another group has one column per group path, and each
#' submission fills one of them. Columns with the same question name are
#' merged when no submission has a value in two of them. Otherwise they are
#' questions of the same name in different groups, and they are kept apart.
#' A merged column keeps the name and the place of its first column.
#' @importFrom dplyr coalesce
#' @importFrom cli cli_inform
#' @noRd
dedup_vars_ <- function(x) {
  nm <- names(x)
  base <- basename(nm)
  for (v in unique(base[duplicated(base)])) {
    groups <- list()
    filled <- list()
    for (col in nm[base == v]) {
      has <- has_value_(x[[col]])
      free <- which(!vapply(filled, function(f) any(f & has), logical(1)))
      if (length(free) == 0L) {
        groups[[length(groups) + 1L]] <- col
        filled[[length(filled) + 1L]] <- has
      } else {
        groups[[free[1L]]] <- c(groups[[free[1L]]], col)
        filled[[free[1L]]] <- filled[[free[1L]]] | has
      }
    }
    if (length(groups) > 1L) {
      paths <- vapply(groups, `[`, character(1), 1L)
      cli_inform(c(
        i = "Several questions are named {.field {v}}.",
        " " = "{.field {paths}} are kept in separate columns."
      ))
    }
    for (k in seq_along(groups)) {
      old_vars <- groups[[k]]
      if (length(old_vars) < 2L) {
        next
      }
      ## The merged column stays at the place of the first one, so the
      ## columns kept apart are numbered in the order of the data
      x[[old_vars[1L]]] <- coalesce(!!!unname(as.list(x[rev(old_vars)])))
      x[old_vars[-1L]] <- NULL
    }
  }
  x
}

#' Rows of a column that hold a value
#' @noRd
has_value_ <- function(x) {
  if (is.list(x)) lengths(x) > 0L else !is.na(x)
}

#' @noRd
make_unique_names_ <- function(x) {
  make.unique(basename(x), sep = "_")
}

#' Repeat group enclosing each row of the form
#'
#' The scope is computed within each form version. A `begin_repeat` row gets
#' the scope that encloses the repeat group, its parent.
#' @noRd
repeat_scope_ <- function(form) {
  ver <- form$version %||% rep(NA_character_, nrow(form))
  ver[is.na(ver)] <- ""
  begin <- repeat_begin_(form)
  scope <- rep("main", nrow(form))
  for (v in unique(ver)) {
    idx <- which(ver == v)
    type <- form$type[idx]
    name <- form$name[idx]
    is_begin <- begin[idx]
    ev <- which(is_begin | type == "end_repeat")
    after <- character(length(ev))
    stack <- "main"
    for (k in seq_along(ev)) {
      if (is_begin[ev[k]]) {
        stack <- c(stack, name[ev[k]])
      } else if (length(stack) > 1L) {
        stack <- stack[-length(stack)]
      }
      after[k] <- stack[length(stack)]
    }
    pos <- findInterval(seq_along(idx), ev)
    s <- c("main", after)[pos + 1L]
    b <- ev[is_begin[ev]]
    s[b] <- c("main", after)[pos[b]]
    scope[idx] <- s
  }
  scope
}

#' Rows of the form that open a repeat group
#'
#' Rows without a label, such as `end_repeat`, only exist in the first
#' language of each form version, so only the `begin_repeat` rows of that
#' language open a repeat group.
#' @noRd
repeat_begin_ <- function(form) {
  ver <- form$version %||% rep(NA_character_, nrow(form))
  ver[is.na(ver)] <- ""
  lang <- form$lang %||% rep("", nrow(form))
  first <- lang[match(ver, ver)]
  same_lang <- lang == first | (is.na(lang) & is.na(first))
  form$type %in% "begin_repeat" & !is.na(form$name) & same_lang %in% TRUE
}

#' Parent tables of each repeat group
#'
#' A repeat group can have a different parent in another form version. The
#' links follow the form order, so the deployed version gives the first
#' parent of each repeat group.
#' @noRd
repeat_links_ <- function(form) {
  b <- repeat_begin_(form)
  res <- data.frame(name = form$name[b], parent = repeat_scope_(form)[b])
  res[!duplicated(res), , drop = FALSE]
}

#' Rows of a repeat group from each of its parent tables
#'
#' `x` holds the list column of the repeat group in each parent table, named
#' by parent. `_parent_index` points to the first parent of `parents`, and
#' `_parent_index_<parent>` to each other one.
#' @importFrom tibble tibble rowid_to_column add_column
#' @importFrom rlang :=
#' @importFrom data.table rbindlist
#' @noRd
repeat_tbl_ <- function(x, parents = names(x)) {
  parts <- lapply(x, function(rows) {
    if (is.null(rows)) {
      rows <- list()
    }
    rows[!vapply(rows, is.data.frame, logical(1))] <- list(tibble())
    rbindlist(rows, idcol = "_parent_index", fill = TRUE)
  })
  from <- rep(names(x), vapply(parts, nrow, integer(1)))
  res <- rbindlist(parts, fill = TRUE)
  if (nrow(res) == 0L) {
    res <- tibble(`_parent_index` = integer())
  } else {
    res <- dt2tibble(res)
    res <- dedup_vars_(res)
    res <- set_names(res, make_unique_names_)
  }
  idx <- res$`_parent_index`
  for (p in rev(parents[-1])) {
    res <- add_column(res, !!paste0("_parent_index_", p) := replace(idx, from != p, NA),
                      .after = "_parent_index")
  }
  res$`_parent_index` <- replace(idx, from != parents[1], NA)
  res <- rowid_to_column(res, "_index")
  res$`_parent_table_name` <- from
  res
}

#' Extract repeat tables at every depth
#'
#' Every repeat group of the form gives a table, with zero rows when the data
#' has no instance of it. Tables are listed in form order. Each one records
#' the parent table of each row in `_parent_table_name`. A repeat group whose
#' parent differs between form versions gets its rows from every parent. With
#' `fields`, only the top-level repeat groups it names are kept, together with
#' the repeat groups nested in them.
#' @noRd
kobo_extract_repeat_tbl <- function(x, form, fields = NULL) {
  links <- repeat_links_(form)
  todo <- unique(links$name)
  done <- "main"
  res <- list()
  while (length(todo) > 0) {
    ready <- vapply(todo, function(nm) all(links$parent[links$name == nm] %in% done),
                    logical(1))
    if (!any(ready)) {
      abort(
        c(
          "Can't extract the repeat groups of this form.",
          i = paste0("Repeat groups ", paste0("`", todo, "`", collapse = ", "),
                     " are nested in each other in different form versions.")
        ),
        call = NULL
      )
    }
    nm <- todo[ready][1]
    parents <- links$parent[links$name == nm]
    from_main <- parents == "main" & (is.null(fields) | nm %in% fields)
    found <- parents[from_main | parents %in% names(res)]
    if (length(found) > 0) {
      src <- lapply(found, function(p) if (p == "main") x[[nm]] else res[[p]][[nm]])
      res[[nm]] <- repeat_tbl_(setNames(src, found), parents)
    }
    done <- c(done, nm)
    todo <- todo[todo != nm]
  }
  res
}

#' Set the primary and foreign keys of repeat tables
#' @importFrom dm as_dm dm_add_pk dm_add_fk
#' @noRd
repeat_dm_ <- function(tbls, links) {
  res <- as_dm(tbls)
  for (nm in names(tbls)) {
    res <- dm_add_pk(res, !!nm, "_index")
    parents <- links$parent[links$name == nm]
    cols <- c("_parent_index", paste0("_parent_index_", parents[-1]))
    for (k in seq_along(parents)) {
      if (parents[k] %in% names(tbls)) {
        res <- dm_add_fk(res, !!nm, !!cols[k], !!parents[k])
      }
    }
  }
  res
}

#' @importFrom data.table rbindlist
#' @importFrom stats setNames
#' @noRd
precompute_val_labels_ <- function(form, lang) {
  form <- form[
    form$lang %in% lang & grepl("select_one", form$type, fixed = TRUE),
  ]
  nm <- unique(form$name)
  if (length(nm) == 0) {
    return(list())
  }
  rows <- split(seq_len(nrow(form)), factor(form$name, levels = nm))
  choices <- lapply(nm, function(n) {
    ch <- rbindlist(form_choices_(form)[rows[[n]]], fill = TRUE)
    if (nrow(ch) == 0) {
      return(setNames(character(), character()))
    }
    if ("value_version" %in% names(ch)) {
      ch$value_version <- NULL
    }
    ch <- ch[ch$value_lang %in% lang, ]
    ch <- ch[!duplicated(ch$value_name), ]
    ch$value_label <- make.unique(ch$value_label, sep = "_")
    setNames(ch$value_name, ch$value_label)
  })
  setNames(choices, nm)
}

#' @importFrom labelled set_value_labels
#' @noRd
val_labels_from_form_ <- function(x, form, lang, precomputed_choices = NULL,
                                  questions = names(x)) {
  if (is.null(precomputed_choices)) {
    precomputed_choices <- precompute_val_labels_(form, lang)
  }
  idx <- which(questions %in% names(precomputed_choices))
  if (length(idx) > 0) {
    nm <- names(x)[idx]
    choices <- setNames(precomputed_choices[questions[idx]], nm)
    kq <- lapply(nm, function(n) attr(x[[n]], "kobo_question", exact = TRUE))
    for (n in nm) {
      x[[n]] <- as.character(x[[n]])
    }
    x <- set_value_labels(x, .labels = choices, .strict = FALSE)
    for (i in which(lengths(kq) > 0)) {
      attr(x[[nm[i]]], "kobo_question") <- kq[[i]]
    }
  }
  x
}

#' @importFrom labelled var_label<-
#' @noRd
val_labels_sm_from_form_ <- function(x, form, lang, questions = names(x)) {
  is_sm <- grepl("select_multiple", form$type, fixed = TRUE)
  sm <- unique(form$name[form$lang %in% lang & is_sm])
  for (i in which(questions %in% sm)) {
    n <- names(x)[i]
    d <- x[[n]]
    vl <- var_label(d)
    kq <- attr(d, "kobo_question", exact = TRUE)
    lookup <- sm_lookup_(form, questions[i], lang)
    x[[n]] <- sm_codes_to_labels_(d, lookup)
    var_label(x[[n]]) <- vl
    attr(x[[n]], "kobo_question") <- kq
    attr(x[[n]], "kobo_sm_lang") <- lang
    attr(x[[n]], "kobo_sm_unlabeled") <- sm_unlabeled_(d, lookup)
  }
  x
}

#' Codes of select_multiple answers that have no label and are kept as is
#' @importFrom stringi stri_split_fixed
#' @noRd
sm_unlabeled_ <- function(x, lookup) {
  x <- as.character(x)
  codes <- unique(unlist(stri_split_fixed(unique(x[!is.na(x)]), " ", omit_empty = TRUE)))
  codes <- as.character(codes)
  sort(codes[is.na(lookup[codes])])
}

#' Labels of the choices of a select_multiple question, named by code
#'
#' Labels shared by several choices are made unique, so each label maps back
#' to one code. A choice without a translation has an empty label, which is
#' taken as no label.
#' @importFrom data.table rbindlist
#' @noRd
sm_lookup_ <- function(form, n, lang) {
  is_sm <- grepl("select_multiple", form$type, fixed = TRUE)
  ch <- rbindlist(form_choices_(form)[form$name %in% n & form$lang %in% lang & is_sm],
                  fill = TRUE)
  if (nrow(ch) == 0) {
    return(setNames(character(), character()))
  }
  if ("value_version" %in% names(ch)) {
    ch$value_version <- NULL
  }
  ch <- ch[ch$value_lang %in% lang, ]
  ch <- ch[!duplicated(ch$value_name), ]
  ch$value_label[ch$value_label %in% ""] <- NA
  has_label <- !is.na(ch$value_label)
  ch$value_label[has_label] <- make.unique(ch$value_label[has_label], sep = "_")
  setNames(ch$value_label, ch$value_name)
}

#' Replace each code of a select_multiple answer by its label
#'
#' Answers are split on spaces and each code is looked up exactly. A code
#' without a label is kept as it is.
#' @importFrom stringi stri_split_fixed
#' @noRd
sm_codes_to_labels_ <- function(x, lookup) {
  x <- as.character(x)
  u <- unique(x[!is.na(x)])
  codes <- stri_split_fixed(u, " ", omit_empty = TRUE)
  labels <- vapply(codes, function(code) {
    lab <- lookup[code]
    lab[is.na(lab)] <- code[is.na(lab)]
    paste(lab, collapse = " ")
  }, character(1))
  labels[match(x, u)]
}

#' Codes of select_multiple answers written with labels
#'
#' Each answer is split into labels of the question, or into single words for
#' codes that had no label. `unlabeled` lists the codes known to be kept as
#' is, and they count as their own labels. The split with the fewest other
#' words is kept, and an answer with two equally good splits is an error, for
#' example an unlabeled code spelled like the label of another choice.
#' @noRd
sm_labels_to_codes_ <- function(x, lookup, name, unlabeled = character()) {
  x <- as.character(x)
  lookup[is.na(lookup)] <- names(lookup)[is.na(lookup)]
  lookup <- c(lookup, setNames(unlabeled, unlabeled))
  lookup <- lookup[!duplicated(names(lookup))]
  u <- unique(x[!is.na(x)])
  codes <- vapply(u, function(s) {
    res <- sm_split_(s, lookup)
    if (is.na(res)) {
      abort(
        c(
          paste0("Can't translate the answers of select_multiple question `", name, "`."),
          i = paste0("The answer \"", s, "\" matches its labels and codes in more than one way."),
          i = "Download the data in this language with `kobo_data(lang = )` instead."
        ),
        call = NULL
      )
    }
    res
  }, character(1), USE.NAMES = FALSE)
  codes[match(x, u)]
}

#' Split one answer into codes, NA when the split is ambiguous
#' @noRd
sm_split_ <- function(s, lookup) {
  n <- nchar(s)
  if (n == 0) {
    return(s)
  }
  starts <- c(1L, which(strsplit(s, "", fixed = TRUE)[[1]] == " ") + 1L)
  starts <- starts[starts <= n]
  unknown <- integer(n + 2L)
  ways <- integer(n + 2L)
  path <- character(n + 2L)
  ways[n + 2L] <- 1L
  lens <- nchar(lookup)
  for (i in rev(starts)) {
    rest <- substr(s, i, n)
    word <- sub(" .*", "", rest)
    hit <- startsWith(rest, lookup)
    cand_next <- c(i + lens[hit] + 1L, i + nchar(word) + 1L)
    cand_code <- c(names(lookup)[hit], word)
    cand_unknown <- c(integer(sum(hit)), 1L)
    ok <- nzchar(c(lookup[hit], word)) & ways[cand_next] > 0
    if (!any(ok)) next
    cost <- cand_unknown[ok] + unknown[cand_next[ok]]
    best <- which(cost == min(cost))
    unknown[i] <- min(cost)
    ways[i] <- min(sum(ways[cand_next[ok][best]]), 2L)
    path[i] <- paste(c(cand_code[ok][best[1]], path[cand_next[ok][best[1]]]),
                     collapse = " ")
  }
  if (ways[1] != 1L) {
    return(NA_character_)
  }
  trimws(path[1], "right")
}

#' @noRd
replace_na_list_efficient_ <- function(x) {
  is_na <- is.na(x)
  x[is_na] <- names(x)[is_na]
  x
}

#' @importFrom tidyr unnest
#' @importFrom dplyr transmute distinct
#' @noRd
select_multiple_var_label <- function(x, form, lang, sep = "_") {
  labels <- list()
  bool <- stri_detect_regex(form$type, "select_multiple")
  if (any(bool) && "choices" %in% names(form)) {
    choices <- filter(
      form,
      .data$lang %in% !!lang,
      stri_detect_regex(.data$type, "select_multiple")
    ) |>
      unnest("choices") |>
      filter(.data$value_lang %in% !!lang) |>
      select(-any_of("value_version")) |>
      distinct() |>
      transmute(
        value_name = paste0(.data$name, sep, .data$value_name),
        value_label = paste0(.data$label, "::", .data$value_label)
      ) |>
      distinct()
    ## The deployed version comes first, so its label wins
    choices <- choices[!duplicated(choices$value_name), ]
    labels <- setNames(as.list(choices$value_label), choices$value_name)
  }
  labels
}

#' Separator of select_multiple dummy columns
#'
#' The separator is the most common string between the name of a
#' select_multiple question and one of its choice codes in `col_names`. It
#' falls back to `"_"` when no dummy column is found.
#' @noRd
sm_sep_ <- function(col_names, form) {
  sm <- form[grepl("select_multiple", form$type, fixed = TRUE), ]
  sm <- sm[!is.na(sm$name) & !duplicated(sm$name), ]
  seps <- character(0)
  for (i in seq_len(nrow(sm))) {
    n <- sm$name[i]
    rest <- col_names[startsWith(col_names, n)]
    rest <- substring(rest, nchar(n) + 1L)
    rest <- rest[nzchar(rest)]
    codes <- unique(form_choices_(sm)[[i]]$value_name)
    for (k in codes[!is.na(codes)]) {
      hit <- rest[endsWith(rest, k) & nchar(rest) > nchar(k)]
      seps <- c(seps, substring(hit, 1L, nchar(hit) - nchar(k)))
    }
  }
  if (length(seps) == 0L) {
    return("_")
  }
  counts <- table(seps)
  names(counts)[which.max(counts)]
}

#' @importFrom stats setNames
#' @noRd
precompute_var_labels_ <- function(form, lang, sep = "_") {
  form_lang <- form[form$lang %in% lang, ]
  form_labels <- setNames(as.list(form_lang$label), form_lang$name)
  sm_labels <- select_multiple_var_label(
    x = NULL,
    form = form_lang,
    lang = lang,
    sep = sep
  )
  list(
    form_labels = form_labels,
    sm_labels = sm_labels,
    sm_parent = c(sm_parents_(form, sep), geo_parents_(form)),
    geo_labels = geo_var_labels_(form, form_lang)
  )
}

#' Parts added to the name of a spatial question for its derived columns
#' @noRd
geo_parts_ <- list(
  geopoint = c("latitude", "longitude", "altitude", "precision", "wkt"),
  geotrace = "wkt",
  geoshape = "wkt"
)

#' Question of each column derived from a spatial question, named by column
#' @noRd
geo_parents_ <- function(form) {
  geo <- form[form$type %in% names(geo_parts_) & !is.na(form$name), ]
  geo <- geo[!duplicated(geo$name), ]
  if (nrow(geo) == 0L) {
    return(setNames(character(), character()))
  }
  part <- geo_parts_[geo$type]
  parent <- rep.int(geo$name, lengths(part))
  setNames(parent, paste0(parent, "_", unlist(part, use.names = FALSE)))
}

#' Rename the columns that have the name of a derived spatial column
#'
#' A question `track_wkt` next to a geotrace `track` is renamed `track_wkt_1`,
#' as a question named like a select_multiple dummy column is. The renames
#' are added to the `kobo_displaced` attribute.
#' @importFrom rlang names_inform_repair
#' @noRd
displace_geo_cols_ <- function(x, geo_parent, displaced = NULL) {
  nm <- names(x)
  derived <- names(geo_parent)[geo_parent %in% nm]
  old_cols <- intersect(nm, derived)
  if (length(old_cols) == 0L) {
    return(structure(x, kobo_displaced = displaced))
  }
  taken <- union(nm, derived)
  new_cols <- make.unique(c(taken, old_cols), sep = "_")[-seq_along(taken)]
  names_inform_repair(old_cols, new_cols)
  names(x)[match(old_cols, nm)] <- new_cols
  structure(x, kobo_displaced = c(displaced, setNames(old_cols, new_cols)))
}

#' Labels of the columns derived from the spatial questions
#'
#' The coordinates and the WKT of a question labeled "Location" are labeled
#' "Location::latitude" to "Location::wkt", as the dummy columns of a
#' select_multiple question are. A question without label in this language
#' gives its name.
#' @param form_lang rows of the form in the language of the labels.
#' @noRd
geo_var_labels_ <- function(form, form_lang) {
  parts <- geo_parts_
  geo <- form[form$type %in% names(parts) & !is.na(form$name), ]
  geo <- geo[!duplicated(geo$name), ]
  if (nrow(geo) == 0L) {
    return(list())
  }
  part <- parts[geo$type]
  ## The deployed version comes first, so its label wins
  label <- form_lang$label[match(geo$name, form_lang$name)]
  label <- ifelse(is.na(label), geo$name, label)
  setNames(
    as.list(paste0(rep.int(label, lengths(part)), "::", unlist(part))),
    paste0(rep.int(geo$name, lengths(part)), "_", unlist(part))
  )
}

#' @importFrom labelled set_variable_labels
#' @importFrom stats setNames
#' @noRd
var_labels_from_form_ <- function(
  x,
  form,
  lang,
  precomputed_var_labels = NULL,
  sep = "_"
) {
  if (is.null(precomputed_var_labels)) {
    precomputed_var_labels <- precompute_var_labels_(form, lang, sep)
  }
  form_labels <- precomputed_var_labels$form_labels
  sm_labels <- precomputed_var_labels$sm_labels
  nm <- names(x)
  q <- col_questions_(x, precomputed_var_labels$sm_parent %||% character())
  labels <- setNames(as.list(nm), nm)
  geo_labels <- precomputed_var_labels$geo_labels
  is_geo <- nm %in% names(geo_labels)
  labels[is_geo] <- geo_labels[nm[is_geo]]
  is_sm <- nm %in% names(sm_labels)
  labels[is_sm] <- sm_labels[nm[is_sm]]
  ## A question label wins, except on a dummy column, which is no question
  has_q <- !is.na(q) & q %in% names(form_labels)
  labels[has_q] <- form_labels[q[has_q]]
  labels <- replace_na_list_efficient_(labels)
  set_var_labels_(x, labels)
}

#' Set variable labels
#'
#' Same as `labelled::set_variable_labels(x, .labels = labels, .strict =
#' FALSE)`, setting the columns on a plain list, which is much faster.
#' @importFrom labelled set_label_attribute
#' @noRd
set_var_labels_ <- function(x, labels) {
  labels <- labels[intersect(names(labels), names(x))]
  attrs <- attributes(x)
  x <- unclass(x)
  for (v in names(labels)) {
    x[[v]] <- set_label_attribute(x[[v]], labels[[v]])
  }
  attributes(x) <- attrs
  x
}

#' @noRd
is_num_ <- function(x) {
  grepl("^[+-]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][+-]?[0-9]+)?$", x)
}

#' Coordinates of one point in WKT order
#'
#' KoboToolbox stores a point as "latitude longitude altitude accuracy". WKT
#' wants "longitude latitude altitude", and altitude is optional.
#'
#' @param p character, the parts of one point.
#' @returns The WKT coordinates, or `NA` when the point is not numeric.
#' @noRd
wkt_coords_ <- function(p) {
  p <- p[seq_len(min(length(p), 3L))]
  if (length(p) < 2L || !all(is_num_(p))) {
    return(NA_character_)
  }
  paste(c(p[2], p[1], p[-(1:2)]), collapse = " ")
}

#' Split spatial answers into points
#'
#' @returns A list with, for each answer, the parts of each point. Missing and
#'   empty answers give `NULL`.
#' @noRd
geo_points_ <- function(x, sep = ";") {
  x <- as.character(x)
  lapply(x, function(s) {
    if (is.na(s)) {
      return(NULL)
    }
    pts <- trimws(strsplit(s, sep, fixed = TRUE)[[1]])
    pts <- pts[nzchar(pts)]
    if (length(pts) == 0L) {
      return(NULL)
    }
    strsplit(pts, "\\s+")
  })
}

#' WKT of geotrace or geoshape answers
#' @noRd
wkt_geo_lines_ <- function(x, open, close, min_pts, ring = FALSE) {
  vapply(geo_points_(x), function(pts) {
    if (is.null(pts) || length(pts) < min_pts) {
      return(NA_character_)
    }
    xy <- vapply(pts, wkt_coords_, character(1))
    if (anyNA(xy)) {
      return(NA_character_)
    }
    ## Points with and without altitude cannot share a geometry, and the
    ## ring of a polygon ends on its first point
    n_coords <- lengths(strsplit(xy, " ", fixed = TRUE))
    if (length(unique(n_coords)) > 1L || (ring && xy[1L] != xy[length(xy)])) {
      return(NA_character_)
    }
    paste0(open, paste(xy, collapse = ", "), close)
  }, character(1))
}

#' Add the coordinates and the WKT of geopoint questions
#'
#' Each geopoint column is preceded by its `_latitude`, `_longitude`,
#' `_altitude` and `_precision` parts, and a `_wkt` column is added at the end.
#' Missing, empty or short answers give `NA` parts. The labels of the added
#' columns come from `geo_var_labels_()`.
#' @importFrom tibble as_tibble
#' @noRd
extract_geopoint_ <- function(x, form) {
  nm <- intersect(names(x), unique(form$name[form$type %in% "geopoint"]))
  if (length(nm) == 0L) {
    return(x)
  }
  parts <- c("latitude", "longitude", "altitude", "precision")
  out <- list()
  wkt <- list()
  for (col in names(x)) {
    if (col %in% nm) {
      pts <- geo_points_(x[[col]], sep = "\n")
      p <- lapply(pts, function(pt) if (is.null(pt)) character() else pt[[1]])
      for (i in seq_along(parts)) {
        v <- vapply(p, function(pt) {
          if (length(pt) >= i && is_num_(pt[[i]])) pt[[i]] else NA_character_
        }, character(1))
        out[[paste0(col, "_", parts[i])]] <- v
      }
      w <- vapply(p, wkt_coords_, character(1))
      w[!is.na(w)] <- paste0("POINT (", w[!is.na(w)], ")")
      wkt[[paste0(col, "_wkt")]] <- w
    }
    out[col] <- list(x[[col]])
  }
  as_tibble(c(out, wkt), .name_repair = "minimal")
}

#' Add the WKT of geotrace questions
#' @noRd
extract_geotrace_ <- function(x, form) {
  nm <- intersect(names(x), unique(form$name[form$type %in% "geotrace"]))
  for (col in nm) {
    w <- wkt_geo_lines_(x[[col]], "LINESTRING (", ")", min_pts = 2L)
    x[[paste0(col, "_wkt")]] <- w
  }
  x
}

#' Add the WKT of geoshape questions
#' @noRd
extract_geoshape_ <- function(x, form) {
  nm <- intersect(names(x), unique(form$name[form$type %in% "geoshape"]))
  for (col in nm) {
    w <- wkt_geo_lines_(x[[col]], "POLYGON ((", "))", min_pts = 4L, ring = TRUE)
    x[[paste0(col, "_wkt")]] <- w
  }
  x
}

#' @noRd
is_zero_length_or_null <- function(x) {
  is.null(x) | length(x) == 0
}

#' @noRd
is_uid_available <- function(x) {
  length(x) > 0 && !is.null(x$uid)
}

#' @noRd
is_label_available <- function(x) {
  length(x) > 0 && !is.null(x$label)
}

#' @noRd
#' @importFrom dplyr select
#' @importFrom labelled labelled
#' @importFrom tidyselect any_of
remove_list_cols <- function(x) {
  val_col <- x[["_validation_status"]]
  cond <- all(vapply(val_col, is_zero_length_or_null, logical(1)))
  if (!cond) {
    val_stat <- vapply(
      val_col,
      \(x) {
        if (!is_uid_available(x)) {
          NA_character_
        } else {
          x$uid
        }
      },
      character(1)
    )
    val_lbl <- vapply(
      val_col,
      \(x) {
        if (!is_label_available(x)) {
          NA_character_
        } else {
          x$label
        }
      },
      character(1)
    )
    b <- is.na(val_stat)
    val_dict <- setNames(val_stat[!b], val_lbl[!b])
    val_dict <- val_dict[unique(names(val_dict))]
    x <- x |>
      mutate(`_validation_status` = labelled(val_stat, val_dict))
    x <- select(x, -is_list_cols(x), any_of("_attachments"))
  } else {
    x <- x |>
      mutate(`_validation_status` = NA_character_)
    x <- select(x, -is_list_cols(x), any_of("_attachments"))
  }
  x
}

#' @noRd
add_missing_cols_ <- function(x, cn) {
  ## Added as text, so that type_convert_() gives them the type of their
  ## question, as for the columns that have answers
  x[setdiff(cn, names(x))] <- NA_character_
  x
}

#' Table without rows, with the columns, types and labels of the form
#'
#' The columns have the types they get with data, so an empty result can be
#' bound to a result with rows.
#' @param keys tibble without rows holding the key columns of a repeat table,
#'   or `NULL`.
#' @noRd
empty_table_ <- function(cn, form, lang, sep, keys = NULL) {
  pc_var_labels <- precompute_var_labels_(form, lang, sep)
  x <- empty_tibble_(as.character(cn))
  x <- type_convert_(x, form, dummies = dummy_names_(pc_var_labels, form))
  if (!is.null(keys)) {
    x <- tibble(x, keys)
  }
  x <- val_labels_from_form_(x, form, lang,
                             questions = col_questions_(x, pc_var_labels$sm_parent))
  var_labels_from_form_(x, form, lang, pc_var_labels, sep = sep)
}

#' Names of the dummy columns of the select_multiple questions
#' @noRd
dummy_names_ <- function(precomputed_var_labels, form) {
  setdiff(names(precomputed_var_labels$sm_parent), names(geo_parents_(form)))
}

#' @importFrom dplyr select
#' @importFrom tidyselect any_of everything
#' @noRd
reorder_cols_ <- function(x, cn) {
  cn <- intersect(cn, names(x))
  select(x, any_of(cn), everything())
}

#' @importFrom utils type.convert
postprocess_data_ <- function(
  x,
  form,
  lang,
  select_multiple_label = FALSE,
  cn,
  select_multiple_sep = "_",
  precomputed_choices = NULL,
  precomputed_var_labels = NULL,
  precomputed_types = NULL,
  precomputed_sm = NULL,
  fields = NULL
) {
  x <- dummy_from_form_(x, form, sep = select_multiple_sep, sm_choices = precomputed_sm)
  if (is.null(precomputed_var_labels)) {
    precomputed_var_labels <- precompute_var_labels_(form, lang, select_multiple_sep)
  }
  geo_parent <- precomputed_var_labels$sm_parent
  geo_parent <- geo_parent[names(geo_parent) %in% names(geo_parents_(form))]
  x <- displace_geo_cols_(x, geo_parent, attr(x, "kobo_displaced", exact = TRUE))
  displaced <- attr(x, "kobo_displaced", exact = TRUE)
  attr(x, "kobo_displaced") <- NULL
  x <- extract_geopoint_(x, form)
  x <- extract_geotrace_(x, form)
  x <- extract_geoshape_(x, form)
  if (!is.null(fields)) {
    x <- add_missing_cols_(x, intersect(cn, fields))
  } else {
    x <- add_missing_cols_(x, cn)
  }
  x <- type_convert_(
    x,
    form,
    map = precomputed_types,
    displaced = displaced,
    dummies = dummy_names_(precomputed_var_labels, form)
  )
  x <- remove_list_cols(x)
  if (length(displaced) > 0) {
    x <- set_displaced_(x, displaced)
    cn <- displaced_order_(cn, displaced, form, select_multiple_sep)
  }
  x <- val_labels_from_form_(
    x = x,
    form = form,
    lang = lang,
    precomputed_choices = precomputed_choices,
    questions = col_questions_(x, precomputed_var_labels$sm_parent)
  )
  x <- var_labels_from_form_(
    x = x,
    form = form,
    lang = lang,
    precomputed_var_labels = precomputed_var_labels,
    sep = select_multiple_sep
  )
  if (isTRUE(select_multiple_label)) {
    x <- val_labels_sm_from_form_(
      x = x,
      form = form,
      lang = lang,
      questions = col_questions_(x, precomputed_var_labels$sm_parent)
    )
  }
  x <- reorder_cols_(x, cn)
}

#' @noRd
kobo_question_types <- function() {
  c(
    "start",
    "end",
    "today",
    "deviceid",
    "phonenumber",
    "username",
    "email",
    "audit",
    "integer",
    "decimal",
    "range",
    "text",
    "select_one",
    "select_multiple",
    "select_one_from_file",
    "select_multiple_from_file",
    "rank",
    "note",
    "geopoint",
    "geotrace",
    "geoshape",
    "date",
    "time",
    "dateTime",
    "datetime",
    "image",
    "audio",
    "background-audio",
    "video",
    "file",
    "barcode",
    "calculate",
    "acknowledge",
    "hidden"
  )
}

#' @noRd
kobo_display_fields <- function() {
  c(
    "label",
    "hint",
    "guidance_hint",
    "constraint_message",
    "required_message",
    "image",
    "media::image",
    "big-image",
    "media::big-image",
    "audio",
    "media::audio",
    "video",
    "media::video"
  )
}

#' @importFrom rlang set_names
#' @noRd
empty_tibble_ <- function(cnames) {
  cnames <- cnames[!is.na(cnames)]
  tibble(!!!cnames, .rows = 0, .name_repair = ~cnames)
}

#' TRUE when columns are named after their labels
#'
#' Columns renamed by `colnames_label = TRUE` keep their question name in the
#' `kobo_name` attribute. Data without it, from older versions, is taken as
#' named after labels when more columns match a label than a question name.
#' @noRd
is_label_cols_ <- function(tbls, form) {
  if (has_kobo_names_(tbls)) {
    return(TRUE)
  }
  cols <- unlist(lapply(tbls, names), use.names = FALSE)
  sum(cols %in% form$label) > sum(cols %in% form$name)
}

#' TRUE when some column keeps its question name in `kobo_name`
#' @noRd
has_kobo_names_ <- function(tbls) {
  any(vapply(tbls, function(tbl) {
    any(vapply(tbl, function(col) !is.null(attr(col, "kobo_name", exact = TRUE)),
               logical(1)))
  }, logical(1)))
}

#' Returns c("original_name" = "label_col", ...) for use with rename()
#'
#' Labels shared by several questions are skipped, since they cannot be mapped
#' back.
#' @noRd
reverse_label_lookup_ <- function(label_cols, form, lang) {
  form_lang <- form[form$lang %in% lang, ]
  name_to_lbl <- setNames(form_lang$label, form_lang$name)
  name_to_lbl <- name_to_lbl[!duplicated(names(name_to_lbl)) & !is.na(name_to_lbl)]
  shared <- name_to_lbl[duplicated(name_to_lbl)]
  name_to_lbl <- name_to_lbl[!name_to_lbl %in% shared]
  in_data <- name_to_lbl[name_to_lbl %in% label_cols]
  setNames(unname(in_data), names(in_data))
}

#' Column names of each table of a repeat group form
#'
#' Scopes are read in each form version, so a field only present in an older
#' version goes to the table it had in that version. A field that moved between
#' tables is listed in both.
#' @noRd
kobo_form_name_to_list_ <- function(x, sep) {
  x$scope <- repeat_scope_(x)
  x <- x[!x$type %in% c("begin_repeat", "end_repeat"), ]
  x <- x[!dup_rows_(x, c("version", "name", "scope")), ]
  x <- kobo_form_extra_(x, sep)
  x <- x[!dup_rows_(x, c("cnames", "scope")), ]
  split(x$cnames, factor(x$scope, levels = unique(c("main", x$scope))))
}

#' Question names of the columns
#'
#' A column renamed after its label keeps its question name in the
#' `kobo_name` attribute.
#' @noRd
kobo_names_ <- function(x) {
  nm <- names(x)
  vapply(seq_along(nm), function(i) {
    attr(x[[i]], "kobo_name", exact = TRUE) %||% nm[i]
  }, character(1))
}

#' Rename columns and keep their question names in `kobo_name`
#'
#' Every column gets the attribute, renamed or not, so data named after
#' labels stays recognizable when the labels equal the question names.
#' @noRd
rename_kobo_ <- function(x, new_names) {
  orig <- kobo_names_(x)
  cls <- class(x)
  x <- unclass(x)
  for (i in seq_along(x)) {
    attr(x[[i]], "kobo_name") <- orig[i]
  }
  names(x) <- new_names
  class(x) <- cls
  x
}

#' @importFrom labelled var_label
#' @noRd
set_names_from_varlabel_ <- function(x) {
  lbl <- var_label(x)
  nm <- names(x)
  lbl <- vapply(seq_along(nm), function(i) {
    l <- lbl[[i]]
    if (is.null(l) || is.na(l)) nm[i] else l
  }, character(1))
  rename_kobo_(x, make.unique(lbl, sep = "_"))
}

#' @importFrom dplyr mutate case_when
#' @importFrom stats na.omit
#' @noRd
kobo_form_extra_ <- function(form, sep) {
  ## end_group and end_repeat rows have no name and give no column
  form <- form[!is.na(form$name), ]
  type <- form$type
  value_name <- rep(list(""), nrow(form))
  value_name[type %in% "geopoint"] <- list(c(
    "", "_latitude", "_longitude", "_altitude", "_precision", "_wkt"
  ))
  value_name[type %in% c("geotrace", "geoshape")] <- list(c("", "_wkt"))
  if ("choices" %in% names(form)) {
    sm <- which(stri_detect_regex(type, "select_multiple") &
                  !type %in% c("geopoint", "geotrace", "geoshape"))
    value_name[sm] <- lapply(form$choices[sm], function(ch) {
      c("", paste0(sep, unique(ch$value_name)))
    })
  }
  res <- form[rep.int(seq_len(nrow(form)), lengths(value_name)), ]
  res$value_name <- unlist(value_name, use.names = FALSE)
  res$cnames <- paste0(res$name, res$value_name)
  res
}

#' Duplicated rows over some columns
#'
#' Same as `duplicated(x[cols])`, with the faster `data.table` method.
#' @importFrom data.table as.data.table
#' @noRd
dup_rows_ <- function(x, cols) {
  duplicated(as.data.table(x[cols]))
}

#' @importFrom dplyr mutate case_when
#' @importFrom stats na.omit
#' @noRd
kobo_form_names_ <- function(x, sep) {
  x <- kobo_form_extra_(x, sep)
  x <- unique(x$cnames)
  na.omit(x)
}

#' @importFrom readr type_convert
#' @noRd
type_convert_ <- function(x, form, map = NULL, displaced = NULL, dummies = NULL) {
  cols <- kobo_type_cols_(x, form, map, displaced, dummies)
  type_convert(x, col_types = cols)
}

#' Column type of each question of a form
#'
#' Computed once per form, since a form with many versions and repeat groups
#' has many rows and is used for every table.
#' @returns A list with the `readr` type code of each question name, and the
#'   names of the columns derived from the geo questions.
#' @noRd
form_type_map_ <- function(form) {
  time_q <- "time"
  datetime_q <- c("start", "end", "datetime", "dateTime")
  date_q <- c("today", "date")
  int_q <- "integer"
  dbl_q <- c("decimal", "range")
  char_q <- c(
    "deviceid",
    "device_id",
    "phonenumber",
    "username",
    "email",
    "audit",
    "select_one",
    "select_multiple",
    "select_one_from_file",
    "select_multiple_from_file",
    "rank",
    "text",
    "note",
    "barcode",
    "acknowledge",
    "hidden",
    "geopoint",
    "geotrace",
    "geoshape",
    "image",
    "audio",
    "background-audio",
    "video",
    "file"
  )
  ## Assigned from the lowest priority to the highest
  cols <- rep("?", nrow(form))
  cols[form$type %in% char_q] <- "c"
  cols[form$type %in% dbl_q] <- "d"
  cols[form$type %in% int_q] <- "i"
  cols[form$type %in% time_q] <- "c"
  cols[form$type %in% date_q] <- "D"
  cols[form$type %in% datetime_q] <- "T"
  first <- !duplicated(form$name)
  geo <- unique(form$name[form$type %in% c("geopoint", "geotrace", "geoshape")])
  gp <- unique(form$name[form$type %in% "geopoint"])
  list(
    name = form$name[first],
    cols = cols[first],
    geo_wkt = paste0(geo, "_wkt")[length(geo) > 0],
    gp_parts = c(outer(gp, c("_latitude", "_longitude", "_altitude", "_precision"), paste0))
  )
}

#' Column types for `readr::type_convert()`
#'
#' Dummy columns of select_multiple questions are created as integers and
#' need no type, since only character columns are converted. A column named
#' in `displaced` takes the type of its question, and the dummy column that
#' took its name takes none. A dummy column named in `dummies` that has no
#' answer yet is still text, and becomes an integer.
#' @importFrom dplyr coalesce
#' @noRd
kobo_type_cols_ <- function(x, form, map = NULL, displaced = NULL, dummies = NULL) {
  if (is.null(map)) {
    map <- form_type_map_(form)
  }
  q <- names(x)
  if (length(displaced) > 0) {
    q[q %in% displaced] <- NA_character_
    moved <- match(names(displaced), names(x))
    q[moved[!is.na(moved)]] <- displaced[!is.na(moved)]
  }
  v <- map$cols[match(q, map$name)]
  sys_types <- c(
    `_id` = "i",
    `formhub/uuid` = "c",
    `__version__` = "c",
    `_version_` = "c",
    `meta/formVersions` = "c",
    formVersions = "c",
    `meta/instanceID` = "c",
    `meta/deprecatedID` = "c",
    `meta/rootUuid` = "c",
    `_xform_id_string` = "c",
    `_uuid` = "c",
    `_status` = "c",
    `_submitted_by` = "c",
    `_submission_time` = "T",
    `_index` = "i",
    `_parent_index` = "i",
    `_parent_table_name` = "c",
    `_validation_status` = "c"
  )
  v <- coalesce(v, unname(sys_types[names(x)]))
  v[is.na(v) & names(x) %in% map$geo_wkt] <- "c"
  v[is.na(v) & names(x) %in% map$gp_parts] <- "d"
  v[is.na(v) & names(x) %in% dummies] <- "i"
  paste(coalesce(v, "?"), collapse = "")
}

#' Form versions named in the submissions
#'
#' `__version__` holds the version a submission was collected with, `_version_`
#' the same on very old submissions, and `meta/formVersions` (KoboToolbox KPI
#' 2.026.33 and later) every version an edited submission went through, space
#' separated.
#' @noRd
submission_versions_ <- function(x) {
  spanned <- as.character(x[["meta/formVersions"]])
  spanned <- unlist(strsplit(spanned[!is.na(spanned)], " ", fixed = TRUE))
  v <- c(
    as.character(x[["__version__"]]),
    as.character(x[["_version_"]]),
    spanned
  )
  unique(v[!is.na(v) & nzchar(v)])
}

#' Deployed versions of an asset, newest first
#'
#' The asset already lists its deployed versions. The version listing is only
#' requested when that list is incomplete or lacks one of `needed`, as with an
#' asset object fetched before a new deployment.
#' @noRd
deployed_version_uids_ <- function(asset, needed = character()) {
  dv <- asset$deployed_versions
  known <- vapply(dv$results %||% list(), function(v) {
    v$uid %||% NA_character_
  }, character(1))
  complete <- length(known) > 0 && is.null(dv[["next"]]) &&
    isTRUE(length(known) == parse_api_count_(dv$count))
  if (complete && all(needed %in% known)) {
    return(unique(known))
  }
  vl <- kobo_asset_version_list(asset$uid)
  unique(vl$uid[vl$deployed])
}

#' Form used to read the submissions
#'
#' The deployed form comes first, so its labels win. With `all_versions`, the
#' other deployed versions found in the submissions follow, newest first, and
#' add the questions and choices the deployed form lacks.
#' @noRd
kobo_form_version_ <- function(x, asset, all_versions, progress = FALSE) {
  form <- kobo_form(asset)
  sub_versions <- submission_versions_(x)
  if (!all_versions || length(sub_versions) == 0)
    return(form)
  extra <- setdiff(sub_versions, unique(form$version))
  if (length(extra) == 0)
    return(form)

  uid <- asset$uid
  form_versions <- deployed_version_uids_(asset, extra)
  versions <- intersect(form_versions, sub_versions)
  versions <- setdiff(versions, unique(form$version))
  if (length(versions) == 0)
    return(form)

  uncached <- versions[!vapply(versions, \(v) has_cached_form_(uid, v), logical(1))]
  if (length(uncached) > 1) {
    tick <- if (isTRUE(progress)) {
      progress_ticker_("Reading form versions", total = length(uncached))
    }
    asset_vers <- tryCatch(
      fetch_asset_versions_async_(uid, uncached, tick = tick),
      error = function(e) NULL
    )
    progress_done_(tick)
    if (!is.null(asset_vers)) {
      for (i in seq_along(uncached)) {
        frm <- kobo_form_api_(asset_vers[[i]], uncached[i])
        set_cached_form_(uid, uncached[i], frm)
      }
    } else {
      for (v in uncached) {
        kobo_form(asset, v)
      }
    }
  } else if (length(uncached) == 1) {
    kobo_form(asset, uncached)
  }
  form <- c(list(form), lapply(versions, \(v) get_cached_form_(uid, v)))
  ## kobo_data_() reads only these columns, and the stack can be large
  keep <- c("name", "list_name", "type", "label", "lang", "version", "choices")
  form <- lapply(form, \(f) f[intersect(keep, names(f))])
  dt2tibble(rbindlist(form, fill = TRUE))
}

#' @importFrom crul HttpRequest
#' @importFrom RcppSimdJson fparse
#' @noRd
fetch_asset_versions_async_ <- function(uid, versions, tick = NULL) {
  check_setup_()
  headers <- list(
    Authorization = paste("Token", Sys.getenv("KOBOTOOLBOX_TOKEN")),
    `User-Agent` = user_agent_()
  )
  reqs <- lapply(versions, function(v) {
    url <- api_url_(asset_path_(uid, v))
    HttpRequest$new(url, headers = headers)$retry(
      "get",
      times = 3L,
      retry_only_on = retry_codes_,
      terminate_on = 404
    )
  })
  async_fetch_batches_(reqs, tick = tick, handle = function(content, i) {
    asset_ver <- fparse(content, max_simplify_lvl = "list")
    asset_ver$asset_uid <- uid
    structure(asset_ver, class = "kobo_asset_version")
  })
}
