#' @noRd
#' @importFrom utils packageVersion
user_agent <- function(x) {
  robotoolbox_version <- packageVersion("robotoolbox")
  os <- Sys.info()[["sysname"]]
  os_version <- paste(Sys.info()[["release"]],
                      Sys.info()[["version"]])
  r_version <- paste0(R.version$major, ".",
                      R.version$minor,
                      ifelse(R.version$status == "", "",
                             paste0("-", R.version$status)))
  header <- paste0("robotoolbox/",
                   robotoolbox_version, " (", os, "/",
                   os_version, "; ", "R/", r_version, ")")
  header
}

#' @noRd
xget <- function(path, args = list(), ...) {
  headers <- list(Authorization = paste("Token",
                                        Sys.getenv("KOBOTOOLBOX_TOKEN")))
  cli <- crul::HttpClient$new(Sys.getenv("KOBOTOOLBOX_URL"),
                              headers = headers, opts = list(...))
  res <- cli$get(path = path, query = args)
  res$raise_for_status()
  res$raise_for_ct_json()
  res$parse("UTF-8")
}

#' @importFrom RcppSimdJson fparse
#' @importFrom readr type_convert
#' @noRd
get_subs <- function(uid, args = list(), ...) {
  path <- paste0("api/v2/assets/", uid, "/data.json")
  res <- xget(path = path, args = args, ...)
  res <- fparse(res, max_simplify_lvl = "data_frame")
  suppressMessages(type_convert(res$results))
}

#' @importFrom glue glue glue_data
#' @importFrom tibble tibble
#' @noRd
build_urls <- function(uid, size, chunk_size = NULL) {
  if (chunk_size >= size || is.null(chunk_size)) {
    urls <- glue("api/v2/assets/{uid}/data.json", uid = uid)
  } else {
    start <- seq(0, size, by = chunk_size)
    limit <- rep(chunk_size, length(start) - 1)
    last_limit <- size %% chunk_size
    last_limit <- ifelse(last_limit, last_limit, chunk_size)
    limit <- c(limit, last_limit)
    df <- tibble(start = start, limit = limit, uid = uid)
    template <-  "api/v2/assets/{uid}/data.json?start={start}&limit={limit}"
    urls <- glue_data(df,
                      template)
  }
  file.path(Sys.getenv("KOBOTOOLBOX_URL"), urls)
}

#' @importFrom crul Async
#' @importFrom RcppSimdJson fparse
#' @importFrom data.table rbindlist
#' @importFrom readr type_convert
#' @noRd
get_subs_async <- function(uid, size, chunk_size = NULL, ...) {
  headers <- list(Authorization = paste("Token",
                                        Sys.getenv("KOBOTOOLBOX_TOKEN")))
  urls <- build_urls(uid, size, chunk_size)
  cli <- Async$new(urls,
                   headers = headers, opts = list(...))
  res <- cli$get()
  res <- vapply(res, function(r) {
    r$raise_for_status()
    r$raise_for_ct_json()
    r$parse(encoding = "UTF-8")
  }, FUN.VALUE = character(1))
  res <- fparse(res, max_simplify_lvl = "data_frame")
  res <- rbindlist(lapply(res, function(r) r$results),
                   fill = TRUE)
  suppressMessages(type_convert(res))
}

#' @noRd
map_character <- function(x, key)
  vapply(x, function(l) as.character(l[[key]]), character(1))

#' @noRd
map_integer <- function(x, key)
  vapply(x, function(l) l[[key]], integer(1))

#' @noRd
parse_kobo_date <- function(x)
  as.POSIXct(x,
             format = "%Y-%m-%dT%H:%M:%OS",
             tz = "GMT")
#' @noRd
is_list_cols <- function(x)
  which(vapply(x, is.list, logical(1)))

#' @noRd
is_null_recursive <- function(x)
  is.null(x) | all(vapply(x, is.null, logical(1)))

#' @noRd
print_list_res <- function(x)
  paste(unlist(x), collapse = ", ")

#' @noRd
drop_nulls <- function(x) {
  x <- Filter(Negate(is_null_recursive), x)
  lapply(x, function(x)
    if (is.list(x)) drop_nulls(x) else x)
}

#' @noRd
as_log <- function(x) {
  stopifnot(is.logical(x))
  tolower(x)
}

#' @importFrom fastDummies dummy_cols
#' @noRd
dummy_from_form_ <- function(x, form) {
  nm <- unique(form$name[form$type %in% "select_multiple"])
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    x <- dummy_cols(x,
                    nm,
                    split = "\\s+",
                    ignore_na = TRUE)
  } else {
    x
  }
  x
}


#' @importFrom rlang set_names
#' @noRd
postprocess_submissions_ <- function(x, form) {
  if (is.data.frame(x)) {
    x <- set_names(x, basename)
    x <- dummy_from_form_(x, form)
  }
  x
}

#' @importFrom purrr some map modify_if
#' @noRd
kobo_postprocess <- function(x, form) {
  if (is.null(x))
    return(NULL)
  modify_if(postprocess_submissions_(x,
                                     form),
            ~ some(., is.data.frame),
            ~ map(., kobo_postprocess, form = form))
}


#' @importFrom purrr modify_if
#' @importFrom tibble tibble rowid_to_column
#' @importFrom data.table rbindlist
#' @noRd
extract_repeat_tbl <- function(x, form) {
  res <- list()
  nm <- unique(form$name[form$type %in% "begin_repeat"])
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    res <- setNames(lapply(nm, function(n) {
      res <- x[[n]]
      res <- modify_if(res, ~ !is.data.frame(.), ~ tibble())
      res <- rbindlist(res,
                       idcol = "_parent_index",
                       fill = TRUE)
      res <- tibble(res)
      rowid_to_column(res, "_index")
    }), nm)
  }
  res
}

#' @importFrom rlang squash
#' @noRd
kobo_extract_repeat_tbl <- function(x, form) {
  if (is.null(x))
    return(NULL)
  x <- extract_repeat_tbl(x, form)
  x <- lapply(x, function(df) {
    df$`_parent_table_name` <- "main"
    df
  })
  res <- lapply(x, function(y)
    if (is.data.frame(y)) {
      c(setNames(list(y), names(x)),
        lapply(extract_repeat_tbl(y, form), function(df) {
               df$`_parent_table_name` <- names(x)
               df}))
      } else {
        list()
      })
  suppressWarnings(squash(res))
}
