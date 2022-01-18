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
#' @noRd
get_subs <- function(uid, args = list(), ...) {
  path <- paste0("api/v2/assets/", uid, "/data.json")
  res <- xget(path = path, args = args, ...)
  res <- fparse(res, max_simplify_lvl = "data_frame")
  res$result
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
  res
}

#' @noRd
map_chr2 <- function(x, key)
  vapply(x, function(l) as.character(l[[key]]), character(1))

#' @noRd
map_int2 <- function(x, key)
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

#' @importFrom data.table as.data.table alloc.col `:=` chmatch set
#' @importFrom stringi stri_sort stri_detect_regex
#' @importFrom tibble as_tibble
#' @importFrom stats na.omit
#' @noRd
fast_dummy_cols <- function(x, cols) {
  x <- as.data.table(x)
  for (col in cols) {
    y <- x[[col]]
    uv <- unique(unlist(strsplit(y, "\\s+")))
    uv <- trimws(na.omit(uv))
    uv <- stri_sort(uv,
                    na_last = TRUE,
                    locale = "en_US",
                    numeric = TRUE)
    new_names <- paste0(col, "_", uv)
    alloc.col(x, ncol(x) + length(uv))
    x[, (new_names) := 0L]
    x[which(is.na(y)), (new_names) := NA_integer_]
    for (iter in seq_along(uv)) {
      j <- paste0(col, "_", uv[iter])
      i <- which(stri_detect_regex(y, paste0("\\b", uv[iter], "\\b")))
      set(x, i = i, j = j, value = 1L)
    }
  }
  as_tibble(x)
}

#' @noRd
dummy_from_form_ <- function(x, form) {
  nm <- unique(form$name[form$type %in% "select_multiple"])
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    x <- fast_dummy_cols(x, nm)
  } else {
    x
  }
  x
}

#' @noRd
make_unique_names_ <- function(x, ...)
  make.unique(basename(x), sep = "_")

#' @importFrom purrr some map modify_if
#' @importFrom rlang set_names
#' @noRd
name_repair_ <- function(x) {
  if (is.null(x))
    return(NULL)
  modify_if(set_names(x, make_unique_names_),
            ~ some(., is.data.frame),
            ~ map(., name_repair_))
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
  res <- lapply(seq_along(x), function(i)
    if (is.data.frame(x[[i]])) {
      c(setNames(list(x[[i]]), names(x[i])),
        lapply(extract_repeat_tbl(x[[i]], form), function(df) {
               df$`_parent_table_name` <- names(x[i])
               df}))
      } else {
        list()
      })
  suppressWarnings(squash(res))
}

#' @importFrom dplyr mutate across
#' @importFrom stats setNames
#' @importFrom labelled set_value_labels
#' @noRd
val_labels_from_form_ <- function(x, form, lang) {
  cond <- form$lang %in% lang & form$type %in% "select_one"
  form <- form[cond, ]
  nm <- unique(form$name)
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    choices <- form$choices
    choices <- lapply(choices, function(ch) {
      ch <- ch[ch$value_lang %in% lang, ]
      ch$value_label <- make.unique(ch$value_label, sep = "_")
      ch <- setNames(ch$value_name, ch$value_label)
      ch[!duplicated(ch)]
    })
    names(choices) <- nm
    labels <- choices[nm]
    x <- set_value_labels(mutate(x, across(nm, as.character)),
                          .labels = labels,
                          .strict = FALSE)
  } else {
    x
  }
  x
}

#' @importFrom labelled set_variable_labels
#' @importFrom stats setNames
#' @noRd
var_labels_from_form_ <- function(x, form, lang) {
  cond <- form$lang %in% lang & form$name %in% names(x)
  form <- form[cond, ]
  nm <- unique(form$name)
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    labels <- setNames(as.list(form$label), form$name)
    x <- set_variable_labels(x,
                             .labels = labels,
                             .strict = FALSE)
  } else {
    x
  }
  x
}

#' @importFrom utils type.convert
#' @noRd
postprocess_data_ <- function(x, form, lang) {
  x <- type.convert(x, as.is = TRUE)
  x <- dummy_from_form_(x, form)
  x <- val_labels_from_form_(x = x, form = form, lang = lang)
  x <- var_labels_from_form_(x = x, form = form, lang = lang)
  x
}
