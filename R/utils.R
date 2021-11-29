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
  res$parse("UTF-8")
}

#' @noRd
map_character <- function(x, key)
  vapply(x, function(l) l[[key]], character(1))

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

#' @importFrom stringi stri_trans_general
#' @noRd
clean_subs_colnames <- function(x, group_names = FALSE) {
  nm <- stri_trans_general(x, id = "Latin-ASCII")
  if (isTRUE(group_names)) {
    nm <- gsub("\\/", "\\_", nm)
  } else {
    nm <- basename(nm)
  }
  nm
}

#' @noRd
form_lang <- function(x, lang) {
  ss <- sum(lengths(x$label))
  x$lang <- lang[seq.int(ss)]
  x
}

#' @noRd
#' @importFrom readr type_convert col_character
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#' @importFrom dplyr select
#' @importFrom tidyselect contains matches
format_kobo_submissions <- function(x, group_names = FALSE) {
  if (!is.null(x)) {
    x <- select(x, -contains(c("instanceid", "xform", "formhub",
                               "deprecatedid", "subscriberid",
                               "imei", "_attachments"),
                             ignore.case = TRUE),
                -matches("^_version_$", perl = TRUE, ignore.case = TRUE))
    new_names <- clean_subs_colnames(names(x),
                                     group_names = group_names)
    res <- suppressMessages(type_convert(x))
    res <- setNames(res,
                    new_names)
    res <- suppressMessages(as_tibble(res,
                                      .name_repair = "universal"))
    x <- res
  }
  x
}

#' @importFrom fastDummies dummy_cols
#' @noRd
dummify_kobo_submissions <- function(x, cols, split = " ", ...) {
  dummy_cols(x,
             select_columns = cols,
             split = split,
             remove_selected_columns = TRUE,
             ...)
}


#' @importFrom tidyr separate
#' @noRd
geopoint_kobo_submissions <- function(x, cols) {
  separate(x, cols,
           into = c("lat_gps", "lon_gps", "alt_gps", "accuracy_gps"),
           sep = "\\s+",
           remove = TRUE,
           convert = TRUE)
}

## mtabulate <- function(vects) {
##     lev <- sort(unique(unlist(vects)))
##     dat <- do.call(rbind, lapply(vects, function(x, lev){
##         tabulate(factor(x, levels = lev, ordered = TRUE),
##         nbins = length(lev))}, lev = lev))
##     colnames(dat) <- sort(lev)
##     data.frame(dat, check.names = FALSE)
## }

## #' @importFrom data.table as.data.table alloc.col set
## #' @importFrom stringr str_sort
## #' @importFrom tibble as_tibble
## #' @noRd
## dummify_kobo_submissions <- function(x, cols) {
##   .data <- as.data.table(x)
##   for (col_name in cols) {
##     unique_vals <- unique(.data[[col_name]])
##     unique_vals <- stringr::str_sort(unique_vals,
##                                      na_last = TRUE,
##                                      locale = "en_US", numeric = TRUE)
##     unique_vals <- unique(trimws(unlist(strsplit(unique_vals,
##                                                  split = "\\s+"))))
##     unique_vals <- unique_vals[!is.na(unique_vals)]
##     data.table::alloc.col(.data, ncol(.data) + length(unique_vals))
##     .data[, paste0(col_name, "_", unique_vals)] <- 0L
##     for (unique_value in unique_vals) {
##       data.table::set(.data,
##                       i = which(data.table::chmatch(as.character(.data[[col_name]]),
##                                                     unique_value, nomatch = 0) == 1L),
##                       j = paste0(col_name, "_", unique_value), value = 1L)
##       data.table::set(.data, i = which(is.na(.data[[col_name]])),
##                       j = paste0(col_name, "_", unique_value),
##                       value = NA)
##       max_split_length <- max(sapply(strsplit(as.character(.data[[col_name]]),
##                                               split = "\\s+"), length))
##       for (split_length in 1:max_split_length) {
##         data.table::set(.data,
##                         i = which(data.table::chmatch(as.character(trimws(sapply(strsplit(as.character(.data[[col_name]]),
##                                                                                           split = "\\s+"),
##                                                                                  `[`, split_length))), unique_value, nomatch = 0) == 1L),
##                         j = paste0(col_name, "_", unique_value), value = 1L)
##       }
##     }
##   }
##   as_tibble(.data[-which(names(.data) %in% cols)])
## }