#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

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
kobo_settings <- function() {
  ops <- list(token = Sys.getenv("KOBOTOOLBOX_TOKEN", ""),
              url = Sys.getenv("KOBOTOOLBOX_URL", ""))
  structure(ops, class = "kobo_settings")
}

#' @noRd
kobo_setup <- function(url = NULL, token = NULL) {
  if (!is.null(token))
    Sys.setenv("KOBOTOOLBOX_TOKEN" = token)
  if (!is.null(url))
    Sys.setenv("KOBOTOOLBOX_URL" = url)
}

#' Custom printing for KoBoToolbox API settings
#'
#' Custom printing for KoBoToolbox API settings
#'
#' @param x kobo_settings, the asset
#' @param ... non used
#' @noRd
print.kobo_settings <- function(x, ...) {
  cat("<KoBoToolbox Settings> \n")
  cat("   URL: ", kobo_settings()$url, "\n", sep = "")
  cat("   Token: ", kobo_settings()$token, "\n", sep = "")
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
parse_kobo_date <- function(date)
  as.character(as.POSIXct(date,
                          format = "%Y-%m-%dT%H:%M:%OS",
                          tz = "GMT"))

#' @noRd
is_list_cols <- function(df)
  which(vapply(df, is.list, logical(1)))

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

#' @importFrom readr type_convert
#' @importFrom stats setNames
postprocess_submission <- function(x) {
  res <- setNames(type_convert(x),
                  basename(names(x)))
  res
}