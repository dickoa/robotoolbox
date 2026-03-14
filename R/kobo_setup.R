#' Get robotoolbox settings
#'
#' Print the KoboToolbox server URL and API token currently in use.
#'
#' @name kobo_settings
#'
#' @returns A list with information about your KoboToolbox server URL and token.
#'
#' @examples
#' \dontrun{
#'  kobo_settings()
#' }
#'
#' @export
kobo_settings <- function() {
  token <- Sys.getenv("KOBOTOOLBOX_TOKEN", "")
  url <- Sys.getenv("KOBOTOOLBOX_URL", "")
  url <- clean_urls(url)
  page_size <- api_max_limit_()
  ops <- list(token = token,
              url = url,
              page_size = page_size)
  structure(ops, class = "kobo_settings")
}

#' Set robotoolbox settings
#'
#' Set the KoboToolbox server URL, API token and return invisibly
#' a \code{kobo_settings} object.
#'
#' @importFrom rlang abort
#'
#' @name kobo_setup
#'
#' @param url character, the base URL of the KoboToolbox server.
#' @param token character, the API token.
#' @param page_size integer, the maximum number of submissions per API request.
#'   Default to `1000L`, which is the limit enforced by public KoboToolbox
#'   servers since KoboToolbox KPI version 2.026.03 (March 2026). Users with private servers that allow higher
#'   limits can set this to a larger value (e.g., `30000L`) to fetch more
#'   data per request and reduce the number of paginated calls.
#'
#' @returns A \code{kobo_settings} object printing the server URL and the API token.
#'
#' @examples
#' \dontrun{
#'  # Public server (default page_size = 1000)
#'  kobo_setup(url = "https://kf.kobotoolbox.org/",
#'             token = "9et1814c285w094f6v9bd629df47a1a0e81x53a0")
#'
#'  # Private server with higher limit
#'  kobo_setup(url = "https://myserver.org/",
#'             token = "9et1814c285w094f6v9bd629df47a1a0e81x53a0",
#'             page_size = 30000)
#'  kobo_settings()
#' }
#'
#' @export
kobo_setup <- function(url = Sys.getenv("KOBOTOOLBOX_URL", ""),
                       token = Sys.getenv("KOBOTOOLBOX_TOKEN", ""),
                       page_size = 1000L) {
  if (!assert_url(url))
    abort(message = "Invalid URL")

  if (!assert_token(token))
    abort(message = "Invalid token")

  if (!is.numeric(page_size) || length(page_size) != 1 || page_size < 1)
    abort("`page_size` must be a positive integer", call = NULL)

  Sys.setenv("KOBOTOOLBOX_URL" = clean_urls(url))
  if (token != "")
    Sys.setenv("KOBOTOOLBOX_TOKEN" = token)
  Sys.setenv("KOBOTOOLBOX_PAGE_SIZE" = as.integer(page_size))
  invisible(kobo_settings())
}

#' @export
print.kobo_settings <- function(x, ...) {
  cat("<robotoolbox settings> \n")
  cat("   KoboToolbox URL: ", x$url, "\n", sep = "")
  cat("   KoboToolbox API Token: ", hide_token(x$token), "\n", sep = "")
  cat("   Max page size: ", x$page_size, "\n", sep = "")
  invisible(x)
}
