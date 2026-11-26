#' Get robotoolbox settings
#'
#' Get the KoboToolbox server URL, the API token and the page size currently
#' in use.
#'
#' @name kobo_settings
#'
#' @returns A `kobo_settings` object, a list with three elements.
#' - `token` the API token
#' - `url` the URL of the KoboToolbox server
#' - `page_size` the number of submissions asked for in each API request
#'
#' Its print method hides the token except for its last characters, and
#' shows `<unset>` when no token is set.
#'
#' @family setup functions
#'
#' @examples
#' kobo_settings()
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
#' Set the KoboToolbox server URL, the API token and the page size for the
#' rest of the R session.
#'
#' @importFrom rlang abort
#'
#' @name kobo_setup
#'
#' @param url character, the base URL of the KoboToolbox server. Defaults to
#'   the `KOBOTOOLBOX_URL` environment variable.
#' @param token character, the API token. Defaults to the `KOBOTOOLBOX_TOKEN`
#'   environment variable. [kobo_token()] gets it from a username and a
#'   password.
#' @param page_size integer, the number of submissions to ask for in each API
#'   request. Defaults to `NULL`, which takes the `KOBOTOOLBOX_PAGE_SIZE`
#'   environment variable, or `1000L` when it is not set. This is the limit of public KoboToolbox servers since
#'   KoboToolbox KPI version 2.026.03 (March 2026). Some private servers allow
#'   more, and a larger value (e.g., `30000L`) then means fewer requests. When
#'   the server returns fewer submissions than asked, `kobo_data()` detects its
#'   limit and requests the missing submissions, so a large value never loses
#'   data.
#'
#' @returns The settings, invisibly, as the `kobo_settings` object that
#'   [kobo_settings()] returns.
#'
#' @family setup functions
#'
#' @examples
#' \dontrun{
#'  # Public server (default page_size = 1000)
#'  kobo_setup(url = "https://kf.kobotoolbox.org",
#'             token = "9et1814c285w094f6v9bd629df47a1a0e81x53a0")
#'
#'  # Private server with higher limit
#'  kobo_setup(url = "https://myserver.org",
#'             token = "9et1814c285w094f6v9bd629df47a1a0e81x53a0",
#'             page_size = 30000)
#'  kobo_settings()
#' }
#'
#' @export
kobo_setup <- function(url = Sys.getenv("KOBOTOOLBOX_URL", ""),
                       token = Sys.getenv("KOBOTOOLBOX_TOKEN", ""),
                       page_size = NULL) {
  if (!assert_url(url))
    abort(message = "Invalid URL")

  if (!assert_token(token))
    abort(message = "Invalid token")

  page_size <- page_size %||% api_max_limit_()
  if (!is.numeric(page_size) || length(page_size) != 1 || is.na(page_size) ||
        page_size < 1)
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
