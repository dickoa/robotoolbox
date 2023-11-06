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
  ops <- list(token = token,
              url = url)
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
#'
#' @returns A \code{kobo_settings} object pritting the server URL and the API token.
#'
#' @examples
#' \dontrun{
#'  # use your own URL and token
#'  kobo_setup(url = "https://kf.kobotoolbox.org/",
#'             token = "9et1814c285w094f6v9bd629df47a1a0e81x53a0")
#'  kobo_settings()
#' }
#'
#' @export
kobo_setup <- function(url = Sys.getenv("KOBOTOOLBOX_URL", ""),
                       token = Sys.getenv("KOBOTOOLBOX_TOKEN", "")) {
  if (!assert_url(url))
    abort(message = "Invalid URL")

  if (!assert_token(token))
    abort(message = "Invalid token")

  Sys.setenv("KOBOTOOLBOX_URL" = clean_urls(url))
  if (token != "")
    Sys.setenv("KOBOTOOLBOX_TOKEN" = token)
  invisible(kobo_settings())
}

#' @export
print.kobo_settings <- function(x, ...) {
  cat("<robotoolbox settings> \n")
  cat("   KoboToolbox URL: ", x$url, "\n", sep = "")
  cat("   KoboToolbox API Token: ", hide_token(x$token), "\n", sep = "")
}
