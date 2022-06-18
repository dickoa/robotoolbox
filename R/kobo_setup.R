#' Get \code{robotoolbox} settings
#'
#' Print the current token and server url used.
#'
#' @return a list, prints your server url and token
#'
#' @examples
#' \dontrun{
#'  kobo_settings()
#' }
#'
#' @export
kobo_settings <- function() {
  ops <- list(token = Sys.getenv("KOBOTOOLBOX_TOKEN", ""),
              url = Sys.getenv("KOBOTOOLBOX_URL", ""))
  structure(ops, class = "kobo_settings")
}

#' Set the \code{robotoolbox} settings
#'
#' Set the KoboToolbox server url and the API token.
#'
#' @param url character, the base url of the KoboToolbox server
#' @param token character, the API token
#'
#' @return a \code{\link{kobo_settings}} object
#'
#' @examples
#' \dontrun{
#'  kobo_setup(url = "https://kf.kobotoolbox.org/",
#'             token = "9et1814c285w094f6v9bd629df47a1a0e81x53a0")
#'  kobo_settings()
#' }
#'
#'
#' @export
kobo_setup <- function(url = Sys.getenv("KOBOTOOLBOX_URL", ""),
                       token = Sys.getenv("KOBOTOOLBOX_TOKEN", "")) {
  if (token != "")
    Sys.setenv("KOBOTOOLBOX_TOKEN" = token)
  if (url != "")
    Sys.setenv("KOBOTOOLBOX_URL" = url)
  invisible(kobo_settings())
}

#' @export
print.kobo_settings <- function(x, ...) {
  cat("<robotoolbox settings> \n")
  cat("   KoboToolbox URL: ", x$url, "\n", sep = "")
  cat("   KoboToolbox API Token: ", x$token, "\n", sep = "")
}
