#' Get \code{robotoolbox} settings
#' @export
kobo_settings <- function() {
  ops <- list(token = Sys.getenv("KOBOTOOLBOX_TOKEN", ""),
              url = Sys.getenv("KOBOTOOLBOX_URL", ""))
  structure(ops, class = "kobo_settings")
}

#' Set \code{robotoolbox} settings
#' @param url the base url of the KoboToolbox server
#' @param token the API token
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
