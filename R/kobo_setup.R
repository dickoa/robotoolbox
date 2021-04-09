#' @noRd
kobo_settings <- function() {
  ops <- list(
    token = Sys.getenv("KOBOTOOLBOX_TOKEN", ""),
    url = Sys.getenv("KOBOTOOLBOX_URL", ""),
    username = Sys.getenv("KOBOTOOLBOX_USERNAME", ""),
    password = Sys.getenv("KOBOTOOLBOX_PASSWORD", ""))
  structure(ops, class = "kobo_settings")
}

#' @noRd
kobo_setup <- function(token = NULL, url = NULL) {
  if (!is.null(token))
    Sys.setenv("KOBOTOOLBOX_TOKEN" = token)
  if (!is.null(url))
    Sys.setenv("KOBOTOOLBOX_URL" = url)
}

#' Get the KoBoToolbox API url
#'
#' @return character, the API url
#' @export
kobo_url <- function() {
  r <- Sys.getenv("KOBOTOOLBOX_URL")
  if (r == "")
    stop("KoBoToolbox API URL not set, please use kobo_setup()",
         call. = FALSE)
  r
}

#' Get verbs
#'
#' Get verbs
#' @param path character, api endpoint
#' @param args API call payload
#' @param ... Option to pass to the get method
#'
#' @return character, API call response
#' @noRd
xget <- function(path, args = list(), ...) {
  token <- paste("Token", kobo_token())
  cli <- crul::HttpClient$new(get_kpi_url(),
                              headers = list(Authorization = token),
                              opts = list(...))
  res <- cli$get(path = path, query = args)
  res$raise_for_status()
  res$parse("UTF-8")
}
