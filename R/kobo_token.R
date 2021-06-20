#' Get your KoBoToolbox API token from your username and password
#'
#' Get your KoBoToolbox API token from your username and password
#'
#'
#' @importFrom crul auth HttpClient
#' @importFrom jsonlite fromJSON
#' @param username character, KoBoToolbox username
#' @param password character, KoBoToolbox password
#' @param url character, KoBoToolbox server url
#'
#' @return character, the KoBoToolbox API token
#' @export
kobo_token <- function(username = NULL, password = NULL, url = NULL) {
  if (!is.null(url) & !nzchar(Sys.getenv("KOBOTOOLBOX_URL")))
     Sys.setenv("KOBOTOOLBOX_URL" = url)

  if (nzchar(Sys.getenv("KOBOTOOLBOX_TOKEN"))) {
    token <- Sys.getenv("KOBOTOOLBOX_TOKEN")
  } else {
    cli <- crul::HttpClient$new(url = Sys.getenv("KOBOTOOLBOX_URL"),
                                auth = auth(user = username,
                                            pwd = password))
    res <- cli$get("/token", list(format = "json"))
    res$raise_for_status()
    token <- fromJSON(res$parse("UTF-8"))$token
    Sys.setenv("KOBOTOOLBOX_TOKEN" = token)
  }
  token
}
