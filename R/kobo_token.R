#' Get your KoBoToolbox API token from your username and password
#'
#' Get your KoBoToolbox API token from your username and password
#'
#'
#' @importFrom crul auth HttpClient
#' @importFrom RcppSimdJson fparse
#' @param username character, KoBoToolbox username
#' @param password character, KoBoToolbox password
#' @param url character, KoBoToolbox server url
#' @param overwrite logical, if TRUE overwrite existing token
#'
#' @return character, the KoBoToolbox API token
#' @export
kobo_token <- function(username = NULL, password = NULL, url = NULL, overwrite = FALSE) {
  if (!is.null(url) & !nzchar(Sys.getenv("KOBOTOOLBOX_URL")))
     Sys.setenv("KOBOTOOLBOX_URL" = url)

  if (nzchar(Sys.getenv("KOBOTOOLBOX_TOKEN")) & !overwrite) {
    token <- Sys.getenv("KOBOTOOLBOX_TOKEN")
  } else {
    cli <- crul::HttpClient$new(url = Sys.getenv("KOBOTOOLBOX_URL"),
                                auth = auth(user = username,
                                            pwd = password))
    res <- cli$get("/token", list(format = "json"))
    res$raise_for_status()
    res$raise_for_ct_json()
    res <- fparse(res$parse("UTF-8"))
    token <- res$token
    Sys.setenv("KOBOTOOLBOX_TOKEN" = token)
  }
  token
}
