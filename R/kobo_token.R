#' Get your KoboToolbox API token
#'
#' Get your KoboToolbox API token from your username and password.
#'
#'
#' @importFrom crul auth HttpClient
#' @importFrom RcppSimdJson fparse
#'
#' @param username character, KoboToolbox username
#' @param password character, KoboToolbox password
#' @param url character, KoboToolbox server url
#' @param overwrite logical, if TRUE overwrite existing token
#'
#' @examples
#' \dontrun{
#' token <- kobo_setup(username = "cool_user_name",
#'                     password = "xsww3@dddb",
#'                     url = "https://kf.kobotoolbox.org/")
#' token
#' }
#'
#' @return character, the KoboToolbox API token
#' @export
kobo_token <- function(username = NULL, password = NULL,
                       url = NULL, overwrite = FALSE) {
  if (!is.null(url) & !nzchar(Sys.getenv("KOBOTOOLBOX_URL")))
     Sys.setenv("KOBOTOOLBOX_URL" = url)

  if (nzchar(Sys.getenv("KOBOTOOLBOX_TOKEN")) & !overwrite) {
    token <- Sys.getenv("KOBOTOOLBOX_TOKEN")
  } else {
    url_path <- file.path(url, "token/?format=json")
    cli <- crul::HttpClient$new(url = url_path,
                                auth = auth(user = username,
                                            pwd = password))
    res <- cli$get()
    res$raise_for_status()
    res$raise_for_ct_json()
    res <- fparse(res$parse("UTF-8"))
    token <- res$token
    Sys.setenv("KOBOTOOLBOX_TOKEN" = token)
    Sys.setenv("KOBOTOOLBOX_URL" = url)
  }
  token
}
