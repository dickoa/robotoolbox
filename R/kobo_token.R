#' Get your KoboToolbox API token
#'
#' Get your KoboToolbox API token from your username and password.
#'
#' @importFrom crul auth HttpClient
#' @importFrom RcppSimdJson fparse
#'
#' @name kobo_token
#'
#' @param username character, KoboToolbox account username.
#' @param password character, KoboToolbox account password.
#' @param url character, KoboToolbox server URL.
#' @param overwrite logical, if `TRUE`, it overwrites the existing token.
#' Default to `FALSE`.
#'
#' @returns A \code{character}, the KoboToolbox API token. It also stores, as a side effect,
#' the URL and token as the environment variables \code{KOBOTOOLBOX_URL} and
#' \code{KOBOTOOLBOX_TOKEN} respectively.
#'
#' @examples
#' \dontrun{
#' # use your own KoboToolbox URL, username and password
#' if (require(askpass)) {
#'  token <- kobo_setup(username = "cool_user_name",
#'                      password = askpass::askpass(),
#'                      url = "https://kf.kobotoolbox.org/")
#'  token
#'  }
#' }
#'
#' @export
kobo_token <- function(username = NULL, password = NULL,
                       url = NULL, overwrite = FALSE) {
  if (!is.null(url) & !nzchar(Sys.getenv("KOBOTOOLBOX_URL"))) {
    if (!assert_url(url))
    abort(message = "Invalid URL")
    Sys.setenv("KOBOTOOLBOX_URL" = url)
  }

  if (is.null(url) & nzchar(Sys.getenv("KOBOTOOLBOX_URL")))
    url <- Sys.getenv("KOBOTOOLBOX_URL")

  if (nzchar(Sys.getenv("KOBOTOOLBOX_TOKEN")) & !overwrite) {
    token <- Sys.getenv("KOBOTOOLBOX_TOKEN")
  } else {
    url_path <- file.path(url, "token/?format=json")
    cli <- HttpClient$new(url = url_path,
                          auth = auth(user = username,
                                      pwd = password))
    res <- cli$retry("get",
                     times = 3L,
                     retry_only_on = c(500, 503),
                     terminate_on = 404)
    if (res$status_code >= 300)
      abort(error_msg(res$content),
            call = NULL)
    res$raise_for_ct_json()
    res <- fparse(res$parse("UTF-8"))
    token <- res$token
    Sys.setenv("KOBOTOOLBOX_TOKEN" = token)
    Sys.setenv("KOBOTOOLBOX_URL" = url)
  }
  token
}
