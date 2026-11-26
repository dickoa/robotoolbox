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
#' Defaults to `FALSE`. The token of the session is returned as it is only
#' when `url` is the server of the session. For another server, a new token
#' is requested, and the URL and the token of the session change together
#' once the request succeeds.
#'
#' @returns A `character`, the KoboToolbox API token. It also stores, as a side effect,
#' the URL and token as the environment variables `KOBOTOOLBOX_URL` and
#' `KOBOTOOLBOX_TOKEN` respectively.
#'
#' @family setup functions
#'
#' @examples
#' \dontrun{
#' # use your own KoboToolbox URL, username and password
#' if (requireNamespace("askpass", quietly = TRUE)) {
#'  token <- kobo_token(username = "cool_user_name",
#'                      password = askpass::askpass(),
#'                      url = "https://kf.kobotoolbox.org")
#'  token
#'  }
#' }
#'
#' @export
kobo_token <- function(username = NULL, password = NULL,
                       url = NULL, overwrite = FALSE) {
  if (is.null(url)) {
    url <- Sys.getenv("KOBOTOOLBOX_URL", "")
    if (!nzchar(url)) {
      abort(
        c("No KoboToolbox server URL provided.",
          i = "Pass `url` or set the KOBOTOOLBOX_URL environment variable."),
        call = NULL
      )
    }
    url <- clean_urls(url)
  } else {
    if (!assert_url(url))
      abort("Invalid URL", call = NULL)
    url <- clean_urls(url)
  }

  ## A token is reused only for the server it was set with
  same_server <- identical(url, server_url_())
  if (nzchar(Sys.getenv("KOBOTOOLBOX_TOKEN")) && !overwrite && same_server) {
    token <- Sys.getenv("KOBOTOOLBOX_TOKEN")
    Sys.setenv("KOBOTOOLBOX_URL" = url)
  } else {
    url_path <- paste0(url, "/token/?format=json")
    cli <- HttpClient$new(url = url_path,
                          headers = list(`User-Agent` = user_agent_()),
                          auth = auth(user = username,
                                      pwd = password))
    res <- cli$retry("get",
                     times = 3L,
                     retry_only_on = retry_codes_,
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
