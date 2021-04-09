#' @noRd
kobo_submissions <- function(x)
  UseMethod("kobo_submissions")

#' Get all submissions from a project
#'
#' Get all submissions from a project
#'
#' @importFrom jsonlite fromJSON
#'
#' @param asset kobo_asset, the asset
#' @return data.frame, all submissions
#' @export
kobo_submissions.kobo_asset <- function(asset) {
  path <- paste0("assets/", asset$uid, "/submissions")
  res <- xget(path = path)
  d <- fromJSON(res,
                simplifyVector = TRUE)
  postprocess_submission(d)
}
