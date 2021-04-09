#' Get a specific KoBoToolbox asset from a uid
#'
#' Get a specific KoBoToolbox asset from a uid
#'
#' @param uid the unique identifier of a specific asset
#' @return
#' @importFrom jsonlite fromJSON
#' @return a kobo_asset object
#' @export
kobo_asset <- function(uid) {
  path <- paste0("api/v2/assets/", uid)
  res <- xget(path = path)
  structure(fromJSON(res,
                     simplifyVector = FALSE),
            class = "kobo_asset")
}

#' List all available KoBoToolbox API assets
#'
#' List all available KoBoToolbox API assets
#'
#' @return a list of kobo_asset
#' @importFrom jsonlite fromJSON
#' @return a list of kobo_asset
#' @export
kobo_asset_list <- function() {
  res <- xget(path = "/api/v2/assets")
  res <- fromJSON(res,
                  simplifyVector = FALSE)$results
  lapply(res,
         function(x) kobo_asset(x$uid))
}

#' Custom printing for KoBoToolbox API asset
#'
#' Custom printing for KoBoToolbox API  asset
#'
#' @param x (kobo_asset) the asset
#' @noRd
#' @export
print.kobo_asset <- function(x, ...) {
  cat("<KoBoToolbox Asset> ", x$uid, "\n")
  cat("   Name: ", x$name, "\n", sep = "")
  cat("   Type: ", x$asset_type, "\n", sep = "")
  cat("   Created: ",
      parse_kobo_date(x$date_created),  "\n", sep = "")
  cat("   Last modified: ",
      parse_kobo_date(x$date_modified),  "\n", sep = "")
}
