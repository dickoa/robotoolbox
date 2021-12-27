#' Get a specific KoBoToolbox Asset from a uid
#'
#' Get a specific KoBoToolbox Asset from a uid
#'
#' @param uid the unique identifier of a specific asset
#'
#' @importFrom RcppSimdJson fparse
#'
#' @return a kobo_asset object
#'
#' @export
kobo_asset <- function(uid) {
  path <- paste0("api/v2/assets/", uid)
  res <- xget(path = path)
  structure(fparse(res,
                   max_simplify_lvl = "list"),
            class = "kobo_asset")
}

#' List all available KoBoToolbox API assets
#'
#' List all available KoBoToolbox API assets
#'
#' @importFrom RcppSimdJson fparse
#' @importFrom tibble tibble
#'
#' @return a list of kobo_asset
#'
#' @export
kobo_asset_list <- function() {
  res <- xget(path = "/api/v2/assets",
              args = list(metadata = "on"))
  res <- fparse(res, max_simplify_lvl = "list")
  res <- res$results
  tibble(uid = map_character(res, "uid"),
         name = map_character(res, "name"),
         asset_type = map_character(res, "asset_type"),
         owner_username = map_character(res, "owner__username"),
         date_created = parse_kobo_date(map_character(res, "date_created")),
         date_modified = parse_kobo_date(map_character(res, "date_modified")),
         submissions = map_integer(res, "deployment__submission_count"))
}

#' @noRd
#' @export
print.kobo_asset <- function(x, ...) {
  cat("<robotoolbox asset> ", x$uid, "\n")
  cat("  Asset Name: ", x$name, "\n", sep = "")
  cat("  Asset Type: ", x$asset_type, "\n", sep = "")
  cat("  Created: ",
      as.character(parse_kobo_date(x$date_created)),  "\n", sep = "")
  cat("  Last modified: ",
      as.character(parse_kobo_date(x$date_modified)),  "\n", sep = "")
  cat("  Submissions: ", x$deployment__submission_count, "\n", sep = "")
}
