#' Get a specific KoboToolbox Asset from a uid
#'
#' Get a specific KoboToolbox Asset from a uid
#'
#' @rdname kobo_asset
#'
#' @param x the unique identifier of a specific asset or an asset object
#'
#' @importFrom RcppSimdJson fparse
#'
#' @return a kobo_asset object
#'
#' @examples
#' \dontrun{
#' kobo_setup()
#' asset_list <- kobo_asset_list()
#' uid <- asset_list$uid[1]
#' asset <- kobo_asset(uid)
#' asset
#' }
#' @export
kobo_asset <- function(x)
  UseMethod("kobo_asset")


#' @export
kobo_asset.character <- function(x) {
  path <- paste0("api/v2/assets/", x)
  res <- xget(path = path)
  structure(fparse(res,
                   max_simplify_lvl = "list"),
            class = "kobo_asset")
}

#' @export
kobo_asset.kobo_asset <- function(x) {
  x
}

#' @export
kobo_asset.default <- function(x) {
  stop("You need to use a 'kobo_asset' or an asset uid",
       call. = FALSE)
}

#' List all available KoboToolbox API assets
#'
#' List all available KoboToolbox API assets
#'
#' @importFrom RcppSimdJson fparse
#' @importFrom tibble tibble
#'
#' @return a list of kobo_asset
#'
#' @examples
#' \dontrun{
#' kobo_setup()
#' asset_list <- kobo_asset_list()
#' asset_list
#' }
#'
#' @export
kobo_asset_list <- function() {
  res <- xget(path = "/api/v2/assets",
              args = list(metadata = "on"))
  res <- fparse(res, max_simplify_lvl = "list")
  res <- res$results
  tibble(uid = map_chr2(res, "uid"),
         name = map_chr2(res, "name"),
         asset_type = map_chr2(res, "asset_type"),
         owner_username = map_chr2(res, "owner__username"),
         date_created = parse_kobo_date(map_chr2(res, "date_created")),
         date_modified = parse_kobo_date(map_chr2(res, "date_modified")),
         submissions = map_int2(res, "deployment__submission_count"))
}

#' Get a specific KoboToolbox Asset version from an asset uid or \code{kobo_asset}
#'
#' Get a specific KoboToolbox Asset version from an asset uid or \code{kobo_asset}
#'
#' @rdname kobo_asset_version
#'
#' @param x the unique identifier of a specific asset or an asset object
#' @param version version of the asset
#'
#' @importFrom RcppSimdJson fparse
#'
#' @return a kobo_asset object
#'
#' @examples
#' \dontrun{
#' kobo_setup()
#' asset_list <- kobo_asset_list()
#' uid <- asset_list$uid[1]
#' asset <- kobo_asset(uid)
#' asset_version_list <- kobo_asset_version_list(asset)
#' kobo_asset_version(asset, asset_version_list$uid[1])
#' }
#'
#' @export
kobo_asset_version <- function(x, version)
  UseMethod("kobo_asset_version")

#' @export
kobo_asset_version.character <- function(x, version) {
  path <- paste0("/api/v2/assets/", x,
                 "/versions/", version)
  res <- xget(path = path)
  res <- fparse(res,
                max_simplify_lvl = "list")
  res$asset_uid <- x
  structure(res, class = "kobo_asset_version")
}

#' @export
kobo_asset_version.kobo_asset <- function(x, version) {
  kobo_asset_version.character(x$uid, version = version)
}

#' @export
kobo_asset_version.default <- function(x, version) {
  stop("You need to use a 'kobo_asset' or an asset uid",
       call. = FALSE)
}

#' List all available versions of a KoboToolbox asset
#'
#' List all available versions of a KoboToolbox asset. Works only if you own
#' the project.
#'
#' @rdname kobo_asset_version_list
#'
#' @param x the uid or kobo_asset object
#' @importFrom RcppSimdJson fparse
#' @importFrom tibble tibble
#'
#' @return a list of kobo_asset_version
#'
#' @examples
#' \dontrun{
#' kobo_setup()
#' asset_list <- kobo_asset_list()
#' uid <- asset_list$uid[1]
#' asset <- kobo_asset(uid)
#' kobo_asset_version_list(asset)
#' }
#'
#' @export
kobo_asset_version_list <- function(x)
  UseMethod("kobo_asset_version_list")

#' @export
kobo_asset_version_list.character <- function(x) {
  res <- xget(path = paste0("/api/v2/assets/",
                            x, "/versions/"))
  res <- fparse(res, max_simplify_lvl = "list")
  res <- res$results
  tibble(uid = map_chr2(res, "uid"),
         url = map_chr2(res, "url"),
         asset_deployed = is.na(as.logical(map_chr2(res,
                                                         "date_deployed"))),
         date_modified = as.POSIXct(map_chr2(res, "date_modified")))
}

#' @export
kobo_asset_version_list.kobo_asset <- function(x) {
  kobo_asset_version_list(x$uid)
}

#' @export
kobo_asset_version_list.default <- function(x) {
  stop("You need to use a 'kobo_asset' or an asset uid",
       call. = FALSE)
}

#' @noRd
#' @export
print.kobo_asset <- function(x, ...) {
  cat("<robotoolbox asset> ", x$uid, "\n")
  cat("  Asset name: ", x$name, "\n", sep = "")
  cat("  Asset type: ", x$asset_type, "\n", sep = "")
  cat("  Created: ",
      as.character(parse_kobo_date(x$date_created)),  "\n", sep = "")
  cat("  Last modified: ",
      as.character(parse_kobo_date(x$date_modified)),  "\n", sep = "")
  cat("  Submissions: ", x$deployment__submission_count, "\n", sep = "")
}

#' @noRd
#' @export
print.kobo_asset_version <- function(x, ...) {
  cat("<robotoolbox asset version> ", x$uid, "\n")
  cat("  Asset uid: ", x$asset_uid, "\n", sep = "")
  cat("  Asset deployed: ",
      is.na(as.logical(x$date_deployed)),  "\n", sep = "")
  cat("  Date modified: ",
      as.character(as.POSIXct(x$date_modified)),  "\n", sep = "")
}
