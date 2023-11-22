#' Get a specific KoboToolbox API asset from a unique identifier
#'
#' Get a specific KoboToolbox API asset from a unique identifier
#'
#' @name kobo_asset
#'
#' @importFrom RcppSimdJson fparse
#'
#' @param x the unique identifier of a specific asset (`character`) or
#' a \code{kobo_asset} object.
#'
#' @returns A \code{kobo_asset} object. It contains all the information about the
#' KoboToolbox API asset associated to the unique identifier.
#'
#' @examples
#' \dontrun{
#' # replace by your own url and token
#' kobo_setup(url = "https://kf.kobotoolbox.org", token = "abcde")
#' # use a valid uid
#' uid <- "a9cwEQcbWqWzA5hzkjRUWi"
#' asset <- kobo_asset(uid)
#' asset
#' }
#' @export
kobo_asset <- function(x) {
  UseMethod("kobo_asset")
}

#' @export
kobo_asset.character <- function(x) {
  if (!assert_uid(x))
    abort(message = "Invalid asset uid")
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
  abort("You need to use a 'kobo_asset' or a valid asset uid")
}

#' List all available KoboToolbox API assets
#'
#' List all available KoboToolbox API assets and their metadata.
#'
#' @name kobo_asset_list
#'
#' @param limit integer, the number of API assets to display per page. Default to 100.
#'
#' @returns A \code{data.frame} containing the list of all your KoboToolbox API assets
#' and the following metadata:
#' - `uid` the asset unique identifier
#' - `name` the name of the asset
#' - `asset_type` the type of asset (`block`, `survey`, `question`, or `template`)
#' - `owner_userame` the user account of the owner of the asset
#' - `date_create` when the asset was created
#' - `date_modified` when the asset was last modified
#' - `deployed` whether or not the asset is currently deployed
#' - `submissions` the number of submissions for the asset (`survey`)
#'
#' @examples
#' \dontrun{
#' kobo_setup()
#' asset_list <- kobo_asset_list(limit = 10L)
#' asset_list
#' }
#'
#' @export
kobo_asset_list <- function(limit = 100L) {
  get_asset_list_paginate(limit = limit)
}

#' Get a specific KoboToolbox API asset version from an asset unique identifier
#'
#' Get a specific KoboToolbox Asset version from an asset unique identifier
#' or \code{kobo_asset} object
#'
#' @name kobo_asset_version
#'
#' @param x the unique identifier of a specific asset (`character`) or
#' a \code{kobo_asset} object.
#' @param version character, the unique identifier of the version of the asset
#'
#' @importFrom RcppSimdJson fparse
#'
#' @returns A \code{kobo_asset_version} object
#'
#' @examples
#' \dontrun{
#' kobo_setup()
#' uid <- "a9cwEQcbWqWzA5hzkjRUWi"
#' asset <- kobo_asset(uid)
#' asset_version_list <- kobo_asset_version_list(asset)
#' kobo_asset_version(asset, asset_version_list$uid[1])
#' }
#'
#' @export
kobo_asset_version <- function(x, version) {
  UseMethod("kobo_asset_version")
}

#' @export
kobo_asset_version.character <- function(x, version) {
  if (!assert_uid(x))
    abort(message = "Invalid asset uid")

  if (!is_zero_length_or_null(version) && !assert_version_uid(version))
    abort(message = "Invalid asset version uid")

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
  abort("You need to use a 'kobo_asset' or a valid asset uid")
}

#' List all available versions of a KoboToolbox API asset
#'
#' List all available versions of a KoboToolbox API asset and their metadata.
#'
#' @name kobo_asset_version_list
#'
#' @param x the uid or \code{kobo_asset} object.
#' @importFrom RcppSimdJson fparse
#' @importFrom tibble tibble
#'
#' @returns A \code{data.frame} containing the list of all the versions
#' of a given KoboToolbox API asset with the following metadata:
#' - `uid` the asset version unique identifier.
#' - `url` the URL of the asset version.
#' - `deployed` whether or not the asset version is deployed
#' - `date_modified` when the asset version was last modified

#' @returns a \code{data.frame}
#'
#' @examples
#' \dontrun{
#' kobo_setup() # setup using your url and token
#' uid <- "a9cwEQcbWqWzA5hzkjRUWi" # pick a valid uid
#' asset <- kobo_asset(uid)
#' kobo_asset_version_list(asset)
#' }
#'
#' @export
kobo_asset_version_list <- function(x) {
  UseMethod("kobo_asset_version_list")
}

#' @export
kobo_asset_version_list.character <- function(x) {
  if (!assert_uid(x))
    abort(message = "Invalid asset uid")
  res <- xget(path = paste0("/api/v2/assets/",
                            x, "/versions/"))
  res <- fparse(res, max_simplify_lvl = "list")
  res <- res$results
  tibble(uid = map_chr2(res, "uid"),
         url = map_chr2(res, "url"),
         deployed = is.na(as.logical(map_chr2(res,
                                              "date_deployed"))),
         date_modified = parse_kobo_datetime(map_chr2(res, "date_modified")))
}

#' @export
kobo_asset_version_list.kobo_asset <- function(x) {
  kobo_asset_version_list(x$uid)
}

#' @export
kobo_asset_version_list.default <- function(x) {
  abort("You need to use a 'kobo_asset' or an asset uid")
}

#' @noRd
#' @export
print.kobo_asset <- function(x, ...) {
  cat("<robotoolbox asset> ", x$uid, "\n")
  cat("  Asset name: ", x$name, "\n", sep = "")
  cat("  Asset type: ", x$asset_type, "\n", sep = "")
  cat("  Asset owner: ", x$owner__username, "\n", sep = "")
  cat("  Created: ",
      parse_kobo_datetime(x$date_created), "\n", sep = "")
  cat("  Last modified: ",
      parse_kobo_datetime(x$date_modified), "\n", sep = "")
  cat("  Submissions: ", x$deployment__submission_count, "\n", sep = "")
}

#' @noRd
#' @export
print.kobo_asset_version <- function(x, ...) {
  cat("<robotoolbox asset version> ", x$uid, "\n")
  cat("  Asset uid: ", x$asset_uid, "\n", sep = "")
  cat("  Asset deployed: ",
      is.na(as.logical(x$date_deployed)), "\n", sep = "")
  cat("  Date modified: ",
      parse_kobo_datetime(x$date_modified), "\n", sep = "")
}
