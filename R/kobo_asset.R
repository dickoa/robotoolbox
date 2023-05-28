#' Get a specific KoboToolbox Asset from a uid
#'
#' Get a specific KoboToolbox Asset from a uid
#'
#' @rdname kobo_asset
#'
#' @importFrom RcppSimdJson fparse
#'
#' @param x the unique identifier of a specific asset or a \code{kobo_asset} object.
#'
#' @details \code{\link{kobo_asset}} allows you to access any `Kobotoolbox` API asset.
#' Assets can include a range of types, such as questions, blocks, surveys, templates, and collections.
#'
#' @return a \code{kobo_asset} object
#'
#' @examples
#' \dontrun{
#' kobo_setup() # setup using your url and token
#' uid <- "a9cwEQcbWqWzA5hzkjRUWi" # pick a valid uid
#' asset <- kobo_asset(uid)
#' asset
#' }
#' @export
kobo_asset <- function(x)
  UseMethod("kobo_asset")

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
#' @importFrom RcppSimdJson fparse
#' @importFrom tibble tibble
#'
#' @param limit integer, the number of project to display per page. Default to 100.
#'
#' @details This function list all the assets (projects) in your Kobotoolbox account with metadata such as `uid`, `name`, `asset_type`, `owner_username`, `date_created`, `date_modified` and `submissions`.
#'
#' @return a \code{data.frame}
#'
#' @examples
#' \dontrun{
#' kobo_setup()
#' asset_list <- kobo_asset_list()
#' asset_list
#' }
#'
#' @export
kobo_asset_list <- function(limit = 100L) {
  res <- xget(path = "/api/v2/assets",
              args = list(metadata = "on", limit = limit))
  res <- fparse(res, max_simplify_lvl = "list")
  cnt <- res$count
  if (cnt > limit) {
    res <- get_asset_list_async(limit, cnt)
  } else {
    res <- asset_list_to_tbl(res$results)
  }
  res
}

#' Get a specific KoboToolbox Asset version from an asset uid or \code{\link{kobo_asset}}
#'
#' Get a specific KoboToolbox Asset version from an asset uid or \code{\link{kobo_asset}}
#'
#' @rdname kobo_asset_version
#'
#' @param x the unique identifier of a specific asset or a `kobo_asset` object
#' @param version version of the asset
#'
#' @importFrom RcppSimdJson fparse
#'
#' @details Get a specific version of your \code{kobo_asset}.
#'
#' @return a \code{kobo_asset_version} object
#'
#' @examples
#' \dontrun{
#' kobo_setup() # setup using your url and token
#' uid <- "a9cwEQcbWqWzA5hzkjRUWi" # pick a valid uid
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
  if (!assert_uid(x))
    abort(message = "Invalid asset uid")

  if (!assert_version_uid(version))
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

#' List all available versions of a KoboToolbox asset
#'
#' List all available versions of a KoboToolbox asset and their metadata.
#'
#' @rdname kobo_asset_version_list
#'
#' @param x the uid or \code{kobo_asset} object.
#' @importFrom RcppSimdJson fparse
#' @importFrom tibble tibble
#'
#' @details \code{kobo_asset_version_list} create a table with all the versions of the asset,
#' and whether or not they're deployed.
#'
#' @return a \code{data.frame}
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
kobo_asset_version_list <- function(x)
  UseMethod("kobo_asset_version_list")

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
      parse_kobo_datetime_simple(x$date_modified), "\n", sep = "")
}
