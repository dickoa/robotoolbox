#' List all uploaded files related to a KoboToolbox API asset
#'
#' List all uploaded files related to a KoboToolbox API asset
#'
#' @importFrom tidyr unnest_wider
#'
#' @name kobo_asset_file_list
#'
#' @param x the asset uid or the \code{kobo_asset} object.
#'
#' @returns A \code{data.frame} containing the list of all your KoboToolbox API files under
#' the asset:
#' - `uid` the asset unique identifier
#' - `url` url of the files API endpoint
#' - `asset` url of the files associated asset API endpoint
#' - `user` the user account of the owner of the asset
#' - `user__username` when the asset was created
#' - `file_type` files type either `form_media` or `map_layer`
#' - `description` files description
#' - `date_created` date when the files were created
#' - `content` url to download the files
#' - `hash`md5 hash of the files
#' - `filename` names of the files
#' - `mimetype` mime type of the files
#'
#' @examples
#' \dontrun{
#' kobo_setup()
#' uid <- "a9cwEQcbWqWzA5hzkjRUWi"
#' kobo_file_list(uid)
#' }
#'
#' @export
kobo_asset_file_list <- function(x) {
  UseMethod("kobo_asset_file_list")
}

#' @export
kobo_asset_file_list.character <- function(x) {
  if (!assert_uid(x))
    abort(message = "Invalid asset uid")
  path <- paste0("/api/v2/assets/",
                 x,
                 "/files/")
  res <- xget(path = path)
  res <- fparse(res,
                max_simplify_lvl = "data_frame")
  if (res$count <= 0) {
    cnames <- c("uid", "url", "asset",
                "user", "user__username",
                "file_type", "description",
                "date_created", "content",
                "hash", "filename", "mimetype")
    res <- empty_tibble_(cnames)
  } else {
    res <- unnest_wider(res$results,
                        "metadata")
  }
  res
}

#' @export
kobo_asset_file_list.kobo_asset <- function(x) {
  kobo_asset_file_list.character(x$uid)
}
