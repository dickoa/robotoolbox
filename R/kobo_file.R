#' List all uploaded files related to a KoboToolbox API asset
#'
#' List all uploaded files related to a KoboToolbox API asset
#'
#' @importFrom tidyr unnest_wider
#' @importFrom purrr list_rbind
#'
#' @name kobo_asset_file_list
#'
#' @param x the asset uid or the \code{kobo_asset} object.
#'
#' @returns A \code{data.frame} containing the list of all your KoboToolbox API files under
#' the asset:
#' - `uid` the file unique identifier
#' - `url` URL of the file in the API
#' - `asset` URL of the asset the file belongs to
#' - `user` URL of the account that uploaded the file
#' - `user__username` username of the account that uploaded the file
#' - `file_type` file type, either `form_media` or `map_layer`
#' - `description` file description
#' - `date_created` date when the file was uploaded
#' - `content` URL to download the file
#' - `hash` MD5 hash of the file, prefixed with `md5:`
#' - `filename` name of the file
#' - `mimetype` MIME type of the file
#'
#' @examples
#' \dontrun{
#' kobo_setup()
#' uid <- "a9cwEQcbWqWzA5hzkjRUWi"
#' kobo_asset_file_list(uid)
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
  pages <- xget_all_(path, simplify = "data_frame")
  if (length(pages) == 0L) {
    cnames <- c("uid", "url", "asset",
                "user", "user__username",
                "file_type", "description",
                "date_created", "content",
                "hash", "filename", "mimetype")
    res <- empty_tibble_(cnames)
  } else {
    res <- unnest_wider(list_rbind(pages),
                        "metadata")
  }
  res
}

#' @export
kobo_asset_file_list.kobo_asset <- function(x) {
  kobo_asset_file_list.character(x$uid)
}

#' @export
kobo_asset_file_list.default <- function(x) {
  abort("You need to use a 'kobo_asset' or a valid asset uid")
}
