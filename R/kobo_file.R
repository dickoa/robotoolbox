#' List all uploaded files related to a KoboToolbox API asset
#'
#' List all uploaded files related to a KoboToolbox API asset
#'
#' @importFrom tidyr unnest_wider
#'
#' @name kobo_file_list
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
kobo_file_list <- function(x) {
  UseMethod("kobo_file_list")
}

#' @export
kobo_file_list.character <- function(x) {
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
kobo_file_list.kobo_asset <- function(x) {
  kobo_file_list.character(x$uid)
}

#' Download all uploaded files related to a KoboToolbox API asset
#'
#' Download all uploaded files related to a KoboToolbox API asset
#'
#' @importFrom crul Async
#'
#' @name kobo_file_download
#'
#' @param x the asset uid or the \code{kobo_asset} object.
#' @param folder character, the folder where you store the downloaded files.
#' The working directory is the default folder.
#'
#' @returns A vector of file paths.
#'
#' @examples
#' \dontrun{
#' kobo_setup()
#' uid <- "a9cwEQcbWqWzA5hzkjRUWi"
#' kobo_file_download(uid, folder = tempdir())
#' }
#'
#' @export
kobo_file_download <- function(x, folder) {
  UseMethod("kobo_file_download")
}

## #' @export
## kobo_file_download.kobo_asset <- function(x, folder = getwd()) {
##   df <- kobo_data(x)
##   attachments <- df[["_attachments"]]

##   urls <- lapply(attachments,
##                  \(x) data.frame(url = x$download_url,
##                                  fname = x$filename))
##   urls <- list_rbind(urls)
##   urls <- unique(urls)
##   urls$fname <- basename(urls$fname)

##   headers <- list(Authorization = paste("Token",
##                                         Sys.getenv("KOBOTOOLBOX_TOKEN")))

##   cc <- Async$new(urls = urls$url,
##                   headers = headers)
##   cc$get(disk = file.path(folder, urls$fname))
##   ## reqs <- lapply(seq_len(nrow(urls)), function(i) {
##   ##   url <- urls$url[i]
##   ##   fname <- file.path(folder, urls$fname[i])
##   ##   req <- HttpRequest$new(url,
##   ##                          headers = headers)
##   ##   req$retry("get",
##   ##             disk = fname,
##   ##             times = 3L,
##   ##             retry_only_on = c(500, 503),
##   ##             terminate_on = 404)
##   ## })

##   ## res <- AsyncQueue$new(.list = reqs,
##   ##                       bucket_size = Inf,
##   ##                       sleep = 0.1)

##   ## res$request()
##   ## cond <- res$status_code() >= 300L
##   ## if (any(cond)) {
##   ##   msg <- res$content()[cond]
##   ##   abort(error_msg(msg[[1]]),
##   ##         call = NULL)
##   ## }

##   ## res$parse(encoding = "UTF-8")

## }
