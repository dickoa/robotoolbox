#' @importFrom RcppSimdJson fparse
#' @importFrom purrr list_rbind
#' @importFrom dplyr mutate select
#' @importFrom tidyr unnest
#' @importFrom data.table fread
#'
#' @noRd
kobo_audit_ <- function(uid) {
  audit_meta <- get_audit_url_(uid)
  headers <- list(Authorization = paste("Token",
                                        Sys.getenv("KOBOTOOLBOX_TOKEN")))
  reqs <- lapply(audit_meta$download_url, function(url) {
    HttpRequest$new(url,
                    headers = headers)$get()
  })
  sleep <- 0.01
  res <- AsyncQueue$new(.list = reqs,
                        bucket_size = Inf,
                        sleep = sleep)
  res$request()
  cond <- any(res$status_code() > 200L)
  if (cond)
    abort(c("The request failed!",
            "i" = "Please check again your asset `uid`, API token and the server url!"),
          call = NULL)
  res <- res$parse(encoding = "UTF-8")
  res <- mutate(audit_meta, data = lapply(res, \(path) dt2tibble(fread(path))))
  res <- select(res, -"download_url")
  unnest(res, "data") |>
    mutate(name = basename(.data$node), .before = "start",
           start = as.POSIXct(.data$start / 1000, origin = "1970-01-01"),
           end = as.POSIXct(.data$end / 1000, origin = "1970-01-01"))
}

#' Get all audit log data from a KoboToolbox project
#'
#' Get all audit log data from a KoboToolbox project through a \code{\link{kobo_asset}} or asset uid.
#'
#' @rdname kobo_audit
#'
#' @param x a \code{\link{kobo_asset}} or character, the asset
#'
#' @return A data.frame
#'
#' @examples
#' \dontrun{
#' kobo_setup() # setup using your url and token
#' uid <- "a9cwEQcbWqWzdA5eqkjRUWi" # pick a valid uid
#' asset <- kobo_asset(uid)
#' audit <- kobo_audit(asset)
#'
#' if (require(dplyr)) {
#'  library(dplyr)
#'  glimpse(audit)
#'  }
#' }
#'
#' @export
kobo_audit <- function(x)
  UseMethod("kobo_audit")

#' @export
kobo_audit.default <- function(x)
  abort("You need to use a 'kobo_asset' or an asset uid",
        call = NULL)


#' @importFrom purrr list_rbind
#' @importFrom dplyr filter
#' @importFrom rlang abort
#' @export
kobo_audit.kobo_asset <- function(x) {
  asset_version_list <- kobo_asset_version_list(x$uid)
  asset_version_list <- filter(asset_version_list,
                               .data$asset_deployed)
  cond <- nrow(asset_version_list) > 0
  if (cond) {
    version <- unique(asset_version_list$uid)
    form <- lapply(version, \(v) kobo_form(x, v))
    form <- list_rbind(form)
  } else {
    form <- kobo_form(x)
  }

  if (!any("audit" %in% form$name))
    abort("`audit` not enabled in the current version of the survey",
          call = NULL)
  kobo_audit_(x$uid)
}

#' @export
kobo_audit.character <- function(x)
  kobo_audit(kobo_asset(x))
