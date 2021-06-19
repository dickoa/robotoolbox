#' @importFrom readr type_convert
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#' @noRd
format_kobo_submissions <- function(x) {
  res <- setNames(type_convert(x),
                  basename(names(x)))
  as_tibble(res,
            .name_repair = "unique")
}

#' @export
#' @rdname kobo_submissions
kobo_submissions <- function(asset)
  UseMethod("kobo_submissions")

#' Get all submissions from a project
#'
#' Get all submissions from a project
#'
#' @rdname kobo_submissions
#'
#' @importFrom jsonlite fromJSON
#' @importFrom labelled var_label<-
#'
#' @param asset kobo_asset, the asset
#' @return data.frame, all submissions
#' @export
kobo_submissions.kobo_asset <- function(asset) {
  path <- paste0("api/v2/assets/", asset$uid, "/data.json")
  res <- xget(path = path)
  res <- fromJSON(res,
                  simplifyVector = TRUE)
  subs <- format_kobo_submissions(res)
  labels <- kobo_form_to_list(kobo_form(asset))
  labels <- drop_nulls(labels[names(subs)])
  var_label(subs) <- labels
  subs
}


## path <- paste0("api/v2/assets/", "a7kgvVN75RQCmo4yyZEUEp", "/data.json")
## res <- xget(path = path)
## res <- fromJSON(res, simplifyVector = TRUE)
## str(res)
