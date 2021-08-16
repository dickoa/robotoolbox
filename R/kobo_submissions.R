#' @importFrom readr type_convert
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#' @noRd
format_kobo_submissions <- function(x) {
  res <- setNames(type_convert(x),
                  clean_subs_colnames(names(x)))
  as_tibble(res,
            .name_repair = "universal")
}

#' @export
#' @rdname kobo_submissions
kobo_submissions <- function(asset, ...)
  UseMethod("kobo_submissions")

#' Get all submissions from a project
#'
#' Get all submissions from a project
#'
#' @rdname kobo_submissions
#'
#' @importFrom jsonlite fromJSON
#' @importFrom labelled var_label<-
#' @importFrom dplyr select relocate
#' @importFrom tidyselect starts_with any_of all_of last_col
#' @importFrom tidyr unnest
#'
#' @param asset kobo_asset, the asset
#' @param group_names logical, keep the group names as prefix
#' @return data.frame, all submissions
#' @export
kobo_submissions.kobo_asset <- function(asset, group_names = TRUE) {
  path <- paste0("api/v2/assets/", asset$uid, "/data.json")
  res <- xget(path = path)
  res <- fromJSON(res,
                  simplifyVector = TRUE)
  subs <- res$results
  form <- kobo_form(asset)
  is_repeat <- form$survey$type %in% "begin_repeat"
  if (any(is_repeat)) {
    cols_to_unnest <- form$survey$name[is_repeat]
    subs <- unnest(subs, cols = cols_to_unnest)
  }
  subs <- format_kobo_submissions(subs)
  labels <- kobo_form_to_list(kobo_form(asset))
  labels <- drop_nulls(labels[names(subs)])
  var_label(subs) <- labels
  subs <- select(subs,
                 -any_of(c("_attachments", "imei",
                           "formhub_uuid", "meta_instanceid",
                           "meta_deprecatedid", "subscriberid")))
  subs <- relocate(subs,
                   starts_with("_"),
                   .after = last_col())
  subs <- relocate(subs,
                   any_of(c("start", "end", "today",
                            "username", "simserial",
                            "deviceid", "phonenumber", "audit")),
                   .before = 1L)
  subs
}

## path <- paste0("api/v2/assets/", "a7kgvVN75RQCmo4yyZEUEp", "/data.json")
## res <- xget(path = path)
## res <- fromJSON(res, simplifyVector = TRUE)
## str(res)
