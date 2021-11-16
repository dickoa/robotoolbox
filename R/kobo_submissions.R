#' @noRd
#' @importFrom readr type_convert col_character
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
#' @importFrom janitor clean_names
#' @importFrom dplyr select
#' @importFrom tidyselect contains matches
format_kobo_submissions <- function(x, group_names = FALSE) {
  if (!is.null(x)) {
    x <- select(x, -contains(c("instanceid", "xform", "formhub",
                               "deprecatedid", "subscriberid",
                               "imei", "_attachments"),
                             ignore.case = TRUE),
                -matches("^_version_$", perl = TRUE, ignore.case = TRUE))
    new_names <- clean_subs_colnames(names(x),
                                     group_names = group_names)
    ## res <- suppressMessages(type_convert(x,
    ##                                      col_types = list(.default = col_character())))
    res <- suppressMessages(type_convert(x))
    res <- setNames(res,
                    new_names)
    res <- suppressMessages(as_tibble(res,
                                      .name_repair = "universal"))
    x <- clean_names(res)
  }
  x
}

#' @importFrom fastDummies dummy_cols
#' @noRd
dummify_kobo_submissions <- function(x, cols, split = " ",
                                     ...) {
  dummy_cols(x, select_columns = cols,
             split = split, ...)
}

#' @export
#' @rdname kobo_submissions
kobo_submissions <- function(asset, group_names)
  UseMethod("kobo_submissions")

#' Get all submissions from a project
#'
#' Get all submissions from a project
#'
#' @rdname kobo_submissions
#'
#' @importFrom jsonlite fromJSON
#' @importFrom labelled var_label<-
#' @importFrom dplyr select
#' @importFrom tidyselect starts_with any_of all_of last_col
#' @importFrom tidyr unnest
#'
#' @param asset kobo_asset, the asset
#' @param group_names logical, keep the group names as prefix
#' @param ... extra parameters for the KPI data endpoints e.g limit, start
#' @return data.frame, all submissions
#' @export
kobo_submissions.kobo_asset <- function(asset, group_names = FALSE, ...) {
  path <- paste0("api/v2/assets/", asset$uid, "/data.json")
  res <- xget(path = path)
  ##res <- xget(path = path, ...)
  res <- fromJSON(res,
                  simplifyVector = TRUE)
  subs <- res$results
  form <- kobo_form(asset)

  subs <- format_kobo_submissions(subs,
                                  group_names = group_names)

  is_repeat <- form$type %in% "begin_repeat"
  if (any(is_repeat)) {
    cols_to_unnest <- unique(tolower(form$name[is_repeat]))
    cols_to_unnest <- intersect(names(subs), cols_to_unnest)
    for (cols in cols_to_unnest)
      subs[[cols]] <- lapply(subs[[cols]], function(df) {
        format_kobo_submissions(df,
                                group_names = group_names)
      })
  }

  is_multiple_choice <- form$type %in% "select_multiple"
  if (any(is_multiple_choice)) {
    nm <- form$name[is_multiple_choice]
    nm <- intersect(names(subs), nm)
    subs <- dummify_kobo_submissions(subs,
                                     nm)
  }

  ## labels <- kobo_form_to_list(kobo_form(asset), "label")
  ## labels <- drop_nulls(labels[names(subs)])
  ## .var_label(subs) <- labels
  ## qtype <- kobo_form_to_list(kobo_form(asset), "type")
  ## qtype <- drop_nulls(qtype[names(subs)])
  ## .var_qtype(subs) <- labels
  ## subs <- select(subs,
  ##                -any_of(c("_attachments", "imei",
  ##                          "formhub_uuid", "meta_instanceid",
  ##                          "meta_deprecatedid", "subscriberid")))
  ## subs <- relocate(subs,
  ##                  any_of(c("start", "end", "today",
  ##                           "username", "simserial",
  ##                           "deviceid", "phonenumber", "audit")),
  ##                  .before = 1L)
  ## subs <- relocate(subs,
  ##                  starts_with("\\._"),
  ##                  .after = last_col())
  subs
}
