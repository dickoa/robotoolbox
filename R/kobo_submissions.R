#' @noRd
#' @importFrom readr type_convert col_character
#' @importFrom tibble as_tibble
#' @importFrom stats setNames
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
    res <- suppressMessages(type_convert(x))
    res <- setNames(res,
                    new_names)
    res <- suppressMessages(as_tibble(res,
                                      .name_repair = "universal"))
    x <- res
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
kobo_submissions <- function(asset, group_names, value_label)
  UseMethod("kobo_submissions")

#' Get all submissions from a project
#'
#' Get all submissions from a project
#'
#' @rdname kobo_submissions
#'
#' @importFrom jsonlite fromJSON
#' @importFrom labelled val_labels<-
#' @importFrom tibble deframe
#' @importFrom dplyr select
#' @importFrom mgsub mgsub
#' @importFrom tidyr unnest
#' @importFrom janitor clean_names
#'
#' @param asset kobo_asset, the asset
#' @param group_names logical, keep the group names as prefix
#' @param value_label logical, use label instead of XML values
#' @param ... extra parameters for the KPI data endpoints e.g limit, start
#' @return data.frame, all submissions with `select_one` being labelled columns
#' @export
kobo_submissions.kobo_asset <- function(asset, group_names = FALSE, value_label = TRUE, ...) {
  path <- paste0("api/v2/assets/", asset$uid, "/data.json")
  res <- xget(path = path, ...)
  res <- fromJSON(res,
                  simplifyVector = TRUE)
  subs <- res$results
  form <- kobo_form(asset)

  subs <- format_kobo_submissions(subs,
                                  group_names = group_names)

  is_select_o <- grepl("^select_one", form$type)
  is_select_m <- grepl("^select_multiple", form$type)
  is_select <- is_select_o | is_select_m
  is_search <- grepl("^search", form$appearance)
  lang <- kobo_form_lang(asset)[[1]]

  if (isTRUE(value_label) & any(is_select_o)) {
    form_o <- form[is_select_o & form$lang %in% lang & !is_search, ]
    choices_o <- form_o$choices
    choices_o <- lapply(choices_o, function(ch) {
      ch <- ch[ch$value_lang == lang, 2:1]
      ch <- deframe(ch)
      ch
    })
    select_o <- form_o$name
    names(choices_o) <- select_o
    select_o <- intersect(names(subs), select_o)
    for (cols in select_o) {
      subs[[cols]] <- as.character(subs[[cols]])
      val_labels(subs[[cols]]) <- choices_o[[cols]]
    }
  }

  if (isTRUE(value_label) & any(is_select_m)) {
    form_m <- form[is_select_m & form$lang %in% lang & !is_search, ]
    choices_m <- form_m$choices
    choices_m <- lapply(choices_m, function(ch) {
      ch <- ch[ch$value_lang == lang, ]
      ch
    })
    select_m <- form_m$name
    names(choices_m) <- select_m
    select_m <- intersect(names(subs), select_m)
    for (cols in select_m) {
      ch <- choices_m[[cols]]
      subs[[cols]] <- mgsub(as.character(subs[[cols]]),
                            pattern = ch$value_name,
                            replacement = ch$value_label)
    }
  }

  is_repeat <- form$type %in% "begin_repeat"
  if (any(is_repeat)) {
    cols_to_unnest <- unique(form$name[is_repeat])
    cols_to_unnest <- intersect(names(subs), cols_to_unnest)
    for (cols in cols_to_unnest)
      subs[[cols]] <- lapply(subs[[cols]], function(df) {
        format_kobo_submissions(df,
                                group_names = group_names)
      })
  }

  is_multiple_choice <- form$type %in% "select_multiple"
  if (any(is_multiple_choice)) {
    nm <- unique(form$name[is_multiple_choice])
    nm <- intersect(names(subs), nm)
    subs <- dummify_kobo_submissions(subs,
                                     nm)
  }
  subs
}
