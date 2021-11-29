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
#' @importFrom dplyr select mutate across
#' @importFrom stringr str_replace_all
#' @importFrom janitor make_clean_names
#'
#' @param asset kobo_asset, the asset
#' @param group_names logical, keep the group names as prefix
#' @param value_label logical, use label instead of XML values
#' @param ... extra parameters for the KPI data endpoints e.g limit, start
#' @return data.frame, all submissions with `select_one` being labelled columns
#' @export
kobo_submissions.kobo_asset <- function(asset, group_names = FALSE, value_label = TRUE, ...) {
  path <- paste0("api/v2/assets/", asset$uid, "/data.json")
  res <- xget(path = path)
  ## res <- xget(path = path, ...)
  res <- fromJSON(res,
                  simplifyVector = TRUE)
  subs <- res$results
  form <- kobo_form(asset)

  subs <- format_kobo_submissions(subs,
                                  group_names = group_names)

  is_select_o <- grepl("^select_one", form$type)
  is_select_m <- grepl("^select_multiple", form$type)
  is_select <- is_select_o | is_select_m
  is_search <- FALSE
  if ("appearance" %in% names(form))
    is_search <- grepl("^search", form$appearance)
  lang <- kobo_form_lang(asset)[[1]]

  if (isTRUE(value_label) & any(is_select_o)) {
    form_o <- form[is_select_o & form$lang %in% lang & !is_search, ]
    choices_o <- form_o$choices
    choices_o <- lapply(choices_o, function(ch) {
      ch <- ch[ch$value_lang %in% lang, 2:1]
      ch <- deframe(ch)
      ch
    })
    select_o <- form_o$name
    names(choices_o) <- select_o
    select_o <- intersect(names(subs), select_o)
    subs <- mutate(subs, across(select_o, as.character))
    val_labels(subs) <- choices_o[select_o]
  }

  ## if (isTRUE(value_label) & any(is_select_m)) {
  ##   form_m <- form[is_select_m & form$lang %in% lang & !is_search, ]
  ##   choices_m <- form_m$choices
  ##   choices_m <- lapply(choices_m, function(ch) {
  ##     ch <- ch[ch$value_lang %in% lang, ]
  ##     ch$value_label <- janitor::make_clean_names(ch$value_label)
  ##     setNames(ch$value_label, paste0("^", ch$value_name, "$"))
  ##   })
  ##   select_m <- form_m$name
  ##   names(choices_m) <- select_m
  ##   select_m <- intersect(names(subs), select_m)
  ##   subs <- mutate(subs,
  ##                  across(.cols = select_m,
  ##                         .fns = ~ str_replace_all(.x,
  ##                                                   choices_m[[cur_column()]])))
  ## }

  is_repeat <- form$type %in% "begin_repeat"
  if (any(is_repeat)) {
    repeat_cols <- unique(form$name[is_repeat])
    repeat_cols <- intersect(names(subs), repeat_cols)
    for (cols in repeat_cols)
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

  is_geopoint <- form$type %in% "geopoint"
  if (any(is_geopoint)) {
    nm <- unique(form$name[is_geopoint])
    nm <- intersect(names(subs), nm)
    subs <- geopoint_kobo_submissions(subs,
                                      nm)
  }

  subs
}
