#' @noRd
kobo_form_to_list <- function(x, key) {
  x <- x$survey[!x$survey$type %in% c("begin_group", "end_group", "note"), ]
  nm <- x$name
  setNames(as.list(x[[key]]),
           nm)
}

#' @noRd
kobo_form_lang <- function(asset) {
  asset_content_nm <- names(asset$content)
  if ("translations" %in% asset_content_nm)
    asset$content$translations
}

#' @export
#' @rdname kobo_form
kobo_form <- function(asset)
  UseMethod("kobo_form")

#' Get the form used for this asset
#'
#' Get the form used for this asset
#'
#' @param asset kobo_asset, the asset
#'
#' @rdname kobo_form
#'
#' @importFrom data.table rbindlist
#' @importFrom dplyr select nest_join
#' @importFrom tibble as_tibble new_tibble
#' @importFrom tidyr unnest drop_na
#' @return tbl_form, the project form
#'
#' @export
kobo_form.kobo_asset <- function(asset) {
  asset_content_nm <- names(asset$content)
  if ("translations" %in% asset_content_nm)
    lang <- asset$content$translations
  survey <- lapply(asset$content$survey, function(l) {
    x <- form_lang(l, lang)
    x
  })
  survey <- rbindlist(survey, fill = TRUE)
  survey <- select(.data = survey,
                   name = "$autoname",
                   list_name = "select_from_list_name",
                   type = "type",
                   label = "label",
                   lang = "lang",
                   contains("appearance"),
                   contains("calculation"))
  survey <- setNames(unnest(survey,
                            cols = is_list_cols(survey),
                            keep_empty = TRUE),
                     gsub("^\\$", "", names(survey)))
  is.na(survey$lang) <- is.na(survey$label)
  survey <- drop_na(survey, "name")
  if ("choices" %in% asset_content_nm) {
    choices <- lapply(asset$content$choices, function(l) {
      x <- form_lang(l, lang)
      x
    })
    choices <- rbindlist(choices, fill = TRUE)
    choices <- select(.data = choices,
                      list_name = "list_name",
                      value_name = "$autovalue",
                      value_label = "label",
                      value_lang = "lang")
    choices <- drop_na(choices, "value_label")
    choices <- setNames(unnest(choices,
                               cols = is_list_cols(choices),
                               keep_empty = TRUE),
                        gsub("^\\$", "", names(choices)))
    form <- nest_join(survey, choices, by = "list_name")
  } else {
    form <- survey
  }
  form$name <- iconv(form$name, to = "ASCII//TRANSLIT")
  new_tibble(form, class = "tbl_form")
}


#' @noRd
#' @importFrom tibble tbl_sum
#' @importFrom stats na.omit
#' @export
tbl_sum.tbl_form <- function(x, ...) {
  list_of_type <- c("decimal", "range", "text", "integer",
                    "select_one", "select_multiple",
                    "select_one_from_file",
                    "select_multiple_from_file",
                    "rank", "note", "geopoint", "geotrace",
                    "geoshape", "date", "time", "dateTime",
                    "image", "audio", "background-audio", "video", "file",
                    "barcode", "calculate", "acknowledge","hidden", "xml-external")
  lang <- unique(na.omit(x$lang))[1]
  n_q <- nrow(x[x$type %in% list_of_type & x$lang %in% lang, , drop = FALSE])
  default <- NextMethod()
  c("A robotoolbox form" = paste(n_q, "questions"), default)
}
