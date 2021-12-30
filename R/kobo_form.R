#' @noRd
kobo_form_lang <- function(asset) {
  lng <- asset$content$translations
  if (sum(lengths(lng)) <= 0)
    lng <- "Labels"
  lng
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
#' @importFrom stringi stri_trans_general
#' @importFrom stats setNames
#' @importFrom tidyselect contains everything
#' @importFrom rlang .data
#'
#' @return tbl_form, the project form
#'
#' @export
kobo_form.kobo_asset <- function(asset) {

  form_lang <- function(x, lang) {
    ss <- sum(lengths(x$label))
    x$lang <- lang[seq.int(ss)]
    x
  }

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
                   list_name = contains("select_from_list_name"),
                   type = "type",
                   label = "label",
                   lang = "lang",
                   everything(),
                   -.data$name)
  survey <- setNames(unnest(survey,
                            cols = is_list_cols(survey),
                            keep_empty = TRUE),
                     gsub("^\\$", "", names(survey)))
  survey$lang[is.na(survey$lang)] <- "Labels"
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
    choices$value_label <- stri_trans_general(choices$value_label,
                                              id = "Latin-ASCII")
    choices <- setNames(unnest(choices,
                               cols = is_list_cols(choices),
                               keep_empty = TRUE),
                        gsub("^\\$", "", names(choices)))
    choices$value_lang[is.na(choices$value_lang)] <- "Labels"
    form <- nest_join(survey, choices,
                      by = "list_name")
  } else {
    form <- survey
  }
  form$name <- stri_trans_general(form$name, "Latin-ASCII")
  form
}
