#' @noRd
kobo_form <- function(x)
  UseMethod("kobo_form")

#' Get the form used for this asset
#'
#' Get the form used for this asset
#'
#' @param asset kobo_asset, the asset
#'
#' @importFrom dplyr bind_rows
#' @importFrom tidyr unnest
#' @return kobo_form, the project form
#' @export
kobo_form.kobo_asset <- function(asset) {
  survey <- bind_rows(asset$content$survey)
  survey <- setNames(unnest(survey, cols = is_list_cols(survey)),
                     gsub("^\\$", "", names(survey)))
  choices <- bind_rows(asset$content$choices)
  choices <- setNames(unnest(choices, cols = is_list_cols(choices)),
                      gsub("^\\$", "", names(choices)))
  structure(list(survey = survey,
                 choices = choices,
                 asset = asset),
            class = "kobo_form")
}

#' Custom printing for KoBoToolbox form
#'
#' Custom printing for KoBoToolbox form
#'
#' @param x (kobo_form) the project form
#' @noRd
#' @export
print.kobo_form <- function(x, ...) {
  cat("<KoBoToolbox Form> ", x$asset$uid, "\n")
  cat("   Asset name: ", x$asset$name, "\n", sep = "")
  cat("   Language(s): ",
      print_list_res(x$content$translations), "\n", sep = "")
}
