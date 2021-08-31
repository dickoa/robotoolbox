#' @noRd
kobo_form_to_list <- function(x, key) {
  x <- x$survey[!x$survey$type %in% c("begin_group", "end_group", "note"), ]
  nm <- x$name
  setNames(as.list(x[[key]]),
           nm)
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
#' @importFrom tibble as_tibble
#' @importFrom tidyr unnest
#' @return kobo_form, the project form
#'
#' @export
kobo_form.kobo_asset <- function(asset) {
  survey <- rbindlist(asset$content$survey, fill = TRUE)
  survey <- setNames(unnest(survey, cols = is_list_cols(survey)),
                     gsub("^\\$", "", names(survey)))
  choices <- rbindlist(asset$content$choices, fill = TRUE)
  choices <- setNames(unnest(choices, cols = is_list_cols(choices)),
                      gsub("^\\$", "", names(choices)))
  settings <- as_tibble(asset$content$settings,
                        .name_repair = "universal")
  structure(list(survey = survey,
                 choices = choices,
                 settings = settings,
                 asset = asset),
            class = "kobo_form")
}

#' @noRd
#' @export
print.kobo_form <- function(x, ...) {
  cat("<robotoolbox form> ", x$asset$uid, "\n")
  cat("   Asset name: ", x$asset$name, "\n", sep = "")
  cat("   Language(s): ",
      print_list_res(x$asset$content$translations), "\n", sep = "")
}
