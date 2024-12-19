#' Get a KoboToolbox survey form
#'
#' Get a KoboToolbox survey form from a \code{kobo_asset} or an asset unique identifier.
#'
#' @param x the unique identifier of a specific asset (`character`) or
#' a \code{kobo_asset} object.
#' @param version character, the unique identifier of the version of the asset.
#'
#' @name kobo_form
#'
#' @importFrom data.table rbindlist
#' @importFrom dplyr select nest_join coalesce
#' @importFrom tibble as_tibble new_tibble
#' @importFrom tidyr unnest drop_na
#' @importFrom stringi stri_trans_general
#' @importFrom stats setNames
#' @importFrom tidyselect contains everything all_of
#' @importFrom rlang .data set_names
#'
#' @returns A \code{data.frame} with the following columns:
#' - `name` the name of the survey questions
#' - `list_name` the name of list of code used for values and labels
#' - `type` the type of KoboToolbox survey questions
#' - `label` the label of the questions
#' - `lang` the languages used in the survey
#' - `version` the survey version unique identifier
#' - `choices` a list column with the choices values and labels
#' - `kuid` the unique identifier of the question
#' - `qpath` and `xpath` the path of the question in JSON/XML
#'
#' You can also have other columns such as `relevant`, `calculation`, etc. depending on how
#' you structure for survey form.
#'
#' @examples
#' \dontrun{
#' # Use your own URL and token
#' kobo_setup(url = "https://kf.kobotoolbox.org/",
#'            token = "9et1814c285w094f6v9bd629df47a1a0e81x53a0")
#' # Use your own API asset identifier
#' uid <- "a9cwEQcbWqWzA5hzkjRUWi"
#' asset <- kobo_asset(uid)
#' form <- kobo_form(asset)
#' }
#'
#' @export
kobo_form <- function(x, version) {
  UseMethod("kobo_form")
}

#' @export
kobo_form.kobo_asset <- function(x, version = NULL) {

  form_display_fields <- function(x, lang) {
    nm <- intersect(names(x),
                    kobo_display_fields())
    nm <- c(nm, "lang")
    ss <- length(x$label)
    x$lang <- lang[seq.int(ss)]
    for (n in nm)
      x[[n]] <- lapply(x[[n]], null2char)
    x
  }

  asset <- x
  if (!is.null(version))
    asset <- kobo_asset_version(x, version)
  asset_content_nm <- names(asset$content)
  cond <- "translations" %in% asset_content_nm &
    !is.null(unlist(asset$content$translations))
  if (cond) {
    lang <- asset$content$translations
  } else {
    lang <- "Labels"
  }
  survey <- lapply(asset$content$survey, function(l) {
    x <- form_display_fields(l, lang)
    x
  })
  survey <- rbindlist(drop_nulls(survey), fill = TRUE)
  survey <- select(.data = survey,
                   name = "$autoname",
                   list_name = contains("select_from_list_name"),
                   type = "type",
                   label = "label",
                   lang = "lang",
                   everything(),
                   -"name")
  cols_to_unnest <- is_list_cols_names(survey)
  survey <- setNames(unnest(survey,
                            cols = all_of(cols_to_unnest),
                            keep_empty = TRUE),
                     gsub("^\\$", "", names(survey)))
  stypes <- c("begin_repeat", "end_repeat",
              kobo_question_types())
  if (any(duplicated(names(survey))))
    survey <- set_names(survey, make.names, unique = TRUE)
  survey <- filter(survey, .data$type %in% stypes)
  survey$version <- x$deployed_version_id
  if (!is.null(version))
    survey$version <- version
  form <- survey
  if ("choices" %in% asset_content_nm && "list_name" %in% names(survey)) {
    choices <- lapply(asset$content$choices, function(l) {
      x <- form_display_fields(l, lang)
      x
    })
    choices <- drop_nulls(choices)
    choices <- rbindlist(choices, fill = TRUE)
    choices <- select(.data = choices,
                      list_name = "list_name",
                      value_name = "$autovalue",
                      value_label = "label",
                      value_lang = "lang")
    choices <- drop_na(choices, "value_label")
    choices$value_label <- stri_trans_general(choices$value_label,
                                              id = "latin-ascii")
    cols_to_unnest <- is_list_cols_names(choices)
    choices <- setNames(unnest(choices,
                               cols = all_of(cols_to_unnest),
                               keep_empty = TRUE),
                        gsub("^\\$", "", names(choices)))
    choices_external <- empty_tibble_(c("list_name",
                                        "value_lang",
                                        "value_label",
                                        "value_name"))
    if (!is.null(version))
      choices$value_version <- version
    form <- nest_join(survey, choices,
                      by = "list_name")
  }
  form$name <- stri_trans_general(form$name, "latin-ascii")
  form
}

#' @export
kobo_form.character <- function(x, version = NULL) {
  if (!assert_uid(x))
    abort(message = "Invalid asset uid")
  kobo_form(kobo_asset(x), version)
}

#' @export
kobo_form.default <- function(x, version) {
  abort("You need to use a 'kobo_asset' or a valid asset uid")
}
