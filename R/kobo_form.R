#' Get a KoboToolbox project form
#'
#' Get a KoboToolbox project form from a \code{\link{kobo_asset}} or asset uid.
#'
#' @param x the asset uid or the kobo_asset object
#' @param version character, uid of the version of the asset
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
#' @return a data.frame with information on name, label, lang, and list-column representing form choices
#'
#' @examples
#' \dontrun{
#' #' kobo_setup()
#' asset_list <- kobo_asset_list()
#' uid <- asset_list$uid[1]
#' asset <- kobo_asset(uid)
#' form <- kobo_form(uid)
#' # form <- kobo_form(asset)
#' }
#'
#' @export
kobo_form <- function(x, version)
  UseMethod("kobo_form")

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
                   -.data$name)
  survey <- setNames(unnest(survey,
                            cols = is_list_cols(survey),
                            keep_empty = TRUE),
                     gsub("^\\$", "", names(survey)))
  stypes <- c("begin_repeat", "end_repeat",
              kobo_question_types())
  survey <- filter(survey, .data$type %in% stypes)
  survey$version <- version
  if ("choices" %in% asset_content_nm) {
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
                                              id = "Latin-ASCII")
    choices <- setNames(unnest(choices,
                               cols = is_list_cols(choices),
                               keep_empty = TRUE),
                        gsub("^\\$", "", names(choices)))
    form <- nest_join(survey, choices,
                      by = "list_name")
  } else {
    form <- survey
  }
  form$name <- stri_trans_general(form$name, "Latin-ASCII")
  form
}

#' @export
kobo_form.character <- function(x, version = NULL) {
  kobo_form(kobo_asset(x), version)
}

#' @export
kobo_form.default <- function(x, version) {
  stop("You need to use a 'kobo_asset' or an asset uid",
       call. = FALSE)
}
