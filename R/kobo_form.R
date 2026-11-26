#' Get a KoboToolbox survey form
#'
#' Get a KoboToolbox survey form from a `kobo_asset` or an asset unique identifier.
#'
#' @inheritParams kobo_asset
#' @param version character, the unique identifier of the version of the asset.
#'   Defaults to the deployed version, or to the draft for a form that was
#'   never deployed.
#'
#' @name kobo_form
#'
#' @importFrom data.table rbindlist
#' @importFrom dplyr select nest_join coalesce filter
#' @importFrom tibble as_tibble new_tibble
#' @importFrom tidyr unnest drop_na
#' @importFrom stringi stri_trans_general
#' @importFrom stats setNames
#' @importFrom tidyselect contains everything all_of
#' @importFrom rlang .data set_names abort
#'
#' @returns A `data.frame` with the following columns:
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
#' Other columns, such as `relevant` or `calculation`, are present when the
#' form uses them. A form without choice list has no `choices` column.
#'
#' @details
#' The form is kept in memory for the rest of the R session, see [kobo_cache].
#'
#' @family data functions
#'
#' @examples
#' # With a bundled example asset, no server is needed
#' form <- kobo_form(asset_ml)
#' form[c("name", "type", "label", "lang")]
#'
#' \dontrun{
#' # Use your own URL and token
#' kobo_setup(url = "https://kf.kobotoolbox.org",
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
  uid <- x$uid
  version <- version %||% form_version_uid_(x)

  if (has_cached_form_(uid, version))
    return(get_cached_form_(uid, version))

  src <- form_source_(x, version)
  form <- kobo_form_api_(src, version)
  set_cached_form_(uid, version, form)
  if (!has_cached_lang_(uid, version))
    set_cached_lang_(uid, version, content_langs_(src$content))

  form
}

#' @export
kobo_form.character <- function(x, version = NULL) {
  if (!assert_uid(x))
    abort(message = "Invalid asset uid")

  if (!is.null(version) && has_cached_form_(x, version))
    return(get_cached_form_(x, version))

  kobo_form(kobo_asset(x), version)
}

#' @export
kobo_form.default <- function(x, version) {
  abort("You need to use a 'kobo_asset' or a valid asset uid")
}

#' @noRd
form_version_uid_ <- function(x) {
  dv <- x$deployed_versions$results
  if (length(dv))
    return(dv[[1]]$uid)
  x$version_id %||% x$deployed_version_id
}

#' @noRd
form_source_ <- function(x, version) {
  if (is.null(version) || inherits(x, "kobo_asset_version") ||
        identical(version, x$version_id))
    return(x)
  dv <- Filter(\(v) identical(v$uid, version), x$deployed_versions$results)
  same <- length(dv) > 0 && !is.null(x$version__content_hash) &&
    identical(dv[[1]]$content_hash, x$version__content_hash)
  if (same)
    return(x)
  res <- kobo_asset_version(x, version)
  ## Versions saved before KPI added autonames can have unnamed questions
  unnamed <- vapply(res$content$survey, \(q) {
    !is.null(q$label) && is.null(q$name) && is.null(q[["$autoname"]])
  }, logical(1))
  if (any(unnamed) && identical(version, form_version_uid_(x)))
    return(x)
  res
}

#' @noRd
content_langs_ <- function(content) {
  lng <- content$translations
  if (sum(lengths(lng)) <= 0)
    lng <- "Labels"
  unlist(lng)
}

#' @noRd
kobo_form_api_ <- function(x, version = NULL) {

  display <- kobo_display_fields()
  form_display_fields <- function(x, lang) {
    nm <- names(x)
    nm <- c(nm[nm %in% display], "lang")
    ss <- length(x$label)
    x$lang <- lang[seq.int(ss)]
    for (n in nm)
      x[[n]] <- lapply(x[[n]], null2char)
    ## Same as drop_nulls(x), recursing only where a NULL can remain
    x <- x[!vapply(x, is.null, logical(1))]
    for (i in which(vapply(x, is.list, logical(1)))) {
      if (any(vapply(x[[i]], \(e) is.null(e) || is.list(e), logical(1))))
        x[[i]] <- drop_nulls(x[[i]])
    }
    x
  }

  asset <- form_source_(x, version)
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
  survey <- rbindlist(survey, fill = TRUE)
  if (!"label" %in% names(survey)) {
    survey$label <- NA_character_
  }
  has_autoname <- "$autoname" %in% names(survey)
  if (has_autoname) {
    survey <- select(.data = survey,
                     name = "$autoname",
                     list_name = contains("select_from_list_name"),
                     type = "type",
                     label = "label",
                     lang = "lang",
                     everything(),
                     -"name")
  } else {
    survey <- select(.data = survey,
                     name = "name",
                     list_name = contains("select_from_list_name"),
                     type = "type",
                     label = "label",
                     lang = "lang",
                     everything())
  }
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
  survey$version <- x$deployed_version_id %||% NA_character_
  if (!is.null(version))
    survey$version <- version
  form <- survey
  if ("choices" %in% asset_content_nm && "list_name" %in% names(survey)) {
    choices <- lapply(asset$content$choices, function(l) {
      x <- form_display_fields(l, lang)
      x
    })
    choices <- rbindlist(choices, fill = TRUE)
    has_autovalue <- "$autovalue" %in% names(choices)
    if (has_autovalue) {
      choices <- select(.data = choices,
                        list_name = "list_name",
                        value_name = "$autovalue",
                        value_label = "label",
                        value_lang = "lang")
    } else {
      choices <- select(.data = choices,
                        list_name = "list_name",
                        value_name = "name",
                        value_label = "label",
                        value_lang = "lang")
    }
    choices <- drop_na(choices, "value_label")
    cols_to_unnest <- is_list_cols_names(choices)
    choices <- setNames(unnest(choices,
                               cols = all_of(cols_to_unnest),
                               keep_empty = TRUE),
                        gsub("^\\$", "", names(choices)))
    choices$value_label <- latin_ascii_(choices$value_label)
    if (!is.null(version))
      choices$value_version <- version
    form <- nest_join(survey, choices,
                      by = "list_name")
  }
  form$name <- latin_ascii_(form$name)
  form
}
