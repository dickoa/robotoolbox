#' Get and set the languages used in KoboToolbox data
#'
#' @description
#' - `kobo_lang()`: Get the languages available in a KoboToolbox survey form.
#' - `kobo_lang_get()`: Get the current language of a dataset returned by `kobo_data()`.
#' - `kobo_lang_set()`: Change the language labels of a dataset without re-downloading.
#'
#' @name kobo_lang
#'
#' @param x For `kobo_lang()`: the unique identifier of a specific asset (`character`) or
#'   a `kobo_asset` object.
#' @param data For `kobo_lang_get()` and `kobo_lang_set()`: a `data.frame` or `dm` object
#'   returned by `kobo_data()`.
#' @param asset For `kobo_lang_get()` and `kobo_lang_set()`: a `kobo_asset` object
#'   or the unique identifier of a specific asset (`character`).
#' @param lang For `kobo_lang_set()`: character, the target language.
#'   Use `kobo_lang(asset)` to see available languages.
#'
#' @returns
#' - `kobo_lang()`: A character vector of the languages of the deployed form
#'   version, or of the draft for a form never deployed. Returns `"Labels"`
#'   when no language is set.
#' - `kobo_lang_get()`: A character string of the current language, or `NA` if
#'   it cannot be determined.
#' - `kobo_lang_set()`: The data with updated labels in the new language.
#'
#' @details
#' KoboToolbox forms can have multiple language translations. Normally, switching
#' languages requires calling `kobo_data()` again with a different `lang` parameter,
#' which re-downloads all data from the server.
#'
#' `kobo_lang_set()` applies labels from a different language to existing data
#' instantly, using the cached form metadata. This is useful when you need to
#' work with data in multiple languages or export data with different label sets.
#'
#' `kobo_lang_get()` inspects the current variable labels in the data and matches
#' them against the form to determine which language is currently applied.
#'
#' With data from `kobo_data(colnames_label = TRUE)`, `kobo_lang_set()` renames
#' the columns to the labels of the new language. It finds the question of each
#' column through the `kobo_name` attribute, so two questions with the same
#' label keep their own columns.
#'
#' With data from `kobo_data(select_multiple_label = TRUE)`, `kobo_lang_set()`
#' also translates the answers of select_multiple questions. It splits each
#' answer into the labels of its question, and stops with an error when an
#' answer splits in more than one way. This happens with "Red Blue" when
#' "Red", "Blue" and "Red Blue" are all choices, or with a code kept without a
#' label, such as `Red` from an older form version, when "Red" is also the
#' label of another choice. Download the data in the new language with
#' `kobo_data(lang = )` in that case.
#'
#' @examples
#' \dontrun{
#' # Setup
#' asset <- kobo_asset("aYuTZn9vegi3Z49MXwKjep")
#'
#' # List available languages
#' kobo_lang(asset)
#'
#' # Fetch data in English
#' df <- kobo_data(asset, lang = "English (en)")
#'
#' # Check current language (using asset object or uid)
#' kobo_lang_get(df, asset)
#' kobo_lang_get(df, "aYuTZn9vegi3Z49MXwKjep")
#'
#' # Switch to French instantly (no API call)
#' df_fr <- kobo_lang_set(df, asset, lang = "Francais (fr)")
#' kobo_lang_get(df_fr, asset)
#'
#' # Works with dm objects (nested forms) too
#' dm_data <- kobo_data(complex_asset)
#' dm_fr <- kobo_lang_set(dm_data, complex_asset, lang = "French")
#' }
#'
#' @importFrom rlang abort
#' @export
kobo_lang <- function(x) {
  UseMethod("kobo_lang")
}

#' @rdname kobo_lang
#' @export
kobo_lang.kobo_asset <- function(x) {
  uid <- x$uid
  version <- form_version_uid_(x)

  if (has_cached_lang_(uid, version))
    return(get_cached_lang_(uid, version))

  lng <- content_langs_(form_source_(x, version)$content)
  set_cached_lang_(uid, version, lng)
  lng
}

#' @rdname kobo_lang
#' @export
kobo_lang.character <- function(x) {
  if (!assert_uid(x))
    abort(message = "Invalid asset uid")
  kobo_lang.kobo_asset(kobo_asset(x))
}

#' @rdname kobo_lang
#' @export
kobo_lang.default <- function(x) {
  abort("You need to use a 'kobo_asset' or a valid asset uid")
}

#' @rdname kobo_lang
#' @export
kobo_lang_get <- function(data, asset) {
  UseMethod("kobo_lang_get")
}

#' @rdname kobo_lang
#' @importFrom labelled var_label
#' @export
kobo_lang_get.data.frame <- function(data, asset) {
  asset <- as_kobo_asset_(asset)
  form <- kobo_form(asset)
  available_langs <- kobo_lang(asset)

  current_labels <- var_label(data)
  has_label <- !vapply(current_labels, is.null, logical(1))
  if (!any(has_label)) return(NA_character_)

  is_label_cols <- is_label_cols_(list(data), form)
  if (has_kobo_names_(list(data))) {
    is_label_cols <- FALSE
    names(current_labels) <- kobo_names_(data)
  }
  current_labels <- current_labels[has_label]
  col_names <- names(current_labels)

  matches <- vapply(available_langs, function(lng) {
    form_lang <- form[form$lang == lng, ]
    form_labels <- setNames(form_lang$label, form_lang$name)

    if (is_label_cols) {
      sum(col_names %in% form_labels, na.rm = TRUE)
    } else {
      common_vars <- intersect(col_names, names(form_labels))
      if (length(common_vars) == 0) return(0L)
      sum(unlist(current_labels[common_vars]) == form_labels[common_vars],
          na.rm = TRUE)
    }
  }, integer(1))

  if (max(matches) == 0) return(NA_character_)
  names(which.max(matches))
}

#' @rdname kobo_lang
#' @importFrom dm dm_get_tables
#' @export
kobo_lang_get.dm <- function(data, asset) {
  asset <- as_kobo_asset_(asset)
  tbls <- dm_get_tables(data)
  order <- c("main", setdiff(names(tbls), "main"))
  for (nm in order) {
    res <- kobo_lang_get.data.frame(tbls[[nm]], asset)
    if (!is.na(res)) return(res)
  }
  NA_character_
}

#' @rdname kobo_lang
#' @export
kobo_lang_get.default <- function(data, asset) {
  abort("data must be a data.frame or dm object returned by kobo_data()")
}

#' @rdname kobo_lang
#' @export
kobo_lang_set <- function(data, asset, lang) {
  UseMethod("kobo_lang_set")
}

#' @rdname kobo_lang
#' @importFrom dplyr rename
#' @importFrom tidyselect all_of
#' @export
kobo_lang_set.data.frame <- function(data, asset, lang) {
  asset <- as_kobo_asset_(asset)
  available_langs <- kobo_lang(asset)
  if (!lang %in% available_langs) {
    abort(
      c(
        paste0("Language '", lang, "' not found."),
        i = paste0("Available languages: ", paste(available_langs, collapse = ", "))
      ),
      call = NULL
    )
  }

  form <- kobo_form(asset)
  is_label_cols <- is_label_cols_(list(data), form)
  cur_lang <- legacy_label_lang_(list(data), data, asset, form, is_label_cols)
  sep <- sm_sep_(kobo_names_(data), form)
  lang_set_tbl_(data, form, lang, sep, is_label_cols, cur_lang)
}

#' Language of label columns that do not record their question name
#'
#' Data from versions before 1.7.0 has no `kobo_name` attribute, so its label
#' columns are mapped back through the labels of the current language.
#' @noRd
legacy_label_lang_ <- function(tbls, data, asset, form, is_label_cols) {
  if (!is_label_cols || has_kobo_names_(tbls)) {
    return(NA_character_)
  }
  kobo_lang_get(data, asset)
}

#' @importFrom dplyr rename
#' @importFrom tidyselect all_of
#' @importFrom labelled remove_val_labels
#' @noRd
lang_set_tbl_ <- function(tbl, form, lang, sep, is_label_cols, cur_lang) {
  if (is_label_cols) {
    if (!is.na(cur_lang)) {
      reverse <- reverse_label_lookup_(names(tbl), form, cur_lang)
      if (length(reverse) > 0) tbl <- rename(tbl, all_of(reverse))
    }
    tbl <- rename_kobo_(tbl, kobo_names_(tbl))
  }
  tbl <- sm_labels_lang_set_(tbl, form, lang)
  so <- unique(form$name[grepl("select_one", form$type, fixed = TRUE)])
  for (n in intersect(names(tbl), so)) {
    tbl[[n]] <- remove_val_labels(tbl[[n]])
  }
  tbl <- val_labels_from_form_(tbl, form, lang)
  tbl <- var_labels_from_form_(tbl, form, lang, sep = sep)
  if (is_label_cols) tbl <- set_names_from_varlabel_(tbl)
  tbl
}

#' Translate select_multiple answers written with labels
#'
#' `kobo_data(select_multiple_label = TRUE)` records the language of these
#' answers in the `kobo_sm_lang` attribute, and the codes kept without a label
#' in `kobo_sm_unlabeled`.
#' @noRd
sm_labels_lang_set_ <- function(tbl, form, lang) {
  for (n in names(tbl)) {
    old <- attr(tbl[[n]], "kobo_sm_lang", exact = TRUE)
    if (is.null(old) || identical(old, lang)) next
    unlabeled <- attr(tbl[[n]], "kobo_sm_unlabeled", exact = TRUE) %||% character()
    codes <- sm_labels_to_codes_(tbl[[n]], sm_lookup_(form, n, old), n, unlabeled)
    lookup <- sm_lookup_(form, n, lang)
    tbl[[n]][] <- sm_codes_to_labels_(codes, lookup)
    attr(tbl[[n]], "kobo_sm_lang") <- lang
    attr(tbl[[n]], "kobo_sm_unlabeled") <- sm_unlabeled_(codes, lookup)
  }
  tbl
}

#' @rdname kobo_lang
#' @importFrom dm dm_get_tables dm_add_pk dm_add_fk dm_get_all_pks dm_get_all_fks
#' @export
kobo_lang_set.dm <- function(data, asset, lang) {
  asset <- as_kobo_asset_(asset)
  available_langs <- kobo_lang(asset)
  if (!lang %in% available_langs) {
    abort(
      c(
        paste0("Language '", lang, "' not found."),
        i = paste0("Available languages: ", paste(available_langs, collapse = ", "))
      ),
      call = NULL
    )
  }

  form <- kobo_form(asset)

  tbls <- dm_get_tables(data)
  tbl_names <- names(tbls)

  is_label_cols <- is_label_cols_(tbls, form)
  cur_lang <- legacy_label_lang_(tbls, data, asset, form, is_label_cols)
  sep <- sm_sep_(unlist(lapply(tbls, kobo_names_), use.names = FALSE), form)
  tbls_updated <- lapply(tbl_names, function(nm) {
    lang_set_tbl_(tbls[[nm]], form, lang, sep, is_label_cols, cur_lang)
  })
  names(tbls_updated) <- tbl_names

  result <- dm::dm(!!!tbls_updated)
  pks <- dm_get_all_pks(data)
  for (i in seq_len(nrow(pks))) {
    result <- dm_add_pk(result, !!pks$table[i], !!unclass(pks$pk_col[[i]]))
  }
  fks <- dm_get_all_fks(data)
  for (i in seq_len(nrow(fks))) {
    result <- dm_add_fk(
      result,
      !!fks$child_table[i],
      !!unclass(fks$child_fk_cols[[i]]),
      !!fks$parent_table[i],
      !!unclass(fks$parent_key_cols[[i]])
    )
  }
  result
}

#' @rdname kobo_lang
#' @export
kobo_lang_set.default <- function(data, asset, lang) {
  abort("data must be a data.frame or dm object returned by kobo_data()")
}
