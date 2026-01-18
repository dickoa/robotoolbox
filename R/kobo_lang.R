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
#' @param asset For `kobo_lang_get()` and `kobo_lang_set()`: the `kobo_asset` object
#'   used to fetch the data.
#' @param lang For `kobo_lang_set()`: character, the target language.
#'   Use `kobo_lang(asset)` to see available languages.
#'
#' @returns
#' - `kobo_lang()`: A character vector of available languages. Returns `"Labels"`
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
#' # Check current language
#' kobo_lang_get(df, asset)
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

 if (has_cached_lang_(uid)) {
   return(get_cached_lang_(uid))
 }

 lng <- x$content$translations
 if (sum(lengths(lng)) <= 0)
   lng <- "Labels"
 lng <- unlist(lng)

 set_cached_lang_(uid, lng)
 lng
}

#' @rdname kobo_lang
#' @export
kobo_lang.character <- function(x) {
  if (!assert_uid(x))
    abort(message = "Invalid asset uid")
 if (has_cached_lang_(x)) {
    return(get_cached_lang_(x))
  }
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
  form <- kobo_form(asset)
  available_langs <- kobo_lang(asset)

  current_labels <- var_label(data)
  current_labels <- current_labels[!sapply(current_labels, is.null)]

  if (length(current_labels) == 0) return(NA_character_)

  matches <- vapply(available_langs, function(lng) {
    form_lang <- form[form$lang == lng, ]
    form_labels <- setNames(form_lang$label, form_lang$name)

    common_vars <- intersect(names(current_labels), names(form_labels))
    if (length(common_vars) == 0)
      return(0L)
    sum(unlist(current_labels[common_vars]) == form_labels[common_vars], na.rm = TRUE)
  }, integer(1))

  if (max(matches) == 0)
    return(NA_character_)

  names(which.max(matches))
}

#' @rdname kobo_lang
#' @importFrom dm dm_get_tables
#' @export
kobo_lang_get.dm <- function(data, asset) {
  tbls <- dm_get_tables(data)
  if ("main" %in% names(tbls)) {
    kobo_lang_get.data.frame(tbls$main, asset)
  } else {
    kobo_lang_get.data.frame(tbls[[1]], asset)
  }
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
#' @export
kobo_lang_set.data.frame <- function(data, asset, lang) {
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

  data <- val_labels_from_form_(data, form, lang)
  data <- var_labels_from_form_(data, form, lang)

  data
}

#' @rdname kobo_lang
#' @importFrom dm dm_get_tables dm_add_pk dm_add_fk
#' @export
kobo_lang_set.dm <- function(data, asset, lang) {
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

  tbls_updated <- lapply(tbl_names, function(nm) {
    tbl <- tbls[[nm]]
    tbl <- val_labels_from_form_(tbl, form, lang)
    tbl <- var_labels_from_form_(tbl, form, lang)
    tbl
  })
  names(tbls_updated) <- tbl_names

  result <- dm::dm(!!!tbls_updated)

  if ("main" %in% tbl_names && "_index" %in% names(result[["main"]])) {
    result <- dm_add_pk(result, "main", "_index")
  }

  for (nm in setdiff(tbl_names, "main")) {
    if ("_index" %in% names(result[[nm]])) {
      result <- dm_add_pk(result, !!nm, "_index")
    }
    if ("_parent_index" %in% names(result[[nm]]) &&
        "_parent_table_name" %in% names(result[[nm]])) {
      parent <- unique(result[[nm]][["_parent_table_name"]])
      if (length(parent) == 1 && parent %in% tbl_names) {
        result <- dm_add_fk(result, !!nm, "_parent_index", !!parent)
      }
    }
  }
  result
}

#' @rdname kobo_lang
#' @export
kobo_lang_set.default <- function(data, asset, lang) {
  abort("data must be a data.frame or dm object returned by kobo_data()")
}
