#' @title Session-level cache for KoboToolbox metadata
#' @name kobo_cache
#' @description
#' Session-level caching for form metadata and languages. Form versions are
#' immutable and languages don't change, so caching is safe and significantly
#' improves performance.
#'
#' @details
#' Cached items:
#' - **Form metadata**: When `all_versions = TRUE` in `kobo_data()`, multiple
#'   API calls are made to fetch form schemas for each version. Caching
#'   eliminates redundant calls within a session.
#' - **Languages**: `kobo_lang()` results are cached per asset, speeding up
#'   `kobo_lang_set()` and `kobo_lang_get()` operations.
#'
#' Cache is stored in memory and cleared when R session ends.
#'
#' Note: Version lists are NOT cached as they can change when users deploy
#' new form versions, and the API call sequence is required by the server.
NULL

# Session-level cache environment
.robotoolbox_cache <- new.env(parent = emptyenv())
.robotoolbox_cache$forms <- list()
.robotoolbox_cache$langs <- list()

#' Clear the metadata cache
#'
#' @param uid Optional asset uid. If provided, clears cache for that asset only.
#' @param type Character, which cache to clear: "all" (default), "forms", or "langs".
#'
#' @return Invisibly returns TRUE
#'
#' @examples
#' \dontrun{
#' kobo_cache_clear()
#' kobo_cache_clear("aXf4gH7kL9mN2pQrStUvWx")
#' kobo_cache_clear(type = "langs")
#' }
#'
#' @export
kobo_cache_clear <- function(uid = NULL, type = c("all", "forms", "langs")) {
  type <- match.arg(type)

  clear_forms <- type %in% c("all", "forms")
  clear_langs <- type %in% c("all", "langs")

  if (is.null(uid)) {
    if (clear_forms) {
      .robotoolbox_cache$forms <- list()
    }
    if (clear_langs) .robotoolbox_cache$langs <- list()
  } else {
    if (clear_forms) {
      pattern <- paste0("^", uid, "_")
      keys <- grep(pattern, names(.robotoolbox_cache$forms), value = TRUE)
      for (k in keys) {
        .robotoolbox_cache$forms[[k]] <- NULL
      }
    }
    if (clear_langs) {
      .robotoolbox_cache$langs[[uid]] <- NULL
    }
  }
  invisible(TRUE)
}

#' Get cache statistics
#'
#' @return A list with cache statistics
#'
#' @examples
#' \dontrun{
#' kobo_cache_info()
#' }
#'
#' @export
kobo_cache_info <- function() {
  list(
    n_forms = length(.robotoolbox_cache$forms),
    form_keys = names(.robotoolbox_cache$forms),
    n_langs = length(.robotoolbox_cache$langs),
    lang_keys = names(.robotoolbox_cache$langs)
  )
}

#' @noRd
cache_key_ <- function(uid, version) {
  paste0(uid, "_", version %||% "latest")
}

#' @noRd
get_cached_form_ <- function(uid, version) {
  .robotoolbox_cache$forms[[cache_key_(uid, version)]]
}

#' @noRd
set_cached_form_ <- function(uid, version, value) {
  .robotoolbox_cache$forms[[cache_key_(uid, version)]] <- value
  invisible(value)
}

#' @noRd
has_cached_form_ <- function(uid, version) {
  !is.null(.robotoolbox_cache$forms[[cache_key_(uid, version)]])
}

#' @noRd
get_cached_lang_ <- function(uid) {
  .robotoolbox_cache$langs[[uid]]
}

#' @noRd
set_cached_lang_ <- function(uid, langs) {
  .robotoolbox_cache$langs[[uid]] <- langs
  invisible(langs)
}

#' @noRd
has_cached_lang_ <- function(uid) {
  uid %in% names(.robotoolbox_cache$langs)
}
