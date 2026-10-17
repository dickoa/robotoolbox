#' @title Session-level cache for KoboToolbox metadata
#' @name kobo_cache
#' @description
#' robotoolbox keeps form metadata and languages in memory for the rest of the
#' R session, so that `kobo_data()`, `kobo_lang_set()` and `kobo_lang_get()` do
#' not rebuild them on every call.
#'
#' @details
#' A form and its languages are cached under the server URL, the asset uid and
#' the form version. Without an explicit version, the version is the deployed
#' one, or the draft for a form that was never deployed. A redeployed form has
#' a new version, so the next call builds and caches the new form. The same
#' uid on two servers gives two cache entries.
#'
#' `kobo_data()` also records, per server URL, the number of submissions the
#' server returns per request when it is lower than the number asked, and uses
#' it for the following requests.
#'
#' The cache is cleared when the R session ends, or with
#' `kobo_cache_clear()`.
NULL

# Session-level cache environment
.robotoolbox_cache <- new.env(parent = emptyenv())
.robotoolbox_cache$forms <- list()
.robotoolbox_cache$langs <- list()
.robotoolbox_cache$page_cap <- list()

#' Clear the metadata cache
#'
#' @param uid Optional asset uid. If provided, clears the forms and languages
#'   of that asset only, on every server.
#' @param type Character, which cache to clear: "all" (default), "forms", or
#'   "langs". "all" also clears the recorded server page limits.
#'
#' @returns Invisibly returns TRUE
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
    if (type == "all") .robotoolbox_cache$page_cap <- list()
  } else {
    if (clear_forms) {
      keys <- cache_keys_of_(names(.robotoolbox_cache$forms), uid)
      .robotoolbox_cache$forms[keys] <- NULL
    }
    if (clear_langs) {
      keys <- cache_keys_of_(names(.robotoolbox_cache$langs), uid)
      .robotoolbox_cache$langs[keys] <- NULL
    }
  }
  invisible(TRUE)
}

#' Get cache statistics
#'
#' @returns A list with cache statistics
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

#' @importFrom rlang %||%
#' @noRd
cache_server_ <- function() {
  sub("/+$", "", Sys.getenv("KOBOTOOLBOX_URL"))
}

#' @noRd
cache_key_ <- function(uid, version) {
  paste(cache_server_(), uid, version, sep = "|")
}

#' @noRd
cache_keys_of_ <- function(keys, uid) {
  keys <- as.character(keys)
  keys[vapply(strsplit(keys, "|", fixed = TRUE), \(k) k[2] %in% uid, logical(1))]
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
get_cached_lang_ <- function(uid, version) {
  .robotoolbox_cache$langs[[cache_key_(uid, version)]]
}

#' @noRd
set_cached_lang_ <- function(uid, version, langs) {
  .robotoolbox_cache$langs[[cache_key_(uid, version)]] <- langs
  invisible(langs)
}

#' @noRd
has_cached_lang_ <- function(uid, version) {
  cache_key_(uid, version) %in% names(.robotoolbox_cache$langs)
}

#' @noRd
page_cap_get_ <- function() {
  .robotoolbox_cache$page_cap[[cache_server_()]] %||% Inf
}

#' @noRd
page_cap_set_ <- function(cap) {
  .robotoolbox_cache$page_cap[[cache_server_()]] <- cap
  invisible(cap)
}
