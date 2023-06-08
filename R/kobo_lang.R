#' Get the languages used in a KoboToolbox survey form
#'
#' Get the languages used in a KoboToolbox survey form from a \code{kobo_asset}
#' or asset unique identifier.
#'
#' @name kobo_lang
#'
#' @param x the unique identifier of a specific asset (`character`) or
#' a \code{kobo_asset} object.
#'
#' @returns A vector of \code{character}. The languages used in the form, it returns "Labels"
#' when no language is set.
#'
#' @examples
#' \dontrun{
#' kobo_setup()
#' uid <- "a9cwEQcbWqWzA5hzkjRUWi"
#' asset <- kobo_asset(uid)
#' lang <- kobo_lang(asset)
#' lang
#' }
#'
#' @export
kobo_lang <- function(x) {
  UseMethod("kobo_lang")
}

#' @export
kobo_lang.kobo_asset <- function(x) {
  lng <- x$content$translations
  if (sum(lengths(lng)) <= 0)
    lng <- "Labels"
  unlist(lng)
}

#' @export
kobo_lang.character <- function(x) {
  if (!assert_uid(x))
    abort(message = "Invalid asset uid")
  kobo_lang.kobo_asset(kobo_asset(x))
}

#' @export
kobo_lang.default <- function(x) {
  abort("You need to use a 'kobo_asset' or a valid asset uid")
}
