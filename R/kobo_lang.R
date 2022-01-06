#' @rdname kobo_lang
#' @export
kobo_lang <- function(x)
  UseMethod("kobo_lang")

#' Languages used in the KoBoToolbox project
#'
#' Languages used in the KoBoToolbox project
#'
#' @rdname kobo_lang
#'
#' @param x kobo_asset or asset uid
#'
#'
#' @return a vector of languages. It returnes "Labels" when no language is set
#' @export
kobo_lang.kobo_asset <- function(x) {
  lng <- x$content$translations
  if (sum(lengths(lng)) <= 0)
    lng <- "Labels"
  unlist(lng)
}

#' @rdname kobo_lang
#' @export
kobo_lang.character <- function(x) {
  kobo_lang(kobo_asset(x))
}

#' @rdname kobo_lang
#' @export
kobo_lang.default <- function(x) {
  stop("You need to use a 'kobo_asset' or a valid asset uid",
       call. = FALSE)
}
