#' Languages used in the KoboToolbox project
#'
#' Languages used in the KoboToolbox project from a \code{\link{kobo_asset}} or asset uid.
#'
#' @rdname kobo_lang
#'
#' @param x kobo_asset or asset uid, the asset
#'
#' @return a vector of languages used in the form. It returns "Labels" when no language is set
#'
#' @examples
#' \dontrun{
#' kobo_setup() # setup using your url and token
#' uid <- "a9cwEQcbWqWzA5hzkjRUWi" # pick a valid uid
#' asset <- kobo_asset(uid)
#' lang <- kobo_lang(asset) ## or kobo_lang(uid)
#' lang
#' }
#'
#' @export
kobo_lang <- function(x)
  UseMethod("kobo_lang")

#' @export
kobo_lang.kobo_asset <- function(x) {
  lng <- x$content$translations
  if (sum(lengths(lng)) <= 0)
    lng <- "Labels"
  unlist(lng)
}

#' @export
kobo_lang.character <- function(x) {
  kobo_lang.kobo_asset(kobo_asset(x))
}

#' @export
kobo_lang.default <- function(x) {
  stop("You need to use a 'kobo_asset' or a valid asset uid",
       call. = FALSE)
}
