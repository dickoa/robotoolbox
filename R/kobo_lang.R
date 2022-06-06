#' Languages used in the KoboToolbox project
#'
#' Languages used in the KoboToolbox project
#'
#' @rdname kobo_lang
#'
#' @param x kobo_asset or asset uid
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
#' @return a vector of languages. It returnes "Labels" when no language is set
#' @export
kobo_lang <- function(x)
  UseMethod("kobo_lang")

#' Languages used in the KoboToolbox project
#'
#' Languages used in the KoboToolbox project
#'
#' @rdname kobo_lang
#'
#' @param x kobo_asset or asset uid
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
#' @return a vector of languages. It returnes "Labels" when no language is set
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
