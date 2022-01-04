#' @rdname kobo_submissions
#' @export
kobo_submissions <- function(x, paginate, page_size)
  UseMethod("kobo_submissions")


#' Get all submissions from a project
#'
#' Get all submissions from a project (KoBoToolbox asset)
#'
#' @rdname kobo_submissions
#'
#'
#' @importFrom tibble rowid_to_column
#'
#' @param x a kobo_asset or  asset uid, the asset
#' @param paginate logical, split submissions by page. Default to FALSE
#' @param page_size integer, number of submissions per page. if missing, default to
#' number of submissions divided by 5
#'
#' @return data.frame
#' @export
kobo_submissions.kobo_asset <- function(x, paginate = FALSE, page_size = NULL) {
  if (isTRUE(paginate)) {
    size <- x$deployment__submission_count
    if (is.null(page_size))
      page_size <- size %/% 5
    subs <- get_subs_async(x$uid, size, page_size)
  } else {
    subs <- get_subs(x$uid)
  }
  form <- kobo_form(x)
  subs <- kobo_postprocess(subs, form)
  subs <- rowid_to_column(subs, "index")
  subs <- select(subs, -contains("_attachments"))
  if ("begin_repeat" %in% form$type)
    subs <- rowid_to_column(subs, "_index")
  subs
}

#' @rdname kobo_submissions
#' @export
kobo_submissions.character <- function(x, paginate = FALSE, page_size = NULL) {
  kobo_submissions(kobo_asset(x))
}

#' @rdname kobo_submissions
#' @export
kobo_submissions.default <- function(x, paginate = FALSE, page_size = NULL) {
  stop("You need to use a 'kobo_asset' or an asset uid 'kobo_submissions'",
       call. = FALSE)
}
