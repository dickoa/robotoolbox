#' @rdname kobo_submissions
#' @export
kobo_submissions <- function(asset, paginate, page_size)
  UseMethod("kobo_submissions")


#' Get all submissions from a project
#'
#' Get all submissions from a project (KoBoToolbox asset)
#'
#' @rdname kobo_submissions
#'
#' @param asset kobo_asset, the asset
#' @param paginate logical, split submissions by page. Default to FALSE
#' @param page_size integer, number of submissions per page. if missing, default to
#' number of submissions divided by 5
#'
#' @return data.frame
#' @export
kobo_submissions.kobo_asset <- function(asset, paginate = FALSE, page_size = NULL) {
  if (isTRUE(paginate)) {
    size <- asset$deployment__submission_count
    if (is.null(page_size))
      page_size <- size %/% 5
    subs <- get_subs_async(asset$uid, size, page_size)
  } else {
    subs <- get_subs(asset$uid)
  }
  form <- kobo_form(asset)
  kobo_postprocess(subs, form)
}
