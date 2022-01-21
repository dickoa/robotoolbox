#' @rdname kobo_data
#' @export
kobo_data <- function(x, paginate, page_size, lang)
  UseMethod("kobo_data")

#' @rdname kobo_data
#' @export
kobo_submissions <- function(x, paginate, page_size, lang)
  UseMethod("kobo_submissions")

#' Get all submissions from a project
#'
#' Get all submissions from a project (KoBoToolbox asset)
#'
#' @rdname kobo_data
#'
#' @importFrom tidyselect contains
#' @importFrom dplyr select
#' @importFrom tibble rowid_to_column tibble
#' @importFrom dm as_dm dm_add_pk dm_add_fk
#'
#' @param x a kobo_asset or  asset uid, the asset
#' @param paginate logical, split submissions by page. Default to FALSE
#' @param page_size integer, number of submissions per page.
#' if missing, default to number of submissions divided by 5
#' @param lang character, language for the variable and value labels
#'
#' @return data.frame
#' @export
kobo_data.kobo_asset <- function(x, paginate = FALSE,
                                 page_size = NULL, lang = NULL) {
  size <- x$deployment__submission_count
  if (size >= 30000)
    paginate <- TRUE

  if (isTRUE(paginate)) {
    if (is.null(page_size))
      page_size <- size %/% 5
    subs <- get_subs_async(x$uid, size, page_size)
  } else {
    subs <- get_subs(x$uid)
  }
  form <- kobo_form(x)
  klang <- kobo_lang(x)
  if (is.null(lang) || !lang %in% klang)
    lang <- klang[1]
  subs <- name_repair_(subs)
  subs <- select(tibble(subs),
                 -contains("_attachments"))
  if ("begin_repeat" %in% form$type) {
    subs <- c(list(main = rowid_to_column(subs, "_index")),
              kobo_extract_repeat_tbl(subs, form))
    subs <- lapply(subs, function(d) {
      d <- postprocess_data_(x = d,
                             form = form,
                             lang = lang)
      d
    })
    subs <- as_dm(subs)
    subs <- dm_add_pk(subs, "main", "_index")
    p <- length(subs)
    for (j in 2:p) {
      tbl_nm <- names(subs)[j]
      ref_tbl_nm <- unique(subs[[j]][["_parent_table_name"]])
      subs <- dm_add_pk(subs, {{tbl_nm}}, "_index")
      subs <- dm_add_fk(subs, {{tbl_nm}},
                        "_parent_index",
                        {{ref_tbl_nm}})
    }
  } else {
    subs <- postprocess_data_(x = subs,
                              form = form,
                              lang = lang)
  }
  subs
}

#' @rdname kobo_data
#' @export
kobo_submissions.kobo_asset <- kobo_data.kobo_asset

#' @rdname kobo_data
#' @export
kobo_data.character <- function(x, paginate = FALSE,
                                page_size = NULL, lang = NULL) {
  kobo_data(kobo_asset(x),
            paginate = paginate,
            page_size = page_size,
            lang = lang)
}

#' @rdname kobo_data
#' @export
kobo_submissions.character <- kobo_data.character

#' @rdname kobo_data
#' @export
kobo_data.default <- function(x, paginate = FALSE,
                              page_size = NULL, lang = NULL) {
  stop("You need to use a 'kobo_asset' or an asset uid",
       call. = FALSE)
}

#' @rdname kobo_data
#' @export
kobo_submissions.default <- kobo_data.default
