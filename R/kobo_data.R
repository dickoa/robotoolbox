#' @noRd
kobo_data_ <- function(x, paginate, page_size, size, lang) {
  if (size >= 10000)
    paginate <- TRUE

  if (isTRUE(paginate)) {
    if (is.null(page_size))
      page_size <- max(size %/% 5, 1)
    subs <- get_subs_async(x$uid, size, page_size)
  } else {
    subs <- get_subs(x$uid)
  }

  form <- kobo_form(x)
  cn <- form$name[form$type %in% kobo_question_types()]
  cn <- unique(cn)
  klang <- kobo_lang(x)
  if (is.null(lang) || !lang %in% klang)
    lang <- klang[1]

  if ("begin_repeat" %in% form$type) {
    names_list <- kobo_form_name_to_list_(filter(form, .data$lang == lang))
    subs <- name_repair_(subs)
    subs <- c(list(main = rowid_to_column(subs, "_index")),
              kobo_extract_repeat_tbl(subs, form))
    nm <- names(subs)
    names_list <- names_list[names(subs)]
    subs <- lapply(nm, function(n) {
      d <- subs[[n]]
      cn_list <- names_list[[n]]
      d[setdiff(cn_list, names(d))] <- NA
      d <- postprocess_data_(x = d,
                             form = form,
                             lang = lang)
      cn <- intersect(cn, names(d))
      select(d, starts_with(cn), everything())
    })
    subs <- setNames(subs, nm)
    subs <- as_dm(subs)
    subs <- dm_add_pk(subs, "main", "_index")
    p <- length(subs)
    for (j in 2:p) {
      tbl_nm <- names(subs)[j]
      ref_tbl_nm <- unique(subs[[j]][["_parent_table_name"]])
      subs <- dm_add_pk(subs, {{tbl_nm}},
                        "_index")
      subs <- dm_add_fk(subs, {{tbl_nm}},
                        "_parent_index",
                        {{ref_tbl_nm}})
    }
  } else {
    subs <- set_names(subs, make_unique_names_)
    subs[setdiff(cn, names(subs))] <- NA
    subs <- postprocess_data_(x = subs,
                              form = form,
                              lang = lang)
    cn <- intersect(cn, names(subs))
    subs <- select(subs, starts_with(cn), everything())
  }
  subs
}

#' Get all submissions from a KoboToolbox project
#'
#' Get all submissions from a KoboToolbox project through a \code{\link{kobo_asset}} or asset uid.
#'
#' @rdname kobo_data
#'
#' @importFrom tidyselect starts_with everything
#' @importFrom dplyr select
#' @importFrom tibble rowid_to_column tibble
#' @importFrom dm as_dm dm_add_pk dm_add_fk
#'
#' @param x a \code{\link{kobo_asset}} or character, the asset
#' @param paginate logical, split submissions by page. Default to FALSE
#' @param page_size integer, number of submissions per page.
#' if missing, default to number of submissions divided by 5
#' @param lang character, language for the variable and value labels
#'
#' @return A data.frame
#'
#' @examples
#' \dontrun{
#' kobo_setup() # setup using your url and token
#' uid <- "a9cwEQcbWqWzA5hzkjRUWi" # pick a valid uid
#' asset <- kobo_asset(uid)
#' subs <- kobo_data(asset) ## kobo_submissions(asset)
#' library(dplyr)
#' glimpse(subs)
#' }
#'
#' @export
kobo_data <- function(x, paginate, page_size, lang)
  UseMethod("kobo_data")

#' @rdname kobo_data
#' @export
kobo_submissions <- function(x, paginate, page_size, lang)
  UseMethod("kobo_submissions")

#' @export
kobo_data.kobo_asset <- function(x, paginate = FALSE,
                                 page_size = NULL, lang = NULL) {
  size <- x$deployment__submission_count
  if (size > 0) {
    res <- kobo_data_(x = x,
                      paginate = paginate,
                      page_size = page_size,
                      size = size,
                      lang = lang)
  } else {
    res <- tibble()
  }
  res
}

#' @rdname kobo_data
#' @export
kobo_submissions.kobo_asset <- kobo_data.kobo_asset

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

#' @export
kobo_data.default <- function(x, paginate = FALSE,
                              page_size = NULL, lang = NULL) {
  stop("You need to use a 'kobo_asset' or an asset uid",
       call. = FALSE)
}

#' @rdname kobo_data
#' @export
kobo_submissions.default <- kobo_data.default
