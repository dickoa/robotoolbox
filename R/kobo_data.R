#' @noRd
kobo_data_ <- function(x, paginate, page_size, size, lang, select_multiple_label, colnames_label, all_versions) {

  if (size >= 10000 | !is.null(page_size))
    paginate <- TRUE

  if (isTRUE(paginate)) {
    if (is.null(page_size))
      page_size <- max(size %/% 5, 1)
    subs <- get_subs_async(x$uid, size, page_size)
  } else {
    subs <- get_subs(x$uid)
  }

  form <- kobo_form_version_(subs, x$uid, all_versions = all_versions)
  cn <- kobo_form_names_(form)
  klang <- kobo_lang(x)
  if (is.null(lang) || !lang %in% klang)
    lang <- klang[1]

  if ("begin_repeat" %in% form$type) {
    names_list <- kobo_form_name_to_list_(filter(form, .data$lang == lang))
    subs <- dedup_vars_(subs, all_versions = all_versions)
    subs <- set_names(subs, make_unique_names_)
    subs <- c(list(main = rowid_to_column(subs, "_index")),
              kobo_extract_repeat_tbl(subs, form, all_versions = all_versions))
    nm <- names(subs)
    names_list <- names_list[nm]
    subs <- lapply(nm, function(n) {
      d <- subs[[n]]
      cn_list <- names_list[[n]]
      postprocess_data_(x = d,
                        form = form,
                        lang = lang,
                        select_multiple_label =  select_multiple_label,
                        cn = cn_list)
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
    subs <- dedup_vars_(subs, all_versions = all_versions)
    subs <- set_names(subs, make_unique_names_)
    subs <- postprocess_data_(x = subs,
                              form = form,
                              lang = lang,
                              select_multiple_label =  select_multiple_label,
                              cn = cn)
  }
  if (isTRUE(colnames_label))
    subs <- set_names_from_varlabel(subs)
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
#' @importFrom purrr list_rbind
#' @importFrom tibble rowid_to_column tibble
#' @importFrom dm as_dm dm_add_pk dm_add_fk
#'
#' @param x a \code{\link{kobo_asset}} or character, the asset
#' if missing, default to number of submissions divided by 5
#' @param all_versions logical, whether or not to include data from all form versions.
#' Default to `TRUE`. If `FALSE`, it uses the latest version of the form.
#' @param colnames_label logical, whether or not to use variable labels in lieu of column names based on form question names. Default to `FALSE`.
#' @param select_multiple_label logical, whether or not to replace select_multiple columns values by labels. Default to `FALSE`.
#' @param lang character, language for the variable and value labels.
#' @param paginate logical, split submissions by page_size. Default to `FALSE`.
#' @param page_size integer, number of submissions per page.
#'
#' @details \code{\link{kobo_data}} is the main function of \code{robotoolbox}, it is used
#' pull submissions from your Kobotoolbox survey.
#'
#' @return A  \code{data.frame} or A  \code{dm} object if you have repeat groups
#'
#' @examples
#' \dontrun{
#' kobo_setup() # setup using your url and token
#' uid <- "a9cwEQcbWqWzA5hzkjRUWi" # pick a valid uid
#' asset <- kobo_asset(uid)
#' subs <- kobo_data(asset) ## kobo_submissions(asset)
#'
#' if (require(dplyr)) {
#'  library(dplyr)
#'  glimpse(subs)
#'  }
#' }
#'
#' @export
kobo_data <- function(x, lang,
                      all_versions,
                      colnames_label,
                      select_multiple_label,
                      paginate, page_size)
  UseMethod("kobo_data")

#' @rdname kobo_data
#' @export
kobo_submissions <- function(x, lang,
                             all_versions,
                             colnames_label,
                             select_multiple_label,
                             paginate, page_size)
  UseMethod("kobo_submissions")

#' @export
kobo_data.kobo_asset <- function(x, lang = NULL,
                                 all_versions = TRUE,
                                 colnames_label = FALSE,
                                 select_multiple_label = FALSE,
                                 paginate = FALSE,
                                 page_size = NULL) {
  if (x$asset_type != "survey")
    abort("You can just read data from survey")
  size <- x$deployment__submission_count
  if (size > 0) {
    res <- kobo_data_(x = x,
                      lang = lang,
                      all_versions = all_versions,
                      colnames_label = colnames_label,
                      select_multiple_label = select_multiple_label,
                      paginate = paginate,
                      page_size = page_size,
                      size = size)
  } else {
    form <- kobo_form(x)
    cn <- kobo_form_names_(form)
    res <- empty_tibble_(cn)
  }
  res
}

#' @rdname kobo_data
#' @export
kobo_submissions.kobo_asset <- kobo_data.kobo_asset

#' @export
kobo_data.character <- function(x, lang = NULL,
                                all_versions = TRUE,
                                colnames_label = FALSE,
                                select_multiple_label = FALSE,
                                paginate = FALSE,
                                page_size = NULL) {
  if (!assert_uid(x))
    abort(message = "Invalid asset uid")
  kobo_data(kobo_asset(x),
            lang = lang,
            all_versions = all_versions,
            colnames_label = colnames_label,
            select_multiple_label = select_multiple_label,
            paginate = paginate,
            page_size = page_size)
}

#' @rdname kobo_data
#' @export
kobo_submissions.character <- kobo_data.character

#' @export
kobo_data.default <- function(x, lang = NULL,
                              all_versions = TRUE,
                              colnames_label = FALSE,
                              select_multiple_label = FALSE,
                              paginate = FALSE,
                              page_size = NULL) {
  abort("You need to use a 'kobo_asset' or a valid asset uid")
}

#' @rdname kobo_data
#' @export
kobo_submissions.default <- kobo_data.default
