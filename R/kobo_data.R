#' @importFrom rlang warn
#' @noRd
kobo_data_ <- function(
  x,
  paginate,
  page_size,
  size,
  lang,
  select_multiple_label,
  select_multiple_sep,
  colnames_label,
  all_versions,
  progress,
  query = NULL,
  fields = NULL
) {
  if (isTRUE(progress)) {
    cli_progress_step("Downloading data")
  }

  if (!is.null(page_size) && (!is.numeric(page_size) || page_size < 1)) {
    abort("`page_size` must be a positive integer", call = NULL)
  }

  api_max_limit <- api_max_limit_()

  if (!is.null(page_size) && page_size > api_max_limit) {
    warn(
      paste0(
        "`page_size` exceeds the API maximum of ",
        api_max_limit,
        "; capping at ",
        api_max_limit
      ),
      call = NULL
    )
    page_size <- api_max_limit
  }

  fields_json <- format_fields_(fields)

  paginate <- isTRUE(paginate) || !is.null(page_size) || size > api_max_limit

  if (paginate) {
    if (is.null(page_size)) {
      page_size <- ifelse(size > api_max_limit, api_max_limit, max(size %/% 2, 1L))
    }
    subs <- get_subs_async(
      x$uid,
      size,
      page_size,
      query = query,
      fields = fields_json
    )
  } else {
    subs <- get_subs(x$uid, query = query, fields = fields_json)
  }

  if (isTRUE(progress)) {
    cli_progress_step("Processing data")
  }
  form <- kobo_form_version_(subs, x, all_versions = all_versions)
  cn <- kobo_form_names_(form, sep = select_multiple_sep)
  klang <- kobo_lang(x)
  if (is.null(lang) || !lang %in% klang) {
    lang <- klang[1]
  }

  repeat_in_data <- "begin_repeat" %in%
    form$type &&
    any(unique(form$name[form$type %in% "begin_repeat"]) %in% names(subs))

  if (repeat_in_data) {
    names_list <- kobo_form_name_to_list_(
      filter(form, .data$lang == lang),
      sep = select_multiple_sep
    )
    pc_choices <- precompute_val_labels_(form, lang)
    pc_var_labels <- precompute_var_labels_(form, lang)
    subs <- dedup_vars_(subs, all_versions = all_versions)
    subs <- set_names(subs, make_unique_names_)
    subs <- c(
      list(main = rowid_to_column(subs, "_index")),
      kobo_extract_repeat_tbl(
        subs,
        form,
        all_versions = all_versions
      )
    )
    nm <- names(subs)
    names_list <- names_list[nm]
    subs <- lapply(nm, function(n) {
      d <- subs[[n]]
      cn_list <- names_list[[n]]
      postprocess_data_(
        x = d,
        form = form,
        lang = lang,
        select_multiple_label = select_multiple_label,
        select_multiple_sep = select_multiple_sep,
        cn = cn_list,
        precomputed_choices = pc_choices,
        precomputed_var_labels = pc_var_labels,
        fields = fields
      )
    })
    subs <- setNames(subs, nm)
    subs <- as_dm(subs)
    subs <- dm_add_pk(subs, "main", "_index")
    p <- length(subs)
    if (p >= 2) {
      for (j in 2:p) {
        tbl_nm <- names(subs)[j]
        ref_tbl_nm <- unique(subs[[j]][["_parent_table_name"]])
        subs <- dm_add_pk(subs, {{ tbl_nm }}, "_index")
        subs <- dm_add_fk(
          subs,
          {{ tbl_nm }},
          "_parent_index",
          {{ ref_tbl_nm }}
        )
      }
    }
    if (isTRUE(colnames_label)) {
      subs <- set_names_from_varlabel_dm_(subs)
    }
  } else {
    subs <- dedup_vars_(subs, all_versions = all_versions)
    subs <- set_names(subs, make_unique_names_)
    subs <- postprocess_data_(
      x = subs,
      form = form,
      lang = lang,
      select_multiple_label = select_multiple_label,
      select_multiple_sep = select_multiple_sep,
      cn = cn,
      fields = fields
    )
    if (isTRUE(colnames_label)) {
      subs <- set_names_from_varlabel_(subs)
    }
  }
  subs
}

#' Get all submissions from a KoboToolbox API asset
#'
#' Get all submissions from a KoboToolbox API asset through a \code{kobo_asset} or
#' asset unique identifier.
#'
#' @name kobo_data
#'
#' @importFrom tidyselect starts_with everything
#' @importFrom dplyr select
#' @importFrom purrr list_rbind
#' @importFrom tibble rowid_to_column tibble
#' @importFrom dm as_dm dm_add_pk dm_add_fk
#' @importFrom cli cli_progress_step
#'
#' @param x the asset uid or the \code{kobo_asset} object.
#' @param lang character, form language used for the variable and value labels.
#' @param all_versions logical, whether or not to include submissions from all form versions.
#' Default to `TRUE`. If `FALSE`, it uses the data from the latest version of the form.
#' @param colnames_label logical, whether or not to use variable labels
#' in lieu of column names based on form question names. Default to `FALSE`.
#' @param select_multiple_label logical, whether or not to replace select_multiple columns values by labels. Default to `FALSE`.
#' @param select_multiple_sep character, column and choices separator for newly created dummy variables. Default to "_".
#' @param progress logical, whether or not you want to see the progess via message.
#' Default to `FALSE`.
#' @param paginate logical, split submissions by page_size. Default to `NULL`.
#' @param page_size integer, number of submissions per page.
#' @param query character, a MongoDB-style query string to filter submissions
#'   server-side. For example, `'{"field_name": "value"}'` to return only
#'   submissions where `field_name` equals `"value"`. The query filters
#'   submissions (main table rows) based on top-level fields only. You cannot
#'   filter on fields inside a repeat group. All repeat group rows belonging to
#'   matching submissions are returned. Default to `NULL` (no filtering).
#' @param fields character vector of field names to return. When provided, only
#'   these fields (plus `__version__`) are fetched from the server. The server
#'   also includes system fields (`_id`, `_uuid`, etc.) regardless. For forms
#'   with repeat groups: use the repeat group name as a field to include or
#'   exclude it. A repeat group is always fetched in full — you cannot select
#'   individual columns within a child table (e.g. `"my_repeat/age"` is not
#'   supported). If all repeat groups are omitted from `fields`, a plain
#'   `data.frame` is returned instead of a `dm` object.
#'   Default to `NULL` (all fields).
#'
#' @details \code{\link{kobo_data}} is the main function of \code{robotoolbox}, it is used
#' pull submissions from your Kobotoolbox survey. The main result is a \code{data.frame}
#' for regular form and you have a \code{dm} for a form with repeating groups of questions.
#'
#' @returns A \code{data.frame} or A \code{dm} object if you have a repeating
#' group of questions. It contains the responses from the Kobotoolbox survey.
#'
#' @examples
#' \dontrun{
#' # Use your own URL and token
#' kobo_setup(url = "https://kf.kobotoolbox.org/",
#'            token = "9et1814c285w094f6v9bd629df47a1a0e81x53a0")
#' # Use your own unique identifier
#' uid <- "a9cwEQcbWqWzA5hzkjRUWi"
#' asset <- kobo_asset(uid)
#' subs <- kobo_data(asset)
#'
#' # Filter submissions server-side with a MongoDB query
#' filtered <- kobo_data(asset, query = '{"pet_yesno": "1"}')
#'
#' # Select specific fields
#' partial <- kobo_data(asset, fields = c("full_name", "pet_yesno"))
#'
#' if (require(dplyr)) {
#'  library(dplyr)
#'  glimpse(subs)
#'  }
#' }
#'
#' @export
kobo_data <- function(
  x,
  lang,
  all_versions,
  colnames_label,
  select_multiple_label,
  select_multiple_sep,
  progress,
  paginate,
  page_size,
  query,
  fields
) {
  UseMethod("kobo_data")
}

#' @name kobo_data
#' @export
kobo_submissions <- function(
  x,
  lang,
  all_versions,
  colnames_label,
  select_multiple_label,
  select_multiple_sep,
  progress,
  paginate,
  page_size,
  query,
  fields
) {
  UseMethod("kobo_submissions")
}

#' @export
kobo_data.kobo_asset <- function(
  x,
  lang = NULL,
  all_versions = TRUE,
  colnames_label = FALSE,
  select_multiple_label = FALSE,
  select_multiple_sep = "_",
  progress = FALSE,
  paginate = NULL,
  page_size = NULL,
  query = NULL,
  fields = NULL
) {
  if (x$asset_type != "survey") {
    abort("You can only read data from a survey")
  }
  validate_query_(query)
  validate_fields_(fields)
  size <- x$deployment__submission_count
  if (!is.null(query) && !is.null(size) && size > 0) {
    size <- get_filtered_count_(x$uid, query)
  }
  if (!is.null(size) && size > 0) {
    res <- kobo_data_(
      x = x,
      lang = lang,
      all_versions = all_versions,
      colnames_label = colnames_label,
      select_multiple_label = select_multiple_label,
      select_multiple_sep = select_multiple_sep,
      progress = progress,
      paginate = paginate,
      page_size = page_size,
      size = size,
      query = query,
      fields = fields
    )
  } else {
    form <- kobo_form(x)
    klang <- kobo_lang(x)
    if (is.null(lang) || !lang %in% klang) lang <- klang[1]
    has_repeat <- "begin_repeat" %in% form$type

    if (has_repeat) {
      names_list <- kobo_form_name_to_list_(
        filter(form, .data$lang == lang),
        sep = select_multiple_sep
      )
      if (!is.null(fields)) {
        names_list[["main"]] <- intersect(names_list[["main"]], fields)
        keep <- c("main", intersect(names(names_list), fields))
        names_list <- names_list[keep]
      }
      tbls <- lapply(names_list, empty_tibble_)
      res <- as_dm(tbls)
      if (isTRUE(colnames_label)) {
        res <- set_names_from_varlabel_empty_dm_(res, form, lang)
      }
    } else {
      cn <- kobo_form_names_(form, sep = select_multiple_sep)
      if (!is.null(fields)) cn <- intersect(cn, fields)
      cn <- na.omit(cn)
      res <- empty_tibble_(cn)
      if (isTRUE(colnames_label)) {
        res <- set_names_from_varlabel_empty_(res, form, lang)
      }
    }
  }
  res
}

#' @name kobo_data
#' @export
kobo_submissions.kobo_asset <- kobo_data.kobo_asset

#' @export
kobo_data.character <- function(
  x,
  lang = NULL,
  all_versions = TRUE,
  colnames_label = FALSE,
  select_multiple_label = FALSE,
  select_multiple_sep = "_",
  progress = FALSE,
  paginate = NULL,
  page_size = NULL,
  query = NULL,
  fields = NULL
) {
  if (!assert_uid(x)) {
    abort(message = "Invalid asset uid")
  }
  kobo_data(
    kobo_asset(x),
    lang = lang,
    all_versions = all_versions,
    colnames_label = colnames_label,
    select_multiple_label = select_multiple_label,
    select_multiple_sep = select_multiple_sep,
    progress = progress,
    paginate = paginate,
    page_size = page_size,
    query = query,
    fields = fields
  )
}

#' @name kobo_data
#' @export
kobo_submissions.character <- kobo_data.character

#' @export
kobo_data.default <- function(
  x,
  lang = NULL,
  all_versions = TRUE,
  colnames_label = FALSE,
  select_multiple_label = FALSE,
  select_multiple_sep = "_",
  progress = FALSE,
  paginate = NULL,
  page_size = NULL,
  query = NULL,
  fields = NULL
) {
  abort("You need to use a 'kobo_asset' or a valid asset uid")
}

#' @name kobo_data
#' @export
kobo_submissions.default <- kobo_data.default

#' @noRd
kobo_attachment_download_ <- function(attachments, folder, overwrite, n_retry) {
  if (!dir.exists(folder)) {
    abort(
      paste(
        folder,
        "folder does not exist, create it first!"
      ),
      call = NULL
    )
  }
  if (nrow(attachments) == 0) {
    return(invisible(character()))
  }

  urls <- attachments |>
    mutate(
      id = .data$`_id`,
      url = .data$download_url,
      att_uid = .data$uid,
      fname = .data$media_file_basename,
      fname_id = paste0(.data$att_uid, "_", .data$fname),
      path = file.path(folder, .data$fname_id),
      .keep = "none"
    ) |>
    distinct()

  headers <- list(
    Authorization = paste(
      "Token",
      Sys.getenv("KOBOTOOLBOX_TOKEN")
    )
  )

  if (isFALSE(overwrite)) {
    urls <- filter(
      urls,
      !.data$fname_id %in% list.files(folder)
    )
  }

  if (nrow(urls) > 0) {
    reqs <- lapply(urls$url, function(url) {
      req <- HttpRequest$new(url, headers = headers)
      req$retry(
        "get",
        times = n_retry,
        retry_only_on = c(500, 502, 503),
        terminate_on = 404
      )
    })
    res <- AsyncQueue$new(
      .list = reqs,
      bucket_size = 15L,
      sleep = 0.05
    )
    res$request()
    cond <- res$status_code() >= 300L
    if (any(cond)) {
      msg <- res$content()[cond]
      abort(error_msg(msg[[1]]), call = NULL)
    }
    walk2(
      res$content(),
      urls$path,
      \(x, y) writeBin(x, con = y)
    )
  }
  invisible(urls$path)
}

#' Download submitted files associated to KoboToolbox API asset
#'
#' Download submitted files associated to a KoboToolbox API asset
#'
#' @importFrom crul AsyncQueue
#' @importFrom purrr walk2
#'
#' @name kobo_attachment_download
#'
#' @param x the asset uid or the \code{kobo_asset} object.
#' @param folder character, the folder where you store the downloaded files.
#' @param progress logical, whether or not you want to see the progess via message.
#' Default to `FALSE`.
#' @param overwrite logical, whether or not you want to overwrite existing media files.
#' Default to `TRUE`.
#' @param n_retry integer, Number of time you should retry the failed request.
#' Default to 3L.
#'
#' @returns Silently returns a vector of files paths.
#'
#' @examples
#' \dontrun{
#' kobo_setup()
#' uid <- "a9cwEQcbWqWzA5hzkjRUWi"
#' kobo_attachment_download(uid, folder = tempdir())
#' }
#'
#' @export
kobo_attachment_download <- function(x, folder, progress, overwrite, n_retry) {
  UseMethod("kobo_attachment_download")
}

#' @export
kobo_attachment_download.character <- function(
  x,
  folder,
  progress = FALSE,
  overwrite = TRUE,
  n_retry = 3L
) {
  if (isTRUE(progress)) {
    cli_progress_step("Listing files")
  }
  subs <- get_attachment_url_(x)
  if (isTRUE(progress)) {
    cli_progress_step("Downloading files")
  }
  kobo_attachment_download_(
    subs,
    folder = folder,
    overwrite = overwrite,
    n_retry = n_retry
  )
}

#' @export
kobo_attachment_download.kobo_asset <- function(
  x,
  folder,
  progress = FALSE,
  overwrite = TRUE,
  n_retry = 3L
) {
  if (isTRUE(progress)) {
    cli_progress_step("Listing files")
  }
  subs <- get_attachment_url_(x$uid)
  if (isTRUE(progress)) {
    cli_progress_step("Downloading files")
  }
  kobo_attachment_download_(
    subs,
    folder = folder,
    overwrite = overwrite,
    n_retry = n_retry
  )
}

#' @export
kobo_attachment_download.default <- function(
  x,
  folder,
  progress = FALSE,
  overwrite = TRUE,
  n_retry = 3L
) {
  abort("You need to use a 'kobo_asset' or a valid asset uid")
}
