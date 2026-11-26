#' @noRd
kobo_audit_ <- function(uid, progress = FALSE) {
  if (isTRUE(progress))
    cli_progress_step("Listing audit files")
  audit_meta <- get_audit_url_(uid)
  if (nrow(audit_meta) == 0L)
    return(empty_audit_())
  headers <- list(
    Authorization = paste("Token", Sys.getenv("KOBOTOOLBOX_TOKEN")),
    `User-Agent` = user_agent_()
  )
  reqs <- lapply(audit_meta$download_url, function(url) {
    req <- HttpRequest$new(url,
                           headers = headers)
    req$retry("get",
              times = 3L,
              retry_only_on = retry_codes_,
              terminate_on = 404)
  })
  tick <- NULL
  if (isTRUE(progress)) {
    cli_progress_step("Downloading audit data")
    tick <- progress_ticker_("Downloading audit files", total = length(reqs))
  }
  texts <- async_fetch_batches_(reqs, handle = audit_text_, tick = tick)
  progress_done_(tick)

  if (isTRUE(progress))
    cli_progress_step("Processing audit data")

  parse_audit_(audit_meta, texts)
}

#' Text of an audit file, read as UTF-8
#'
#' A byte that is not valid UTF-8 is written as its code, such as `<ff>`, so
#' it does not discard the file.
#' @noRd
audit_text_ <- function(content, i) {
  if (length(content) == 0L) {
    return("")
  }
  iconv(readBin(content, character()), from = "UTF-8", to = "UTF-8", sub = "byte")
}

#' Read the audit files of a survey
#'
#' @param audit_meta data.frame with the `_id` of each submission and the
#'   `download_url` of its audit file.
#' @param texts list, the content of each audit file.
#' @returns A tibble with the columns of `empty_audit_()`. Columns that a file
#'   does not have, such as the location ones, are `NA`.
#' @importFrom data.table fread
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @importFrom cli cli_warn
#' @noRd
parse_audit_ <- function(audit_meta, texts) {
  char_cols <- c("event", "node", "old-value", "new-value", "user", "change-reason")
  data <- lapply(seq_along(texts), function(i) {
    txt <- texts[[i]]
    if (length(txt) != 1L || is.na(txt) || !nzchar(trimws(txt))) {
      return(NULL)
    }
    header <- strsplit(sub("[\r\n].*", "", trimws(txt, "left")), ",", fixed = TRUE)[[1]]
    header <- gsub("^\\s*\"?|\"?\\s*$", "", header)
    ## A row with fewer values than the header is kept, with `NA` for the
    ## values it lacks. Any other problem of the file is reported with the
    ## submission it belongs to.
    res <- withCallingHandlers(
      fread(
        text = txt,
        colClasses = list(character = intersect(char_cols, header)),
        fill = TRUE,
        header = TRUE,
        data.table = FALSE
      ),
      warning = function(w) {
        cli_warn(c(
          "The audit log of submission {audit_meta[[\"_id\"]][i]} may be incomplete.",
          x = conditionMessage(w)
        ))
        invokeRestart("muffleWarning")
      }
    )
    required <- c("event", "node", "start", "end")
    if (!all(required %in% names(res))) {
      cli_warn(c(
        "The audit log of submission {audit_meta[[\"_id\"]][i]} is left out.",
        x = "Its file does not have the columns {.field {required}}."
      ))
      return(NULL)
    }
    extra <- setdiff(names(res), header)
    if (length(extra) > 0L) {
      cli_warn(c(
        "The audit log of submission {audit_meta[[\"_id\"]][i]} may be incomplete.",
        x = "A row has more values than the header of the file."
      ))
      res <- res[setdiff(names(res), extra)]
    }
    if (nrow(res) == 0L) NULL else res
  })
  keep <- !vapply(data, is.null, logical(1))
  if (!any(keep)) {
    return(empty_audit_())
  }
  res <- audit_meta[keep, setdiff(names(audit_meta), "download_url")]
  res$data <- data[keep]
  res <- unnest(res, "data")
  template <- empty_audit_()
  optional <- c("latitude", "longitude", "accuracy", "old-value", "new-value",
                "user", "change-reason")
  for (col in setdiff(optional, names(res))) {
    res[[col]] <- template[[col]][rep(NA_integer_, nrow(res))]
  }
  res <- mutate(res, name = basename(.data$node), .before = "start",
                start_int = .data$start,
                start = as.POSIXct(.data$start_int/1000, origin = "1970-01-01"),
                end_int = .data$end,
                end = as.POSIXct(.data$end_int/1000, origin = "1970-01-01"))
  res[c(names(template), setdiff(names(res), names(template)))]
}

#' Get all audit logs data from a KoboToolbox survey
#'
#' Get all audit logs data from a KoboToolbox survey through a `kobo_asset` or
#' asset unique identifier.
#'
#' @name kobo_audit
#'
#' @importFrom RcppSimdJson fparse
#' @importFrom purrr list_rbind
#' @importFrom dplyr mutate select
#' @importFrom tidyr unnest
#' @importFrom data.table fread
#' @importFrom readr type_convert
#'
#' @inheritParams kobo_data
#'
#' @returns A `data.frame` with one row per audit event. It always has the
#' following columns, and the columns of an audit option that the form does
#' not use are `NA`.
#'
#' - `_id` the `_id` of the submission in [kobo_data()], added by robotoolbox.
#' - `event` the action that took place, such as form start, form exit,
#'   question, group questions, end screen, and device or metadata audit.
#' - `node` the path of the question or group related to the event.
#' - `name` the name of the question or group, added by robotoolbox to match
#'   the column names of [kobo_data()].
#' - `start_int` and `end_int` the times when the event started and ended, in
#'   milliseconds since 1970-01-01 UTC, as `integer64` (bit64 package).
#' - `start` and `end` the same times as `POSIXct`.
#' - `latitude`, `longitude` and `accuracy` the location of the device when
#'   the event occurred and its accuracy (audit option `location-priority`).
#' - `old-value` and `new-value` the answer before and after a change (audit
#'   option `track-changes`).
#' - `user` the username of the data collector (audit option `identify-user`).
#' - `change-reason` the reason given when saving changes to a form (audit
#'   option `track-changes-reasons`).
#'
#' When no submission has an audit log, the result has zero rows and all the
#' columns above.
#'
#' @family data functions
#'
#' @examples
#' \dontrun{
#' kobo_setup()
#' uid <- "a9cwEQcbWqWzdA5eqkjRUWi"
#' asset <- kobo_asset(uid)
#' audit <- kobo_audit(asset)
#'
#' if (require(dplyr)) {
#'  library(dplyr)
#'  glimpse(audit)
#'  }
#' }
#'
#' @export
kobo_audit <- function(x, progress = FALSE) {
  UseMethod("kobo_audit")
}

#' @importFrom purrr list_rbind
#' @importFrom dplyr filter
#' @importFrom rlang abort
#' @export
kobo_audit.kobo_asset <- function(x, progress = FALSE) {
  if (isTRUE(progress))
    cli_progress_step("Checking audit data availability")
  form <- kobo_form(x)
  if (!"audit" %in% form$name) {
    asset_version_list <- kobo_asset_version_list(x$uid)
    version <- unique(asset_version_list$uid[asset_version_list$deployed])
    if (length(version) > 0) {
      form <- lapply(version, \(v) kobo_form(x, v))
      form <- dt2tibble(rbindlist(form, fill = TRUE))
    }
  }

  if (!any("audit" %in% form$name))
    abort("`audit` not enabled in the current version of the survey",
          call = NULL)
  kobo_audit_(x$uid, progress = progress)
}

#' @export
kobo_audit.character <- function(x, progress = FALSE) {
  if (!assert_uid(x))
    abort(message = "Invalid asset uid")
  kobo_audit(kobo_asset(x), progress = progress)
}

#' @export
kobo_audit.default <- function(x, progress = FALSE) {
  abort("You need to use a 'kobo_asset' or an asset uid")
}
