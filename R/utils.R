#' @noRd
#' @importFrom utils packageVersion
user_agent_ <- function() {
  robotoolbox_version <- packageVersion("robotoolbox")
  os <- Sys.info()[["sysname"]]
  os_version <- paste(Sys.info()[["release"]],
                      Sys.info()[["version"]])
  r_version <- paste0(R.version$major, ".",
                      R.version$minor,
                      ifelse(R.version$status == "", "",
                             paste0("-", R.version$status)))
  header <- paste0("robotoolbox/",
                   robotoolbox_version, " (", os, "/",
                   os_version, "; ", "R/", r_version, ")")
  header
}

#' @noRd
xget <- function(path, args = list(), n_retry = 3L, ...) {
  headers <- list(Authorization = paste("Token",
                                        Sys.getenv("KOBOTOOLBOX_TOKEN")),
                  `User-Agent` = user_agent_())
  cli <- crul::HttpClient$new(Sys.getenv("KOBOTOOLBOX_URL"),
                              headers = headers, opts = list(...))
  res <- cli$retry("GET", path = path,
                   query = args,
                   times = n_retry,
                   terminate_on = 404)
  res$raise_for_status()
  res$raise_for_ct_json()
  res$parse("UTF-8")
}

#' @importFrom RcppSimdJson fparse
#' @noRd
get_subs <- function(uid, args = list(), ...) {
  path <- paste0("api/v2/assets/", uid, "/data.json")
  res <- xget(path = path, args = args, ...)
  res <- fparse(res, max_simplify_lvl = "data_frame")
  res <- res$result
  as_tibble(res)
}

#' @importFrom glue glue glue_data
#' @importFrom tibble tibble
#' @noRd
build_subs_urls <- function(uid, size, chunk_size = NULL) {
  if (chunk_size >= size || is.null(chunk_size)) {
    urls <- glue("api/v2/assets/{uid}/data.json", uid = uid)
  } else {
    start <- seq(0, size, by = chunk_size)
    limit <- rep(chunk_size, length(start) - 1)
    last_limit <- size %% chunk_size
    last_limit <- ifelse(last_limit, last_limit, chunk_size)
    limit <- c(limit, last_limit)
    df <- tibble(start = start, limit = limit, uid = uid)
    template <- "api/v2/assets/{uid}/data.json?start={start}&limit={limit}"
    urls <- glue_data(df,
                      template)
  }
  file.path(Sys.getenv("KOBOTOOLBOX_URL"), urls)
}

#' @importFrom crul AsyncQueue HttpRequest
#' @importFrom RcppSimdJson fparse
#' @importFrom data.table rbindlist setattr
#' @importFrom tibble as_tibble
#' @importFrom dplyr case_when
#' @noRd
get_subs_async <- function(uid, size, chunk_size = NULL, ...) {
  headers <- list(Authorization = paste("Token",
                                        Sys.getenv("KOBOTOOLBOX_TOKEN")))
  urls <- build_subs_urls(uid, size, chunk_size)
  reqs <- lapply(urls, function(url) {
    HttpRequest$new(url,
                    headers = headers,
                    opts = list(...))$get()
  })
  sleep <- case_when(
    size > 30000 ~ 2.5,
    size > 15000 ~ 1.5,
    size > 5000 ~ 1,
    size > 1000 ~ 0.5,
    TRUE ~ 0.25
  )
  res <- AsyncQueue$new(.list = reqs,
                        bucket_size = Inf,
                        sleep = sleep)
  res$request()
  cond <- any(res$status_code() > 200L)
  if (cond)
    stop("A request failed! check the settings and try again",
         call. = FALSE)
  res <- res$parse(encoding = "UTF-8")
  res <- fparse(res,
                max_simplify_lvl = "data_frame")
  res <- rbindlist(lapply(res, function(r) r$results),
                   fill = TRUE)
  res <- as_tibble(res)
  attr(res, ".internal.selfref") <- NULL
  res
}

#' @importFrom glue glue glue_data
#' @importFrom tibble tibble
#' @noRd
build_assets_urls <- function(limit, count) {
  if (limit >= count) {
    urls <- "api/v2/assets/?format=json&metadata=on"
  } else {
    idx <- seq.int(limit, count, by = limit)
    df <- tibble(limit = limit, offset = idx[idx < count])
    template <- "api/v2/assets/?format=json&limit={limit}&metadata=on&offset={offset}"
    urls <- glue_data(df, template)
    urls <- c(glue_data(tibble(limit = limit),
                        "api/v2/assets/?format=json&limit={limit}&metadata=on"),
              urls)
  }
  file.path(Sys.getenv("KOBOTOOLBOX_URL"), urls)
}

#' @importFrom crul AsyncQueue HttpRequest
#' @importFrom RcppSimdJson fparse
#' @importFrom data.table rbindlist setattr
#' @importFrom tibble as_tibble
#' @importFrom dplyr case_when
#' @noRd
get_asset_list_async <- function(limit, count, ...) {
  headers <- list(Authorization = paste("Token",
                                        Sys.getenv("KOBOTOOLBOX_TOKEN")))
  urls <- build_assets_urls(limit, count)
  reqs <- lapply(urls, function(url) {
    HttpRequest$new(url,
                    headers = headers,
                    opts = list(...))$get()
  })
  sleep <- 0.05
  res <- AsyncQueue$new(.list = reqs,
                        bucket_size = Inf,
                        sleep = sleep)
  res$request()
  cond <- any(res$status_code() > 200L)
  if (cond)
    stop("Request failed! check the settings and try again",
         call. = FALSE)
  res <- res$parse(encoding = "UTF-8")
  res <- fparse(res,
                max_simplify_lvl = "list")
  res <- lapply(res, function(r) asset_list_to_tbl(r$results))
  res <- rbindlist(res, fill = TRUE)
  res <- as_tibble(res)
  attr(res, ".internal.selfref") <- NULL
  res
}

#' @importFrom tibble tibble
#' @noRd
asset_list_to_tbl <- function(x) {
  tibble(uid = map_chr2(x, "uid"),
         name = map_chr2(x, "name"),
         asset_type = map_chr2(x, "asset_type"),
         owner_username = map_chr2(x, "owner__username"),
         date_created = parse_kobo_date(map_chr2(x, "date_created")),
         date_modified = parse_kobo_date(map_chr2(x, "date_modified")),
         submissions = map_int2(x, "deployment__submission_count"))
}

#' @importFrom RcppSimdJson fparse
#' @noRd
get_form_media <- function(uid) {
  headers <- list(Authorization = paste("Token",
                                        Sys.getenv("KOBOTOOLBOX_TOKEN")))
  path <- paste0("api/v2/assets/", uid, "/files/?file_type=form_media")
  cli <- crul::HttpClient$new(Sys.getenv("KOBOTOOLBOX_URL"),
                              headers = headers)
  res <- cli$get(path = path)
  res$raise_for_status()
  res$raise_for_ct_json()
  res <- fparse(res$parse("UTF-8"),
                max_simplify_lvl = "data_frame")
  fname <- map_chr2(res$results$metadata, "filename")
  mtype <- map_chr2(res$results$metadata, "mimetype")
  uid <- res$results$uid
  url <- res$results$content
  tibble(uid = uid, url = url, filename = fname, mimetype = mtype)
}

#' @noRd
download_form_media <- function(url, filename) {
  headers <- list(Authorization = paste("Token",
                                        Sys.getenv("KOBOTOOLBOX_TOKEN")))
  cli <- crul::HttpClient$new(url, headers = headers)
  out <- cli$get(disk = filename)
  out$raise_for_status()
  invisible(filename)
}

#' @importFrom data.table fread
#' @noRd
form_media_one_csv_tbl <- function(url) {
  headers <- list(Authorization = paste("Token",
                                        Sys.getenv("KOBOTOOLBOX_TOKEN")))
  cli <- crul::HttpClient$new(url, headers = headers)
  out <- cli$get()
  out$raise_for_status()
  out <- fread(out$parse(encoding = "UTF-8"))
  out <- as_tibble(out)
  attr(out, ".internal.selfref") <- NULL
  out
}

## Find a clever to label it, update example
#' @noRd
form_media_csv_tbl <- function(uid) {
  meta <- get_form_media(uid)
  df <- lapply(meta$url,
               function(x) form_media_one_csv_tbl(x))
  df
}

#' @noRd
map_chr2 <- function(x, key)
  vapply(x, function(l) as.character(l[[key]]), character(1))

#' @noRd
map_if_chr2 <- function(x, key)
  vapply(x,
         function(l)
           if (length(l) > 0) as.character(l[[key]]) else NA_character_,
         character(1))

#' @noRd
map_int2 <- function(x, key)
  vapply(x, function(l) l[[key]], integer(1))

#' @noRd
parse_kobo_date <- function(x)
  as.POSIXct(x,
             format = "%Y-%m-%dT%H:%M:%OS",
             tz = "GMT")
#' @noRd
is_list_cols <- function(x)
  which(vapply(x, is.list, logical(1)))

#' @noRd
is_null_recursive <- function(x)
  is.null(x) | all(vapply(x, is.null, logical(1)))

#' @noRd
print_list_res <- function(x)
  paste(unlist(x), collapse = ", ")

#' @noRd
drop_nulls <- function(x) {
  x <- Filter(Negate(is_null_recursive), x)
  lapply(x, function(x)
    if (is.list(x)) drop_nulls(x) else x)
}

#' @noRd
null2char <- function(x) {
  if (is.null(x))
    return("")
  x
}

#' @importFrom stringi stri_sort
#' @importFrom stats na.omit
#' @noRd
col2choices <- function(x, form, col) {
  idx <- which(form$name %in% col)
  if (length(idx) > 0) {
    idx <- idx[1]
    uv <- unique(form$choices[[idx]]$value_name)
  } else {
    uv <- unique(unlist(strsplit(x[[col]], "\\s+")))
    uv <- trimws(na.omit(uv))
  }
  stri_sort(uv,
            na_last = TRUE,
            locale = "en_US",
            numeric = TRUE)
}

#' @importFrom data.table as.data.table alloc.col `:=` chmatch set
#' @importFrom stringi stri_detect_regex
#' @importFrom tibble as_tibble
#' @importFrom stats na.omit
#' @noRd
fast_dummy_cols <- function(x, form, cols) {
  x <- as.data.table(x)
  for (col in cols) {
    y <- x[[col]]
    uv <- col2choices(x, form, col)
    new_names <- paste0(col, "_", uv)
    alloc.col(x, ncol(x) + length(uv))
    x[, (new_names) := 0L]
    x[which(is.na(y)), (new_names) := NA_integer_]
    for (iter in seq_along(uv)) {
      j <- paste0(col, "_", uv[iter])
      i <- which(stri_detect_regex(y,
                                   paste0("\\b", uv[iter], "\\b")))
      set(x, i = i, j = j, value = 1L)
    }
  }
  as_tibble(x)
}

#' @noRd
dummy_from_form_ <- function(x, form) {
  nm <- unique(form$name[form$type %in% "select_multiple"])
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    x <- fast_dummy_cols(x, form, nm)
  } else {
    x
  }
  x
}

#' @noRd
make_unique_names_ <- function(x, ...)
  make.unique(basename(x), sep = "_")

#' @importFrom purrr some map modify_if
#' @importFrom rlang set_names
#' @noRd
name_repair_ <- function(x) {
  if (is.null(x))
    return(NULL)
  modify_if(set_names(x, make_unique_names_),
            ~ some(., is.data.frame),
            ~ lapply(., name_repair_))
}

#' @importFrom purrr modify_if
#' @importFrom tibble tibble rowid_to_column
#' @importFrom data.table rbindlist
#' @noRd
extract_repeat_tbl <- function(x, form) {
  res <- list()
  nm <- unique(form$name[form$type %in% "begin_repeat"])
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    res <- setNames(lapply(nm, function(n) {
      res <- x[[n]]
      res <- modify_if(res, ~ !is.data.frame(.), ~ tibble())
      res <- rbindlist(res,
                       idcol = "_parent_index",
                       fill = TRUE)
      res <- tibble(res)
      rowid_to_column(res, "_index")
    }), nm)
  }
  res
}

#' @importFrom rlang squash
#' @noRd
kobo_extract_repeat_tbl <- function(x, form) {
  if (is.null(x))
    return(NULL)
  x <- extract_repeat_tbl(x, form)
  x <- lapply(x, function(df) {
    df$`_parent_table_name` <- "main"
    df
  })
  res <- lapply(seq_along(x), function(i)
    if (is.data.frame(x[[i]])) {
      c(setNames(list(x[[i]]), names(x[i])),
        lapply(extract_repeat_tbl(x[[i]], form), function(df) {
               df$`_parent_table_name` <- names(x[i])
               df}))
      } else {
        list()
      })
  suppressWarnings(squash(res))
}

#' @importFrom dplyr mutate across filter
#' @importFrom stats setNames
#' @importFrom labelled set_value_labels
#' @importFrom rlang .data
#' @noRd
val_labels_from_form_ <- function(x, form, lang) {
  form <- filter(form,
                 .data$lang %in% !!lang,
                 .data$type %in% "select_one")
  nm <- unique(form$name)
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    form <- form[match(nm, form$name), ]
    choices <- form$choices
    choices <- lapply(choices, function(ch) {
      ch <- filter(ch,
                   .data$value_lang %in% !!lang)
      ch$value_label <- make.unique(ch$value_label, sep = "_")
      ch <- setNames(ch$value_name, ch$value_label)
      ch[!duplicated(ch)]
    })
    names(choices) <- nm
    labels <- choices[nm]
    x <- set_value_labels(mutate(x, across(nm, as.character)),
                          .labels = labels,
                          .strict = FALSE)
  } else {
    x
  }
  x
}

#' @importFrom dplyr mutate across filter
#' @importFrom stats setNames
#' @importFrom labelled set_value_labels
#' @importFrom rlang .data
#' @noRd
val_labels_from_form_external_ <- function(x, form, lang) {
  form <- filter(form,
                 .data$lang %in% !!lang,
                 grepl(".+from\\_file$", .data$type))
  nm <- unique(form$name)
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    form <- form[match(nm, form$name), ]
    choices <- form$choices
    choices <- lapply(choices, function(ch) {
      ch <- filter(ch,
                   .data$value_lang %in% !!lang)
      ch$value_label <- make.unique(ch$value_label, sep = "_")
      ch <- setNames(ch$value_name, ch$value_label)
      ch[!duplicated(ch)]
    })
    names(choices) <- nm
    labels <- choices[nm]
    x <- set_value_labels(mutate(x, across(nm, as.character)),
                          .labels = labels,
                          .strict = FALSE)
  } else {
    x
  }
  x
}

#' @importFrom labelled set_variable_labels
#' @importFrom stats setNames
#' @noRd
var_labels_from_form_ <- function(x, form, lang) {
  cond <- form$lang %in% lang & form$name %in% names(x)
  form <- form[cond, ]
  nm <- unique(form$name)
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    labels <- setNames(as.list(form$label), form$name)
    x <- set_variable_labels(x,
                             .labels = labels,
                             .strict = FALSE)
  } else {
    x
  }
  x
}

#' @importFrom utils type.convert
#' @noRd
postprocess_data_ <- function(x, form, lang) {
  x <- dummy_from_form_(x, form)
  x <- type.convert(x, as.is = TRUE)
  x <- val_labels_from_form_(x = x, form = form, lang = lang)
  x <- var_labels_from_form_(x = x, form = form, lang = lang)
  x
}

#' @noRd
kobo_question_types <- function() {
  c("start", "end", "today", "deviceid",
    "phonenumber", "username", "email", "audit",
    "integer", "decimal", "range", "text",
    "select_one", "select_multiple",
    "select_one_from_file", "select_multiple_from_file",
    "rank", "geopoint", "geotrace", "geoshape",
    "date", "time", "dateTime", "image", "audio",
    "background-audio", "video", "file", "barcode",
    "calculate", "acknowledge", "hidden")
}

#' @noRd
kobo_display_fields <- function() {
  c("label", "hint", "guidance_hint",
    "constraint_message", "required_message",
    "image", "audio", "video")
}

#' @importFrom dplyr bind_rows
#' @importFrom rlang set_names
#' @noRd
empty_tibble_ <- function(cnames) {
  x <- set_names(lapply(cnames, \(x) NA), cnames)
  bind_rows(x)
}


#' @importFrom dplyr distinct
#' @noRd
kobo_repeat_form_to_list <- function(x, .dm) {
  types <- c("begin_repeat", "end_repeat",
             kobo_question_types())
  x <- filter(x, .data$type %in% types)
  x <- distinct(x, .data$name, .data$type)
  rpt_name <- c("main", x$name[x$type %in% "begin_repeat"])
  idx <- which(x$type %in% "begin_repeat")
  idx <- c(0, idx, length(x$type))
  seq_idx <- 1:(length(idx) - 1L)
  setNames(lapply(seq_idx,
                  function(i) x$name[seq(idx[i] + 1, idx[i+1] - 1, by = 1)]),
           rpt_name)
}

#' @importFrom purrr map map_chr accumulate
#' @importFrom dplyr mutate right_join arrange if_else
#' @importFrom tibble rowid_to_column
#' @importFrom rlang set_names
#' @importFrom tidyr fill
#' @noRd
kobo_form_name_to_list_ <- function(x) {
  x <- distinct(x, .data$name, .data$type, .data$kuid)

  x <- x |>
    rowid_to_column() |>
    filter(grepl( "repeat", .data$type)) |>
    mutate(scope = accumulate(.data$name, ~ if(!is.na(.y)) c(.x, .y) else head(.x, -1))) |>
    select(.data$rowid, .data$scope) |>
    right_join(rowid_to_column(x), by = "rowid") |>
    arrange(.data$rowid) |>
    select(-.data$rowid) |>
    fill(.data$scope) |>
    mutate(scope = map_chr(.data$scope, paste, collapse = "/"),
           scope = if_else(.data$scope == "", "main", .data$scope)) |>
    filter(!grepl("repeat", .data$type))

  x |>
    split(x$scope) |>
    map(~ .x$name) |>
    set_names(basename)
}

#' @noRd
kobo_add_validation_status_ <- function(x) {
  if (any("_validation_status" %in% names(x)))
    x[["_validation_status_label"]] <- map_if_chr2(x[["_validation_status"]], "label")
  x
}
