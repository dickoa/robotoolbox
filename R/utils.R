#' @noRd
assert_url <- function(x) {
  url_pattern <- "^(https?:\\/\\/)?([\\da-z.-]+)\\.([a-z.]{2,6})(\\/[\\w .-]*)*\\/?$"
  all(grepl(pattern = url_pattern, x))
}

#' @noRd
assert_uid <- function(x) {
  uid_pattern <- "^a[A-Za-z0-9]{21}$"
  grepl(pattern = uid_pattern, x)
}

#' @noRd
assert_version_uid <- function(x) {
  vuid_pattern <- "^v[A-Za-z0-9]{21}$"
  grepl(pattern = vuid_pattern, x)
}

#' @noRd
assert_token <- function(x) {
  token_pattern <- "^[0-9a-f]{40}$"
  grepl(pattern = token_pattern, x)
}

#' @noRd
clean_urls <- function(x) {
  gsub("/$", "", x)
}

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
#' @importFrom RcppSimdJson fparse
error_msg <- function(x) {
  check_uid_info <- "Check your uid and/or credentials and try again"
  check_token_info <- "Check again your token in `kobo_setup` is correct or use `kobo_token`"
  check_cred_info <- "Check again your username or password"
  check_default <- "Check that you have the right server and if it's working"
  msg <- fparse(rawToChar(x))$detail
  info <- case_when(grepl("not found",
                          msg, ignore.case = TRUE) ~ c("uid (project) not found in this account/server",
                                                       i = check_uid_info),
                    grepl("invalid token",
                          msg, ignore.case = TRUE) ~ c("Invalid API token",
                                                       i = check_token_info),
                    grepl("invalid username/password",
                          msg, ignore.case = TRUE) ~ c("Invalid username or password",
                                                       i = check_cred_info),
                    .default = setNames(check_default, "i"))
  info
}

#' @importFrom rlang abort
#' @noRd
xget <- function(path, args = list(), n_retry = 3L, ...) {
  headers <- list(Authorization = paste("Token",
                                        Sys.getenv("KOBOTOOLBOX_TOKEN")),
                  `User-Agent` = user_agent_())
  cli <- HttpClient$new(Sys.getenv("KOBOTOOLBOX_URL"),
                        headers = headers, opts = list(...))
  res <- cli$retry("get",
                   path = path,
                   query = args,
                   times = n_retry,
                   retry_only_on = c(500, 503),
                   terminate_on = 404)
  if (res$status_code >= 300)
    abort(error_msg(res$content),
          call = NULL)
  res$raise_for_ct_json()
  res$parse("UTF-8")
}

#' @importFrom rlang abort
#' @noRd
xget_mime <- function(path, args = list(),
                      n_retry = 3L, mimetype = "application/json", ...) { # nocov start
  headers <- list(Authorization = paste("Token",
                                        Sys.getenv("KOBOTOOLBOX_TOKEN")),
                  `User-Agent` = user_agent_())
  cli <- HttpClient$new(Sys.getenv("KOBOTOOLBOX_URL"),
                        headers = headers, opts = list(...))
  res <- cli$retry("get",
                   path = path,
                   query = args,
                   times = n_retry,
                   retry_only_on = c(500, 503),
                   terminate_on = 404)
  if (res$status_code >= 300)
    abort(error_msg(res$content),
          call = NULL)
  res$raise_for_ct(mimetype)
  res$parse("UTF-8")
}  # nocov end

#' @importFrom purrr list_rbind
#' @importFrom tibble as_tibble
#' @importFrom dplyr transmute
#' @importFrom crul Paginator
#' @importFrom rlang abort
#' @noRd
get_asset_list_paginate <- function(limit = 100L,
                                    chunk = min(limit, 100L), n_retry = 3L) {
  headers <- list(Authorization = paste("Token",
                                        Sys.getenv("KOBOTOOLBOX_TOKEN")),
                  `User-Agent` = user_agent_())
  cli <- HttpClient$new(Sys.getenv("KOBOTOOLBOX_URL"),
                        headers = headers)

  cc <- Paginator$new(cli,
                      by = "limit_offset",
                      limit_param = "limit",
                      offset_param = "offset",
                      limit = limit,
                      chunk = chunk)

  cc$get(path = "/api/v2/assets",
         args = list(metadata = "on"),
         times = n_retry,
         retry_only_on = c(500, 503),
         terminate_on = 404)

  cond <- cc$status_code() >= 300L
  if (any(cond)) {
    msg <- cc$content()[cond]
    abort(error_msg(msg[[1]]),
          call = NULL)
  }

  resp <- cc$parse("UTF-8")
  res <- fparse(resp,
                max_simplify_lvl = "data_frame",
                query = "/results")

  if (length(resp) >= 2)
    res <- list_rbind(res)

  transmute(as_tibble(res),
            .data$uid, .data$name, .data$asset_type,
            owner_username = .data$owner__username,
            date_created = parse_kobo_datetime(.data$date_created),
            date_modified = parse_kobo_datetime(.data$date_modified),
            deployed = .data$deployment__active,
            submissions = .data$deployment__submission_count)
}

#' @importFrom tibble as_tibble
#' @noRd
dt2tibble <- function(x) {
  x <- as_tibble(x)
  attr(x, ".internal.selfref") <- NULL
  x
}

#' @importFrom RcppSimdJson fparse
#' @noRd
get_subs <- function(uid, args = list(), ...) {
  path <- paste0("api/v2/assets/", uid, "/data.json")
  res <- xget(path = path, args = args, ...)
  res <- fparse(res, max_simplify_lvl = "data_frame",
                query = "/results")
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
    df <- tibble(start = sprintf("%d", start),
                 limit = sprintf("%d", limit),
                 uid = uid)
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
#' @importFrom rlang abort
#' @noRd
get_subs_async <- function(uid, size, chunk_size = NULL, n_retry = 3L, ...) {
  headers <- list(Authorization = paste("Token",
                                        Sys.getenv("KOBOTOOLBOX_TOKEN")))
  urls <- build_subs_urls(uid, size, chunk_size)
  reqs <- lapply(urls, function(url) {
    req <- HttpRequest$new(url,
                           headers = headers,
                           opts = list(...))
    req$retry("get",
              times = n_retry,
              retry_only_on = c(500, 503),
              terminate_on = 404)
  })
  sleep <- case_when(
    size > 30000 ~ 1.5,
    size > 15000 ~ 1,
    size > 5000 ~ 0.5,
    size > 1000 ~ 0.25,
    .default = 0.1
  )
  res <- AsyncQueue$new(.list = reqs,
                        bucket_size = Inf,
                        sleep = sleep)
  res$request()
  cond <- res$status_code() >= 300L
  if (any(cond)) {
    msg <- res$content()[cond]
    abort(error_msg(msg[[1]]),
          call = NULL)
  }
  res <- res$parse(encoding = "UTF-8")
  res <- fparse(res,
                max_simplify_lvl = "data_frame",
                query = "/results")
  res <- rbindlist(res, fill = TRUE)
  res <- dt2tibble(res)
  res
}

#' @importFrom RcppSimdJson fparse
#' @importFrom purrr list_rbind
#' @importFrom dplyr select filter
#' @noRd
get_audit_url_ <- function(uid) {
  path <- paste0("api/v2/assets/", uid, "/data.json")
  res <- xget(path = path,
              args = list(fields = '["_attachments", "formhub/uuid", "_uuid"]'))
  res <- fparse(res, max_simplify_lvl = "data_frame")
  res <- res$result
  list_rbind(res[["_attachments"]]) |>
   filter(grepl("audit.csv$", .data$filename)) |>
   select(`_id` = "instance", "download_url")
}

#' @importFrom RcppSimdJson fparse
#' @noRd
get_attachment_url_ <- function(uid) {
  path <- paste0("api/v2/assets/", uid, "/data.json")
  res <- xget(path = path,
              args = list(fields = '["_attachments", "formhub/uuid", "_uuid"]'))
  res <- fparse(res, max_simplify_lvl = "data_frame")
  res <- res$result
  res[["_attachments"]]
}


#' @noRd
map_chr2 <- function(x, key) {
  vapply(x, function(l) as.character(l[[key]]), character(1))
}

#' @noRd
map_int2 <- function(x, key) { # nocov start
  vapply(x, function(l) as.integer(l[[key]]), integer(1))
} # nocov end

#' @noRd
parse_kobo_datetime <- function(x) {
  x <- as.POSIXct(x,
                  format = "%Y-%m-%dT%H:%M:%OS",
                  tz = "GMT")
  x <- round(x, "secs")
  as.character(x)
}

#' @noRd
parse_kobo_datetime_simple <- function(x) {
  as.character(round(as.POSIXct(x), "secs"))
}

#' @noRd
is_list_cols <- function(x) {
  which(vapply(x, is.list, logical(1)))
}

#' @noRd
is_list_cols_names <- function(x) {
  names(is_list_cols(x))
}

#' @noRd
is_null_recursive <- function(x) {
  is.null(x) | all(vapply(x, is.null, logical(1)))
}

#' @noRd
is_null_recursive <- function(x) {
  is.null(x) | any(vapply(x, is.null, logical(1)))
}

#' @noRd
drop_nulls <- function(x) {
  x <- lapply(x, function(node) {
    if (is.list(node))
      drop_nulls(node)
    else
      node
  })
  x[vapply(x, is.null, logical(1))] <- NULL
  x
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

#' @noRd
hide_token <- function(x) {
  a <- paste(rep("x", 35), collapse = "")
  b <- substr(x, 36, nchar(x))
  paste(c(a, b), collapse = "")
}

#' @importFrom data.table as.data.table alloc.col := chmatch set .SD
#' @importFrom stringi stri_detect_regex
#' @importFrom tibble as_tibble
#' @importFrom stats na.omit
#' @importFrom rlang names_inform_repair
#' @noRd
fast_dummy_cols <- function(x, form, cols, sep) {
  x <- as.data.table(x)
  for (col in cols) {
    y <- x[[col]]
    uv <- col2choices(x, form, col)
    new_names <- paste0(col, sep, uv)
    nm <- names(x)
    if (any(new_names %in% nm)) {
      old_cols <- intersect(new_names, nm)
      new_cols <- setdiff(make.unique(c(new_names, old_cols),
                                      sep = "_"),
                          new_names)
      names_inform_repair(old_cols, new_cols)
      x[, (new_cols) := .SD, .SDcols = old_cols]
      x[, (old_cols) := NULL]
    }
    alloc.col(x, ncol(x) + length(uv))
    x[, (new_names) := 0L]
    x[which(is.na(y)), (new_names) := NA_integer_]
    for (iter in seq_along(uv)) {
      a <- gsub("([[:punct:]])", "\\\\\\1", uv[iter])
      j <- paste0(col, sep, uv[iter])
      i <- which(stri_detect_regex(y,
                                   paste0("\\b", a,  "(\\s|$)")))
      set(x, i = i, j = j, value = 1L)
    }
  }
  as_tibble(x)
}

#' @importFrom stringi stri_detect_regex
#' @noRd
dummy_from_form_ <- function(x, form, sep) {
  bool <- stri_detect_regex(form$type, "select_multiple")
  nm <- unique(form$name[bool])
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    x <- fast_dummy_cols(x, form, nm, sep)
  } else {
    x
  }
  x
}

#' In case group_names are changed during data collection
#'
#' @importFrom dplyr coalesce
#' @noRd
dedup_vars_ <- function(x, all_versions = TRUE) {
  nm <- names(x)
  base_nm <- basename(nm)
  nm_to_test <- paste0("/", base_nm)
  cond <- duplicated(nm_to_test, fromLast = FALSE)
  if (any(cond)) {
    if (all_versions) {
    rs <- c(nm[cond],
            nm[duplicated(nm_to_test, fromLast = TRUE)])
    rs <- unique(rs)
    rs_key <- basename(rs)
    rs <- split(rs, rs_key)
    rs_key <- names(rs)
    for (v in rs_key) {
      old_vars <- rs[[v]]
      x[[v]] <- with(x, coalesce(!!!as.list(x[old_vars])))
      x[old_vars] <- NULL
    }
    } else {
      x <- x[!cond]
    }
  }
  x
}

#' @noRd
make_unique_names_ <- function(x) {
  make.unique(basename(x), sep = "_")
}

#' @importFrom purrr modify_if
#' @importFrom tibble tibble rowid_to_column
#' @importFrom data.table rbindlist
#' @noRd
extract_repeat_tbl <- function(x, form, all_versions) {
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
      res <- dedup_vars_(res, all_versions = all_versions)
      res <- set_names(res, make_unique_names_)
      rowid_to_column(res, "_index")
    }), nm)
  }
  res
}

#' @importFrom rlang squash
#' @noRd
kobo_extract_repeat_tbl <- function(x, form, all_versions) {
  if (is.null(x))
    return(NULL)
  x <- extract_repeat_tbl(x, form, all_versions)
  x <- lapply(x, function(df) {
    df$`_parent_table_name` <- "main"
    df
  })
  res <- lapply(seq_along(x), function(i)
    if (is.data.frame(x[[i]])) {
      c(setNames(list(x[[i]]), names(x[i])),
        lapply(extract_repeat_tbl(x[[i]], form, all_versions), function(df) {
               df$`_parent_table_name` <- names(x[i])
               df}))
      } else {
        list()
      })
  res <- suppressWarnings(squash(res))
  res <- set_names(res, make.unique, sep = "_")
  res
}

#' @importFrom dplyr mutate across filter group_by distinct bind_rows ungroup slice
#' @importFrom tidyselect all_of
#' @importFrom stats setNames
#' @importFrom labelled set_value_labels
#' @importFrom stringi stri_detect_regex
#' @importFrom rlang .data
#' @noRd
val_labels_from_form_ <- function(x, form, lang) {
  form <- filter(form,
                 .data$lang %in% !!lang,
                 stri_detect_regex(.data$type, "select_one"))
  nm <- unique(form$name)
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    form <- form |>
      filter(.data$name %in% nm) |>
      distinct(.data$name, .data$choices) |>
      group_by(.data$name) |>
      mutate(choices = list(distinct(select(bind_rows(choices),
                                            -"value_version")))) |>
      ungroup()
    choices <- form$choices
    names(choices) <- form$name
    choices <- lapply(nm, function(n) {
      ch <- choices[[n]]
      ch <- filter(ch,
                   .data$value_lang %in% !!lang)
      ch$value_label <- make.unique(ch$value_label, sep = "_")
      ch <- setNames(ch$value_name, ch$value_label)
      ch[!duplicated(ch)]
    })
    names(choices) <- nm
    labels <- choices[nm]
    x <- set_value_labels(mutate(x, across(all_of(nm), as.character)),
                          .labels = labels,
                          .strict = FALSE)
  } else {
    x
  }
  x
}

#' @importFrom dplyr mutate across filter group_by distinct bind_rows ungroup slice
#' @importFrom tidyselect all_of
#' @importFrom stats setNames
#' @importFrom labelled set_value_labels var_label<-
#' @importFrom rlang .data
#' @importFrom stringi stri_replace_all_regex
#'
#' @noRd
val_labels_sm_from_form_ <- function(x, form, lang) {
  form <- filter(form,
                 .data$lang %in% !!lang,
                 stri_detect_regex(.data$type, "select_multiple"))
  nm <- unique(form$name)
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    form <- form |>
      filter(.data$name %in% nm) |>
      distinct(.data$name, .data$choices) |>
      group_by(.data$name) |>
      mutate(choices = list(distinct(select(bind_rows(choices),
                                            -"value_version")))) |>
      ungroup()
    choices <- form$choices
    names(choices) <- form$name
    choices <- lapply(nm, function(n) {
      ch <- choices[[n]]
      ch <- filter(ch,
                   .data$value_lang %in% !!lang)
      ch$value_label <- make.unique(ch$value_label, sep = "_")
      ch
    })
    names(choices) <- nm
    for (n in nm) {
      d <- x[[n]]
      ch <- choices[[n]]
      vl <- var_label(d)
      x[[n]] <- stri_replace_all_regex(d,
                                       paste0("\\b", ch$value_name, "\\b"),
                                       ch$value_label,
                                       vectorize_all = FALSE)
      var_label(x[[n]]) <- vl
    }
  } else {
    x
  }
  x
}

#' @noRd
replace_na_list_ <- function(x) {
  b <- is.na(x)
  v <- names(x)[b]
  for (i in seq_along(v))
    x[b][[i]] <- v[i]
  x
}

#' @noRd
replace_na_list_efficient_ <- function(x) {
  is_na <- is.na(x)
  x[is_na] <- names(x)[is_na]
  x
}

#' @importFrom tidyr unnest
#' @importFrom dplyr transmute distinct
#' @noRd
select_multiple_var_label <- function(x, form, lang) {
  labels <- list()
  bool <- stri_detect_regex(form$type, "select_multiple")
  if (any(bool)) {
    choices <- filter(form,
                      .data$lang %in% !!lang,
                      stri_detect_regex(.data$type, "select_multiple")) |>
      unnest("choices") |>
      filter(.data$value_lang %in% !!lang) |>
      select(-any_of("value_version")) |>
      distinct() |>
      transmute(value_name = paste0(.data$name, "_", .data$value_name),
                value_label = paste0(.data$label, "::", .data$value_label)) |>
      distinct()
    labels <- setNames(as.list(choices$value_label),
                       make.unique(choices$value_name, sep = "_"))
  }
  labels
}

#' @importFrom labelled set_variable_labels
#' @importFrom stats setNames
#' @noRd
var_labels_from_form_ <- function(x, form, lang) {
  cond <- form$lang %in% lang & form$name %in% names(x)
  form <- form[cond, ]
  nm <- unique(form$name)
  labels_select_multiple <- select_multiple_var_label(x = x,
                                                      form = form, lang = lang)
  nm_select_multiple <- names(labels_select_multiple)
  nm_missing <- setdiff(names(x), nm)
  nm_missing <- setdiff(nm_missing, nm_select_multiple)
  nm <- intersect(names(x), nm)
  if (length(nm) > 0) {
    labels <- setNames(as.list(form$label), form$name)
    labels_missing <- setNames(as.list(nm_missing), nm_missing)
    labels <- c(labels, labels_missing, labels_select_multiple)
    labels <- replace_na_list_efficient_(labels)
    x <- set_variable_labels(x,
                             .labels = labels,
                             .strict = FALSE)
  } else {
    labels <- setNames(as.list(names(x)), names(x))
    x <- set_variable_labels(x,
                             .labels = labels,
                             .strict = FALSE)
  }
  x
}

#' @noRd
#' @importFrom dplyr rename_with
#' @importFrom tidyr separate_wider_delim
#' @importFrom tidyselect all_of
#' @importFrom labelled set_variable_labels var_label
extract_geopoint_ <- function(x, form) {
  cond <- form$type %in% "geopoint"
  nm <- unique(form$name[cond])
  nm <- intersect(names(x), nm)
  if (any(cond) && length(nm) > 0) {
    wkt_geopoint <- function(x) {
      pattern <- "^(\\s*-?\\d+(?:\\.\\d+)?)\\s+(-?\\d+(?:\\.\\d+)?)\\s+(-?\\d+(?:\\.\\d+)?)(\\s+-?\\d+(?:\\.\\d+)?)$"
      replacement <- "POINT (\\2 \\1 \\3)"
      gsub(pattern, replacement, x)
    }

    separate_geopoint <- function(x, col) {
      lbl <- var_label(select(x, all_of(col)))
      nm <- c("latitude", "longitude", "altitude", "precision", "wkt")
      lbl <- lapply(seq_along(lbl), function(i)  setNames(paste0(lbl[[i]], "::", nm),
                                                          paste0(names(lbl)[i], "_", nm)))
      lbl <- lapply(lbl, as.list)
      lbl <- unlist(lbl, recursive = FALSE)

      x <- x |>
        mutate(across(.cols = all_of(col),
                      .fns = ~ wkt_geopoint(.x),
                      .names = "{.col}_wkt")) |>
        separate_wider_delim({{col}},
                             names = c("latitude", "longitude",
                                       "altitude", "precision"),
                             delim = " ",
                             names_sep = "_",
                             cols_remove = FALSE) |>
        rename_with(~ gsub("^(.+)_(\\1)$", "\\1", .x), starts_with(col)) |>
        set_variable_labels(.labels = lbl, .strict = FALSE)
      x
    }
    x <- separate_geopoint(x, nm)
  }
  x
}

#' @noRd
#' @importFrom dplyr rename_with
#' @importFrom tidyr separate_wider_delim
#' @importFrom tidyselect all_of
#' @importFrom labelled set_variable_labels var_label
extract_geotrace_ <- function(x, form) {
  cond <- form$type %in% "geotrace"
  nm <- unique(form$name[cond])
  nm <- intersect(names(x), nm)
  if (any(cond) && length(nm) > 0) {
    wkt_geotrace <- function(x) {
      x <- strsplit(x, ";")
      pattern <- "^(\\s*-?\\d+(?:\\.\\d+)?)\\s+(-?\\d+(?:\\.\\d+)?)\\s+(-?\\d+(?:\\.\\d+)?)(\\s+-?\\d+(?:\\.\\d+)?)$"
      replacement <- "\\2 \\1 \\3"
      x <- lapply(x, \(s) gsub(pattern, replacement, s))
      vapply(x, \(s) paste0("LINESTRING (", paste0(s, collapse = ", "), ")"),
             character(1))
    }

    separate_geotrace <- function(x, col) {
      lbl <- var_label(select(x, all_of(col)))
      lbl <- set_names(lbl, paste0(col, "_wkt"))
      lbl <- lapply(lbl, function(x)  paste0(x, "::", "wkt"))

      x <- x |>
        mutate(across(.cols = all_of(col),
                      .fns = ~ wkt_geotrace(.x),
                      .names = "{.col}_wkt")) |>
        set_variable_labels(.labels = lbl, .strict = FALSE)
      x
    }
    x <- separate_geotrace(x, nm)
  }
  x
}

#' @noRd
#' @importFrom dplyr rename_with
#' @importFrom tidyr separate_wider_delim
#' @importFrom tidyselect all_of
#' @importFrom labelled set_variable_labels var_label
extract_geoshape_ <- function(x, form) {
  cond <- form$type %in% "geoshape"
  nm <- unique(form$name[cond])
  nm <- intersect(names(x), nm)
  if (any(cond) && length(nm) > 0) {
    wkt_geoshape <- function(x) {
      x <- strsplit(x, ";")
      pattern <- "^(\\s*-?\\d+(?:\\.\\d+)?)\\s+(-?\\d+(?:\\.\\d+)?)\\s+(-?\\d+(?:\\.\\d+)?)(\\s+-?\\d+(?:\\.\\d+)?)$"
      replacement <- "\\2 \\1 \\3"
      x <- lapply(x, \(s) gsub(pattern, replacement, s))
      x <- vapply(x, \(s) paste0("POLYGON ((", paste0(s, collapse = ", "), "))"),
                  character(1))
      x
    }

    separate_geoshape <- function(x, col) {
      lbl <- var_label(select(x, all_of(col)))
      lbl <- set_names(lbl, paste0(col, "_wkt"))
      lbl <- lapply(lbl, function(x)  paste0(x, "::", "wkt"))

      x <- x |>
        mutate(across(.cols = all_of(col),
                      .fns = ~ wkt_geoshape(.x),
                      .names = "{.col}_wkt")) |>
        set_variable_labels(.labels = lbl, .strict = FALSE)
      x
    }
    x <- separate_geoshape(x, nm)
  }
  x
}

#' @noRd
is_zero_length_or_null <- function(x) {
  is.null(x) | length(x) == 0
}

#' @noRd
is_uid_available <- function(x) {
  length(x) > 0 && !is.null(x$uid)
}

#' @noRd
is_label_available <- function(x) {
  length(x) > 0 && !is.null(x$label)
}

#' @noRd
#' @importFrom dplyr select
#' @importFrom labelled labelled
#' @importFrom tidyselect any_of
remove_list_cols <- function(x) {
  val_col <- x[["_validation_status"]]
  cond <- all(vapply(val_col, is_zero_length_or_null, logical(1)))
  if (!cond) {
  val_stat <- vapply(val_col,
                     \(x) if (!is_uid_available(x))
                       NA_character_ else x$uid, character(1))
  val_lbl <- vapply(val_col,
                    \(x) if (!is_label_available(x))
                      NA_character_ else x$label, character(1))
  b <- is.na(val_stat)
  val_dict <- setNames(val_stat[!b], val_lbl[!b])
  val_dict <- val_dict[unique(names(val_dict))]
  x <- x |>
    mutate(`_validation_status` = labelled(val_stat, val_dict))
  x <- select(x, -is_list_cols(x), any_of("_attachments"))
  } else {
  x <- x |>
    mutate(`_validation_status` = NA_character_)
  x <- select(x, -is_list_cols(x), any_of("_attachments"))
  }
  x
}

#' @noRd
add_missing_cols_ <- function(x, cn) {
  x[setdiff(cn, names(x))] <- NA
  x
}

#' @importFrom dplyr select
#' @importFrom tidyselect any_of everything
#' @noRd
reorder_cols_ <- function(x, cn) {
  cn <- intersect(cn, names(x))
  select(x, any_of(cn), everything())
}

#' @importFrom utils type.convert
postprocess_data_ <- function(x, form, lang, select_multiple_label = FALSE, cn, select_multiple_sep = "_") {
  x <- dummy_from_form_(x, form, sep = select_multiple_sep)
  x <- extract_geopoint_(x, form)
  x <- extract_geotrace_(x, form)
  x <- extract_geoshape_(x, form)
  x <- type_convert_(x, form)
  x <- remove_list_cols(x)
  x <- add_missing_cols_(x, cn)
  x <- val_labels_from_form_(x = x, form = form, lang = lang)
  x <- var_labels_from_form_(x = x, form = form, lang = lang)
  if (isTRUE(select_multiple_label))
    x <- val_labels_sm_from_form_(x = x, form = form, lang = lang)
  x <- reorder_cols_(x, cn)
}

#' @noRd
kobo_question_types <- function() {
  c("start", "end", "today", "deviceid",
    "phonenumber", "username", "email", "audit",
    "integer", "decimal", "range", "text",
    "select_one", "select_multiple",
    "select_one_from_file", "select_multiple_from_file",
    "rank", "note", "geopoint", "geotrace", "geoshape",
    "date", "time", "dateTime", "datetime", "image", "audio",
    "background-audio", "video", "file", "barcode",
    "calculate", "acknowledge", "hidden")
}

#' @noRd
kobo_display_fields <- function() {
  c("label", "hint", "guidance_hint",
    "constraint_message", "required_message",
    "image", "media::image",
    "big-image", "media::big-image",
    "audio", "media::audio",
    "video", "media::video")
}

#' @importFrom dplyr bind_rows
#' @importFrom rlang set_names
#' @noRd
empty_tibble_ <- function(cnames) {
  tibble(!!!cnames,
         .rows = 0,
         .name_repair = ~ cnames)
}

#' @importFrom purrr map map_chr accumulate
#' @importFrom dplyr mutate right_join arrange if_else
#' @importFrom tibble rowid_to_column
#' @importFrom rlang set_names
#' @importFrom tidyr fill
#' @noRd
kobo_form_name_to_list_ <- function(x, sep) {
  x <- kobo_form_extra_(x, sep)
  x <- distinct(x, .data$name, .data$type, .data$cnames)

  x <- x |>
    rowid_to_column() |>
    filter(grepl("repeat", .data$type)) |>
    mutate(scope = accumulate(.data$name, ~ if(!is.na(.y))
      c(.x, .y) else head(.x, -1))) |>
    select("rowid", "scope") |>
    right_join(rowid_to_column(x), by = "rowid") |>
    arrange(.data$rowid) |>
    select(-"rowid") |>
    fill("scope") |>
    mutate(scope = map_chr(.data$scope, paste, collapse = "/"),
           scope = if_else(.data$scope == "", "main", .data$scope)) |>
    filter(!grepl("repeat", .data$type))

  x |>
    split(x$scope) |>
    map(~ .x$cnames) |>
    set_names(basename)
}

#' @noRd
#' @importFrom labelled var_label
lookup_varlabel_ <- function(x) {
  v <- var_label(x)
  setNames(names(v),
           make.unique(unlist(v), sep = "_"))
}

#' @importFrom dplyr rename
#' @noRd
set_names_from_varlabel_ <- function(x) {
  lookup <- lookup_varlabel_(x)
  rename(x, all_of(lookup))
}

#' @importFrom dm dm_rename
#' @noRd
set_names_from_varlabel_dm_ <- function(x) {
  tbl_nm <- names(x)
  for (nm in tbl_nm) {
    x <- dm_rename(x,
                   {{nm}},
                   lookup_varlabel_(x[[nm]]))
  }
  x
}

#' @importFrom dplyr mutate case_when
#' @importFrom stats na.omit
#' @noRd
kobo_form_extra_ <- function(form, sep) {
  media_type <- c("image", "video", "audio", "file")
  if ("choices" %in% names(form)) {
    res <- mutate(form, "value_name" =
                          case_when(type %in% "geopoint" ~ list(c("",
                                                                  "_latitude",
                                                                  "_longitude",
                                                                  "_altitude",
                                                                  "_precision",
                                                                  "_wkt")),
                                    type %in% "geotrace" ~ list(c("", "_wkt")),
                                    type %in% "geoshape" ~ list(c("", "_wkt")),
                                    stri_detect_regex(type, "select_multiple") ~ lapply(.data$choices,
                                                                         \(ch) c("", paste0(sep, unique(ch$value_name)))),
                                    .default = list(""))) |>
      unnest("value_name") |>
      mutate(cnames = paste0(.data$name, .data$value_name))
  } else {
    res <- mutate(form, "value_name" =
                          case_when(type %in% "geopoint" ~ list(c("",
                                                                  "_latitude",
                                                                  "_longitude",
                                                                  "_altitude",
                                                                  "_precision",
                                                                  "_wkt")),
                                    type %in% "geotrace" ~ list(c("", "_wkt")),
                                    type %in% "geoshape" ~ list(c("", "_wkt")),
                                    .default = list(""))) |>
      unnest("value_name") |>
      mutate(cnames = paste0(.data$name, .data$value_name))
  }
  res
}

#' @importFrom dplyr mutate case_when
#' @importFrom stats na.omit
#' @noRd
kobo_form_names_ <- function(x, sep) {
  x <- kobo_form_extra_(x, sep)
  x <- unique(x$cnames)
  na.omit(x)
}

#' @importFrom readr type_convert
#' @noRd
type_convert_ <- function(x, form) {
  cols <- kobo_type_cols_(x, form)
  type_convert(x, col_types = cols)
}

#' @importFrom dplyr case_when coalesce arrange
#' @noRd
kobo_type_cols_ <- function(x, form) {
  time_q <- "time"
  datetime_q <- c("start", "end", "datetime", "dateTime")
  date_q <- c("today", "date")
  char_q <- c("device_id", "phonenumber", "username", "email",
              "audit", "select_one", "select_multiple",
              "select_one_from_file", "select_multiple_from_file",
              "rank", "text")
  form$cols <- case_when(form$type %in% datetime_q ~ "T",
                         form$type %in% date_q ~ "D",
                         form$type %in% time_q ~ "c",
                         form$type %in% char_q ~ "c",
                         .default = "?")
  v <- form$cols[match(names(x), form$name)]
  paste(coalesce(v, "?"), collapse = "")
}

#' @noRd
kobo_form_version_ <- function(x, asset, all_versions) {
  uid <- asset$uid
  default_version <- asset$deployed_version_id
  form_versions <- kobo_asset_version_list(uid)
  form_versions <- filter(form_versions, .data$deployed)
  cond3 <- nrow(form_versions) > 0
  form_versions <- unique(form_versions$uid)
  cond1 <- sum(grepl("\\_version\\_", names(x))) == 1
  subs_versions <- unique(x[["__version__"]])
  cond2 <- length(subs_versions) > 1
  versions <- intersect(unique(form_versions),
                        unique(subs_versions))
  if (length(versions) == 0)
    versions <- default_version
  cond <- cond1 & cond2 & cond3 & all_versions
  if (cond) {
    form <- lapply(versions, \(v) kobo_form(uid, v))
    form <- dt2tibble(rbindlist(form, fill = TRUE))
  } else {
    v <- if (!cond2 & cond3) versions else NULL
    form <- kobo_form(uid, version = v)
  }
  form
}

#' @noRd
external_file_lookup_ <- function(x) {
  x <- scan(text = gsub("value\\=", "name\\=", x),
            what = character(),
            sep = ",",
            quiet = TRUE)
  x <- strsplit(x, "=")
  p1 <- vapply(x, \(val) val[2], "")
  p2 <- vapply(x, \(val) val[1], "")
  set_names(p1, p2)
}

#' @importFrom tools toTitleCase
#' @importFrom stringi stri_replace_all_regex
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr rename
#' @importFrom tidyselect any_of starts_with
#' @importFrom rlang .data
#' @noRd
parse_external_csv_ <- function(x, lookup, lang) {
  x |>
    select(-starts_with("label::")[-1]) |>
    rename_with(~ gsub("::.+", "", .x)) |>
    rename(any_of(lookup)) |>
    mutate(value_name = as.character(.data$name),
           value_label = as.character(.data$label),
           value_lang = as.character(lang),
           .keep = "none")
}

#' @importFrom purrr list_rbind
#' @noRd
external_files_choice_ <- function(urls, lookup = NULL, lang = "Labels", n_retry = 3L) {
  headers <- list(Authorization = paste("Token",
                                        Sys.getenv("KOBOTOOLBOX_TOKEN")))
  reqs <- lapply(urls, function(url) {
    req <- HttpRequest$new(url,
                           headers = headers)
    req$retry("get",
              times = n_retry,
              retry_only_on = c(500, 503),
              terminate_on = 404)
  })
  res <- AsyncQueue$new(.list = reqs,
                        bucket_size = Inf,
                        sleep = 0.01)
  res$request()
  cond <- res$status_code() >= 300L
  if (any(cond)) {
    msg <- res$content()[cond]
    abort(error_msg(msg[[1]]),
          call = NULL)
  }

  res <- res$content()
  res <- lapply(res,
                \(f) parse_external_csv_(fread(text = rawToChar(f)),
                                         lookup, lang))
  res
}
