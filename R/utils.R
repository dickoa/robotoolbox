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

#' Resolve an asset argument to a kobo_asset object
#' @importFrom rlang abort
#' @noRd
as_kobo_asset_ <- function(x) {
  if (inherits(x, "kobo_asset")) {
    return(x)
  }
  if (is.character(x) && length(x) == 1 && assert_uid(x)) {
    return(kobo_asset(x))
  }
  abort(
    "'asset' must be a 'kobo_asset' object or a valid asset uid",
    call = NULL
  )
}

#' Maximum number of records per request for the /data endpoint
#' @noRd
api_max_limit_ <- function() {
  val <- Sys.getenv("KOBOTOOLBOX_PAGE_SIZE", "1000")
  as.integer(val)
}

#' @importFrom RcppSimdJson is_valid_json
#' @noRd
validate_query_ <- function(query) {
  if (is.null(query)) {
    return(invisible(NULL))
  }
  if (!is.character(query) || length(query) != 1 || is.na(query)) {
    abort("`query` must be a single character string", call = NULL)
  }
  is_object <- grepl("^[[:space:]]*[{]", query) && is_valid_json(query)
  if (!is_object) {
    abort(
      c(
        "`query` must be a valid JSON object string",
        i = 'Example: \'{"field_name": "value"}\''
      ),
      call = NULL
    )
  }
  invisible(NULL)
}

#' @importFrom rlang warn
#' @noRd
validate_fields_ <- function(fields) {
  if (is.null(fields)) {
    return(invisible(NULL))
  }
  if (!is.character(fields) || length(fields) == 0) {
    abort("`fields` must be a non-empty character vector", call = NULL)
  }
  has_slash <- grepl("/", fields, fixed = TRUE)
  if (any(has_slash)) {
    abort(
      c(
        paste0(
          "Nested field selection is not supported: ",
          paste0("`", fields[has_slash], "`", collapse = ", ")
        ),
        i = "Use the repeat group name to fetch the entire group.",
        i = 'Example: fields = c("name", "my_repeat_group")'
      ),
      call = NULL
    )
  }
  invisible(NULL)
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
  os_version <- paste(Sys.info()[["release"]], Sys.info()[["version"]])
  r_version <- paste0(
    R.version$major,
    ".",
    R.version$minor,
    ifelse(R.version$status == "", "", paste0("-", R.version$status))
  )
  header <- paste0(
    "robotoolbox/",
    robotoolbox_version,
    " (",
    os,
    "/",
    os_version,
    "; ",
    "R/",
    r_version,
    ")"
  )
  header
}

#' @noRd
#' @importFrom RcppSimdJson fparse
error_msg <- function(x) {
  check_uid_info <- "Check your uid and/or credentials and try again"
  check_token_info <- "Check again your token in `kobo_setup` is correct or use `kobo_token`"
  check_cred_info <- "Check again your username or password"
  check_default <- "Check that you have the right server and if it's working"
  msg <- tryCatch(
    fparse(rawToChar(x))$detail,
    error = function(e) NULL
  )
  if (is.null(msg)) {
    return(setNames(check_uid_info, "i"))
  }
  if (grepl("not found", msg, ignore.case = TRUE)) {
    c("uid (project) not found in this account/server", i = check_uid_info)
  } else if (grepl("invalid token", msg, ignore.case = TRUE)) {
    c("Invalid API token", i = check_token_info)
  } else if (grepl("invalid username/password", msg, ignore.case = TRUE)) {
    c("Invalid username or password", i = check_cred_info)
  } else {
    setNames(check_default, "i")
  }
}

#' @importFrom rlang abort
#' @noRd
xget <- function(path, args = list(), n_retry = 3L, ...) {
  headers <- list(
    Authorization = paste("Token", Sys.getenv("KOBOTOOLBOX_TOKEN")),
    `User-Agent` = user_agent_()
  )
  cli <- HttpClient$new(
    Sys.getenv("KOBOTOOLBOX_URL"),
    headers = headers,
    opts = list(...)
  )
  res <- cli$retry(
    "get",
    path = path,
    query = args,
    times = n_retry,
    retry_only_on = c(500, 503),
    terminate_on = 404
  )
  if (res$status_code >= 300) {
    abort(
      error_msg(res$content),
      class = paste0("robotoolbox_http_", res$status_code),
      call = NULL
    )
  }
  res$raise_for_ct_json()
  res$parse("UTF-8")
}

#' @importFrom rlang abort
#' @noRd
xget_mime <- function(
  path,
  args = list(),
  n_retry = 3L,
  mimetype = "application/json",
  ...
) {
  # nocov start
  headers <- list(
    Authorization = paste("Token", Sys.getenv("KOBOTOOLBOX_TOKEN")),
    `User-Agent` = user_agent_()
  )
  cli <- HttpClient$new(
    Sys.getenv("KOBOTOOLBOX_URL"),
    headers = headers,
    opts = list(...)
  )
  res <- cli$retry(
    "get",
    path = path,
    query = args,
    times = n_retry,
    retry_only_on = c(500, 503),
    terminate_on = 404
  )
  if (res$status_code >= 300) {
    abort(
      error_msg(res$content),
      class = paste0("robotoolbox_http_", res$status_code),
      call = NULL
    )
  }
  res$raise_for_ct(mimetype)
  res$parse("UTF-8")
} # nocov end

#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr transmute
#' @importFrom cli cli_warn
#' @noRd
get_asset_list_ <- function(limit = 100L) {
  pages <- xget_all_(
    "api/v2/assets/",
    args = list(ordering = asset_ordering_),
    n_max = limit,
    simplify = "data_frame"
  )
  if (length(pages) == 0L) {
    return(tibble(
      uid = character(),
      name = character(),
      asset_type = character(),
      owner_username = character(),
      date_created = character(),
      date_modified = character(),
      deployed = logical(),
      submissions = integer(),
      created_by = character()
    ))
  }
  res <- distinct_assets_(pages)
  expected <- min(attr(pages, "count") %||% 0, limit)
  if (nrow(res) < expected) {
    cli_warn(c(
      "The server listed {nrow(res)} of {expected} assets.",
      i = "Pages overlapped because several assets sort identically."
    ))
  }
  if (!"created_by" %in% names(res)) {
    res$created_by <- NA_character_
  }
  transmute(
    res,
    .data$uid,
    .data$name,
    .data$asset_type,
    owner_username = .data$owner__username,
    date_created = parse_kobo_datetime(.data$date_created),
    date_modified = parse_kobo_datetime(.data$date_modified),
    deployed = .data$deployment__active,
    submissions = .data$deployment__submission_count,
    created_by = as.character(.data$created_by)
  )
}

#' @importFrom tibble tibble
#' @importFrom cli cli_inform
#' @importFrom rlang try_fetch
#' @noRd
get_asset_minimal_list_ <- function(limit = 100L) {
  args <- list(ordering = asset_ordering_)
  pages <- try_fetch(
    xget_all_(
      "api/v2/assets/minimal-list/",
      args = args,
      n_max = limit,
      simplify = "data_frame"
    ),
    robotoolbox_http_404 = function(e) {
      cli_inform(c(
        i = "The minimal asset listing needs KoboToolbox KPI 2.026.21 or later.",
        " " = "Using the full asset listing instead."
      ))
      xget_all_("api/v2/assets/", args = args, n_max = limit, simplify = "data_frame")
    }
  )
  if (length(pages) == 0L) {
    return(tibble(
      uid = character(),
      name = character(),
      deployment_status = character()
    ))
  }
  distinct_assets_(pages)[c("uid", "name", "deployment_status")]
}

#' Sort order for asset listings
#'
#' The default order (deployment status, then `-date_modified`) has ties, for
#' example after a server migration touches many assets at once, and tied rows
#' can move between pages. `uid` is not an allowed ordering field, so further
#' fields break the ties.
#' @noRd
asset_ordering_ <- "-date_modified,name,owner__username,asset_type,date_deployed"

#' @importFrom purrr list_rbind
#' @importFrom tibble as_tibble
#' @noRd
distinct_assets_ <- function(pages) {
  res <- as_tibble(list_rbind(pages))
  res[!duplicated(res$uid), ]
}

#' @importFrom tibble as_tibble
#' @noRd
dt2tibble <- function(x) {
  x <- as_tibble(x)
  attr(x, ".internal.selfref") <- NULL
  x
}

#' @noRd
format_fields_ <- function(fields) {
  if (is.null(fields)) {
    return(NULL)
  }
  if (!"__version__" %in% fields) {
    fields <- c(fields, "__version__")
  }
  paste0('["', paste(fields, collapse = '","'), '"]')
}

#' @noRd
get_filtered_count_ <- function(uid, query) {
  path <- paste0("api/v2/assets/", uid, "/data.json")
  args <- list(limit = 1, query = query)
  res <- xget(path = path, args = args)
  parsed <- fparse(res, max_simplify_lvl = "list")
  count <- parsed$count
  if (is.null(count)) 0L else as.integer(count)
}

#' @noRd
get_audit_url_ <- function(uid) {
  empty <- tibble(`_id` = integer(), download_url = character())
  res <- get_subs_fields_(uid, '["_attachments", "formhub/uuid", "_uuid"]')
  if (is.null(res)) {
    return(empty)
  }
  res <- unnest(res[c("_id", "_attachments")], "_attachments")
  if (!"media_file_basename" %in% names(res)) {
    return(empty)
  }
  res |>
    filter(.data$media_file_basename %in% "audit.csv") |>
    select("_id", "download_url")
}

#' @noRd
empty_audit_ <- function() {
  int64 <- structure(numeric(), class = "integer64")
  time <- as.POSIXct(numeric(), origin = "1970-01-01")
  tibble(`_id` = integer(), event = character(), node = character(),
         name = character(), start_int = int64, end_int = int64,
         start = time, end = time, latitude = numeric(),
         longitude = numeric(), accuracy = numeric(),
         `old-value` = character(), `new-value` = character(),
         user = character(), `change-reason` = character())
}

#' @importFrom RcppSimdJson fparse
#' @noRd
get_attachment_url_ <- function(uid) {
  res <- get_subs_fields_(uid, '["_attachments", "formhub/uuid", "_uuid"]')
  if (is.null(res)) {
    return(tibble(`_id` = integer()))
  }
  unnest(res[c("_id", "_attachments")], "_attachments")
}


#' @noRd
map_chr2 <- function(x, key) {
  vapply(x, function(l) as.character(l[[key]]), character(1))
}

#' @noRd
map_int2 <- function(x, key) {
  # nocov start
  vapply(x, function(l) as.integer(l[[key]]), integer(1))
} # nocov end

#' @noRd
parse_kobo_datetime <- function(x) {
  x <- as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%OS", tz = "GMT")
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
drop_nulls <- function(x) {
  x <- lapply(x, function(node) {
    if (is.list(node)) {
      drop_nulls(node)
    } else {
      node
    }
  })
  x[vapply(x, is.null, logical(1))] <- NULL
  x
}

#' @noRd
null2char <- function(x) {
  if (is.null(x)) {
    return("")
  }
  x
}

#' @importFrom stringi stri_sort
#' @importFrom stats na.omit
#' @noRd
col2choices <- function(x, form, col) {
  idx <- which(form$name %in% col & grepl("select_multiple", form$type, fixed = TRUE))
  if (length(idx) > 0) {
    uv <- unique(unlist(lapply(form$choices[idx], `[[`, "value_name")))
  } else {
    uv <- unique(unlist(strsplit(x[[col]], "\\s+")))
    uv <- trimws(na.omit(uv))
  }
  stri_sort(uv, na_last = TRUE, locale = "en_US", numeric = TRUE)
}

#' @noRd
hide_token <- function(x) {
  a <- paste(rep("x", 35), collapse = "")
  b <- substr(x, 36, nchar(x))
  paste(c(a, b), collapse = "")
}

#' @importFrom data.table chmatch
#' @importFrom rlang names_inform_repair
#' @noRd
fast_dummy_cols <- function(x, form, cols, sep) {
  for (col in cols) {
    y <- x[[col]]
    uv <- col2choices(x, form, col)
    new_names <- paste0(col, sep, uv)
    nm <- names(x)
    if (any(new_names %in% nm)) {
      old_cols <- intersect(new_names, nm)
      new_cols <- setdiff(
        make.unique(c(new_names, old_cols), sep = "_"),
        new_names
      )
      names_inform_repair(old_cols, new_cols)
      for (i in seq_along(old_cols)) {
        x[[new_cols[i]]] <- x[[old_cols[i]]]
      }
      x[old_cols] <- NULL
    }
    na_idx <- is.na(y)
    init <- ifelse(na_idx, NA_integer_, 0L)
    for (nm in new_names) {
      x[[nm]] <- init
    }
    non_na_idx <- which(!na_idx)
    if (length(non_na_idx) > 0) {
      splits <- strsplit(y[non_na_idx], " ", fixed = TRUE)
      rids <- rep.int(non_na_idx, lengths(splits))
      vals <- unlist(splits, use.names = FALSE)
      midx <- chmatch(vals, uv)
      valid <- !is.na(midx)
      if (any(valid)) {
        rids_by_col <- split(rids[valid], midx[valid])
        for (k in names(rids_by_col)) {
          x[[new_names[as.integer(k)]]][rids_by_col[[k]]] <- 1L
        }
      }
    }
  }
  x
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
dedup_vars_ <- function(x) {
  nm <- names(x)
  nm_to_test <- paste0("/", basename(nm))
  cond <- duplicated(nm_to_test, fromLast = FALSE)
  if (any(cond)) {
    rs <- c(nm[cond], nm[duplicated(nm_to_test, fromLast = TRUE)])
    rs <- unique(rs)
    rs <- split(rs, basename(rs))
    for (v in names(rs)) {
      old_vars <- rs[[v]]
      merged <- coalesce(!!!unname(as.list(x[old_vars])))
      x[old_vars] <- NULL
      x[[v]] <- merged
    }
  }
  x
}

#' @noRd
make_unique_names_ <- function(x) {
  make.unique(basename(x), sep = "_")
}

#' Repeat group enclosing each row of the form
#'
#' The scope is computed within each form version. A `begin_repeat` row gets
#' the scope that encloses the repeat group, its parent.
#' @noRd
repeat_scope_ <- function(form) {
  ver <- form$version %||% rep(NA_character_, nrow(form))
  ver[is.na(ver)] <- ""
  begin <- repeat_begin_(form)
  scope <- rep("main", nrow(form))
  for (v in unique(ver)) {
    idx <- which(ver == v)
    type <- form$type[idx]
    name <- form$name[idx]
    is_begin <- begin[idx]
    ev <- which(is_begin | type == "end_repeat")
    after <- character(length(ev))
    stack <- "main"
    for (k in seq_along(ev)) {
      if (is_begin[ev[k]]) {
        stack <- c(stack, name[ev[k]])
      } else if (length(stack) > 1L) {
        stack <- stack[-length(stack)]
      }
      after[k] <- stack[length(stack)]
    }
    pos <- findInterval(seq_along(idx), ev)
    s <- c("main", after)[pos + 1L]
    b <- ev[is_begin[ev]]
    s[b] <- c("main", after)[pos[b]]
    scope[idx] <- s
  }
  scope
}

#' Rows of the form that open a repeat group
#'
#' Rows without a label, such as `end_repeat`, only exist in the first
#' language of each form version, so only the `begin_repeat` rows of that
#' language open a repeat group.
#' @noRd
repeat_begin_ <- function(form) {
  ver <- form$version %||% rep(NA_character_, nrow(form))
  ver[is.na(ver)] <- ""
  lang <- form$lang %||% rep("", nrow(form))
  first <- lang[match(ver, ver)]
  same_lang <- lang == first | (is.na(lang) & is.na(first))
  form$type %in% "begin_repeat" & !is.na(form$name) & same_lang %in% TRUE
}

#' Parent tables of each repeat group
#'
#' A repeat group can have a different parent in another form version. The
#' links follow the form order, so the deployed version gives the first
#' parent of each repeat group.
#' @noRd
repeat_links_ <- function(form) {
  b <- repeat_begin_(form)
  res <- data.frame(name = form$name[b], parent = repeat_scope_(form)[b])
  res[!duplicated(res), , drop = FALSE]
}

#' Rows of a repeat group from each of its parent tables
#'
#' `x` holds the list column of the repeat group in each parent table, named
#' by parent. `_parent_index` points to the first parent of `parents`, and
#' `_parent_index_<parent>` to each other one.
#' @importFrom tibble tibble rowid_to_column add_column
#' @importFrom rlang :=
#' @importFrom data.table rbindlist
#' @noRd
repeat_tbl_ <- function(x, parents = names(x)) {
  parts <- lapply(x, function(rows) {
    if (is.null(rows)) {
      rows <- list()
    }
    rows[!vapply(rows, is.data.frame, logical(1))] <- list(tibble())
    rbindlist(rows, idcol = "_parent_index", fill = TRUE)
  })
  from <- rep(names(x), vapply(parts, nrow, integer(1)))
  res <- rbindlist(parts, fill = TRUE)
  if (nrow(res) == 0L) {
    res <- tibble(`_parent_index` = integer())
  } else {
    res <- dt2tibble(res)
    res <- dedup_vars_(res)
    res <- set_names(res, make_unique_names_)
  }
  idx <- res$`_parent_index`
  for (p in rev(parents[-1])) {
    res <- add_column(res, !!paste0("_parent_index_", p) := replace(idx, from != p, NA),
                      .after = "_parent_index")
  }
  res$`_parent_index` <- replace(idx, from != parents[1], NA)
  res <- rowid_to_column(res, "_index")
  res$`_parent_table_name` <- from
  res
}

#' Extract repeat tables at every depth
#'
#' Every repeat group of the form gives a table, with zero rows when the data
#' has no instance of it. Tables are listed in form order. Each one records
#' the parent table of each row in `_parent_table_name`. A repeat group whose
#' parent differs between form versions gets its rows from every parent. With
#' `fields`, only the top-level repeat groups it names are kept, together with
#' the repeat groups nested in them.
#' @noRd
kobo_extract_repeat_tbl <- function(x, form, fields = NULL) {
  links <- repeat_links_(form)
  todo <- unique(links$name)
  done <- "main"
  res <- list()
  while (length(todo) > 0) {
    ready <- vapply(todo, function(nm) all(links$parent[links$name == nm] %in% done),
                    logical(1))
    if (!any(ready)) {
      abort(
        c(
          "Can't extract the repeat groups of this form.",
          i = paste0("Repeat groups ", paste0("`", todo, "`", collapse = ", "),
                     " are nested in each other in different form versions.")
        ),
        call = NULL
      )
    }
    nm <- todo[ready][1]
    parents <- links$parent[links$name == nm]
    from_main <- parents == "main" & (is.null(fields) | nm %in% fields)
    found <- parents[from_main | parents %in% names(res)]
    if (length(found) > 0) {
      src <- lapply(found, function(p) if (p == "main") x[[nm]] else res[[p]][[nm]])
      res[[nm]] <- repeat_tbl_(setNames(src, found), parents)
    }
    done <- c(done, nm)
    todo <- todo[todo != nm]
  }
  res
}

#' Set the primary and foreign keys of repeat tables
#' @importFrom dm as_dm dm_add_pk dm_add_fk
#' @noRd
repeat_dm_ <- function(tbls, links) {
  res <- as_dm(tbls)
  for (nm in names(tbls)) {
    res <- dm_add_pk(res, !!nm, "_index")
    parents <- links$parent[links$name == nm]
    cols <- c("_parent_index", paste0("_parent_index_", parents[-1]))
    for (k in seq_along(parents)) {
      if (parents[k] %in% names(tbls)) {
        res <- dm_add_fk(res, !!nm, !!cols[k], !!parents[k])
      }
    }
  }
  res
}

#' @importFrom data.table rbindlist
#' @importFrom stats setNames
#' @noRd
precompute_val_labels_ <- function(form, lang) {
  form <- form[
    form$lang %in% lang & grepl("select_one", form$type, fixed = TRUE),
  ]
  nm <- unique(form$name)
  if (length(nm) == 0) {
    return(list())
  }
  choices <- lapply(nm, function(n) {
    ch <- rbindlist(form$choices[form$name == n], fill = TRUE)
    if ("value_version" %in% names(ch)) {
      ch$value_version <- NULL
    }
    ch <- unique(ch)
    ch <- ch[ch$value_lang %in% lang, ]
    ch$value_label <- make.unique(ch$value_label, sep = "_")
    vals <- setNames(ch$value_name, ch$value_label)
    vals[!duplicated(vals)]
  })
  setNames(choices, nm)
}

#' @importFrom labelled set_value_labels
#' @noRd
val_labels_from_form_ <- function(x, form, lang, precomputed_choices = NULL) {
  if (is.null(precomputed_choices)) {
    precomputed_choices <- precompute_val_labels_(form, lang)
  }
  nm <- intersect(names(x), names(precomputed_choices))
  if (length(nm) > 0) {
    choices <- precomputed_choices[nm]
    for (n in nm) {
      x[[n]] <- as.character(x[[n]])
    }
    x <- set_value_labels(x, .labels = choices, .strict = FALSE)
  }
  x
}

#' @importFrom labelled var_label<-
#' @noRd
val_labels_sm_from_form_ <- function(x, form, lang) {
  is_sm <- grepl("select_multiple", form$type, fixed = TRUE)
  nm <- intersect(names(x), unique(form$name[form$lang %in% lang & is_sm]))
  for (n in nm) {
    d <- x[[n]]
    vl <- var_label(d)
    lookup <- sm_lookup_(form, n, lang)
    x[[n]] <- sm_codes_to_labels_(d, lookup)
    var_label(x[[n]]) <- vl
    attr(x[[n]], "kobo_sm_lang") <- lang
    attr(x[[n]], "kobo_sm_unlabeled") <- sm_unlabeled_(d, lookup)
  }
  x
}

#' Codes of select_multiple answers that have no label and are kept as is
#' @importFrom stringi stri_split_fixed
#' @noRd
sm_unlabeled_ <- function(x, lookup) {
  x <- as.character(x)
  codes <- unique(unlist(stri_split_fixed(unique(x[!is.na(x)]), " ", omit_empty = TRUE)))
  codes <- as.character(codes)
  sort(codes[is.na(lookup[codes])])
}

#' Labels of the choices of a select_multiple question, named by code
#'
#' Labels shared by several choices are made unique, so each label maps back
#' to one code. A choice without a translation has an empty label, which is
#' taken as no label.
#' @importFrom data.table rbindlist
#' @noRd
sm_lookup_ <- function(form, n, lang) {
  is_sm <- grepl("select_multiple", form$type, fixed = TRUE)
  ch <- rbindlist(form$choices[form$name %in% n & form$lang %in% lang & is_sm],
                  fill = TRUE)
  if (nrow(ch) == 0) {
    return(setNames(character(), character()))
  }
  if ("value_version" %in% names(ch)) {
    ch$value_version <- NULL
  }
  ch <- unique(ch)
  ch <- ch[ch$value_lang %in% lang, ]
  ch$value_label[ch$value_label %in% ""] <- NA
  has_label <- !is.na(ch$value_label)
  ch$value_label[has_label] <- make.unique(ch$value_label[has_label], sep = "_")
  ch <- ch[!duplicated(ch$value_name), ]
  setNames(ch$value_label, ch$value_name)
}

#' Replace each code of a select_multiple answer by its label
#'
#' Answers are split on spaces and each code is looked up exactly. A code
#' without a label is kept as it is.
#' @importFrom stringi stri_split_fixed
#' @noRd
sm_codes_to_labels_ <- function(x, lookup) {
  x <- as.character(x)
  u <- unique(x[!is.na(x)])
  codes <- stri_split_fixed(u, " ", omit_empty = TRUE)
  labels <- vapply(codes, function(code) {
    lab <- lookup[code]
    lab[is.na(lab)] <- code[is.na(lab)]
    paste(lab, collapse = " ")
  }, character(1))
  labels[match(x, u)]
}

#' Codes of select_multiple answers written with labels
#'
#' Each answer is split into labels of the question, or into single words for
#' codes that had no label. `unlabeled` lists the codes known to be kept as
#' is, and they count as their own labels. The split with the fewest other
#' words is kept, and an answer with two equally good splits is an error, for
#' example an unlabeled code spelled like the label of another choice.
#' @noRd
sm_labels_to_codes_ <- function(x, lookup, name, unlabeled = character()) {
  x <- as.character(x)
  lookup[is.na(lookup)] <- names(lookup)[is.na(lookup)]
  lookup <- c(lookup, setNames(unlabeled, unlabeled))
  lookup <- lookup[!duplicated(names(lookup))]
  u <- unique(x[!is.na(x)])
  codes <- vapply(u, function(s) {
    res <- sm_split_(s, lookup)
    if (is.na(res)) {
      abort(
        c(
          paste0("Can't translate the answers of select_multiple question `", name, "`."),
          i = paste0("The answer \"", s, "\" matches its labels and codes in more than one way."),
          i = "Download the data in this language with `kobo_data(lang = )` instead."
        ),
        call = NULL
      )
    }
    res
  }, character(1), USE.NAMES = FALSE)
  codes[match(x, u)]
}

#' Split one answer into codes, NA when the split is ambiguous
#' @noRd
sm_split_ <- function(s, lookup) {
  n <- nchar(s)
  if (n == 0) {
    return(s)
  }
  starts <- c(1L, which(strsplit(s, "", fixed = TRUE)[[1]] == " ") + 1L)
  starts <- starts[starts <= n]
  unknown <- integer(n + 2L)
  ways <- integer(n + 2L)
  path <- character(n + 2L)
  ways[n + 2L] <- 1L
  lens <- nchar(lookup)
  for (i in rev(starts)) {
    rest <- substr(s, i, n)
    word <- sub(" .*", "", rest)
    hit <- startsWith(rest, lookup)
    cand_next <- c(i + lens[hit] + 1L, i + nchar(word) + 1L)
    cand_code <- c(names(lookup)[hit], word)
    cand_unknown <- c(integer(sum(hit)), 1L)
    ok <- nzchar(c(lookup[hit], word)) & ways[cand_next] > 0
    if (!any(ok)) next
    cost <- cand_unknown[ok] + unknown[cand_next[ok]]
    best <- which(cost == min(cost))
    unknown[i] <- min(cost)
    ways[i] <- min(sum(ways[cand_next[ok][best]]), 2L)
    path[i] <- paste(c(cand_code[ok][best[1]], path[cand_next[ok][best[1]]]),
                     collapse = " ")
  }
  if (ways[1] != 1L) {
    return(NA_character_)
  }
  trimws(path[1], "right")
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
select_multiple_var_label <- function(x, form, lang, sep = "_") {
  labels <- list()
  bool <- stri_detect_regex(form$type, "select_multiple")
  if (any(bool)) {
    choices <- filter(
      form,
      .data$lang %in% !!lang,
      stri_detect_regex(.data$type, "select_multiple")
    ) |>
      unnest("choices") |>
      filter(.data$value_lang %in% !!lang) |>
      select(-any_of("value_version")) |>
      distinct() |>
      transmute(
        value_name = paste0(.data$name, sep, .data$value_name),
        value_label = paste0(.data$label, "::", .data$value_label)
      ) |>
      distinct()
    labels <- setNames(
      as.list(choices$value_label),
      make.unique(choices$value_name, sep = "_")
    )
  }
  labels
}

#' Separator of select_multiple dummy columns
#'
#' The separator is the most common string between the name of a
#' select_multiple question and one of its choice codes in `col_names`. It
#' falls back to `"_"` when no dummy column is found.
#' @noRd
sm_sep_ <- function(col_names, form) {
  sm <- form[grepl("select_multiple", form$type, fixed = TRUE), ]
  sm <- sm[!is.na(sm$name) & !duplicated(sm$name), ]
  seps <- character(0)
  for (i in seq_len(nrow(sm))) {
    n <- sm$name[i]
    rest <- col_names[startsWith(col_names, n)]
    rest <- substring(rest, nchar(n) + 1L)
    rest <- rest[nzchar(rest)]
    codes <- unique(sm$choices[[i]]$value_name)
    for (k in codes[!is.na(codes)]) {
      hit <- rest[endsWith(rest, k) & nchar(rest) > nchar(k)]
      seps <- c(seps, substring(hit, 1L, nchar(hit) - nchar(k)))
    }
  }
  if (length(seps) == 0L) {
    return("_")
  }
  counts <- table(seps)
  names(counts)[which.max(counts)]
}

#' @importFrom stats setNames
#' @noRd
precompute_var_labels_ <- function(form, lang, sep = "_") {
  form_lang <- form[form$lang %in% lang, ]
  form_labels <- setNames(as.list(form_lang$label), form_lang$name)
  sm_labels <- select_multiple_var_label(
    x = NULL,
    form = form_lang,
    lang = lang,
    sep = sep
  )
  list(form_labels = form_labels, sm_labels = sm_labels)
}

#' @importFrom labelled set_variable_labels
#' @importFrom stats setNames
#' @noRd
var_labels_from_form_ <- function(
  x,
  form,
  lang,
  precomputed_var_labels = NULL,
  sep = "_"
) {
  if (is.null(precomputed_var_labels)) {
    precomputed_var_labels <- precompute_var_labels_(form, lang, sep)
  }
  form_labels <- precomputed_var_labels$form_labels
  sm_labels <- precomputed_var_labels$sm_labels
  nm <- intersect(names(x), names(form_labels))
  nm_sm <- names(sm_labels)
  nm_missing <- setdiff(names(x), nm)
  nm_missing <- setdiff(nm_missing, nm_sm)
  if (length(nm) > 0) {
    labels <- form_labels[nm]
    labels_missing <- setNames(as.list(nm_missing), nm_missing)
    labels_sm <- sm_labels[intersect(names(x), nm_sm)]
    labels <- c(labels, labels_missing, labels_sm)
    labels <- replace_na_list_efficient_(labels)
    x <- set_variable_labels(x, .labels = labels, .strict = FALSE)
  } else {
    labels <- setNames(as.list(names(x)), names(x))
    x <- set_variable_labels(x, .labels = labels, .strict = FALSE)
  }
  x
}

#' @noRd
#' @importFrom dplyr rename_with across mutate
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
      lbl <- lapply(seq_along(lbl), function(i) {
        setNames(paste0(lbl[[i]], "::", nm), paste0(names(lbl)[i], "_", nm))
      })
      lbl <- lapply(lbl, as.list)
      lbl <- unlist(lbl, recursive = FALSE)

      x <- x |>
        mutate(across(
          .cols = all_of(col),
          .fns = ~ wkt_geopoint(.x),
          .names = "{.col}_wkt"
        )) |>
        separate_wider_delim(
          {{ col }},
          names = c("latitude", "longitude", "altitude", "precision"),
          delim = " ",
          names_sep = "_",
          cols_remove = FALSE
        ) |>
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
      vapply(
        x,
        \(s) paste0("LINESTRING (", paste0(s, collapse = ", "), ")"),
        character(1)
      )
    }

    separate_geotrace <- function(x, col) {
      lbl <- var_label(select(x, all_of(col)))
      lbl <- set_names(lbl, paste0(col, "_wkt"))
      lbl <- lapply(lbl, function(x) paste0(x, "::", "wkt"))

      x <- x |>
        mutate(across(
          .cols = all_of(col),
          .fns = ~ wkt_geotrace(.x),
          .names = "{.col}_wkt"
        )) |>
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
      x <- vapply(
        x,
        \(s) paste0("POLYGON ((", paste0(s, collapse = ", "), "))"),
        character(1)
      )
      x
    }

    separate_geoshape <- function(x, col) {
      lbl <- var_label(select(x, all_of(col)))
      lbl <- set_names(lbl, paste0(col, "_wkt"))
      lbl <- lapply(lbl, function(x) paste0(x, "::", "wkt"))

      x <- x |>
        mutate(across(
          .cols = all_of(col),
          .fns = ~ wkt_geoshape(.x),
          .names = "{.col}_wkt"
        )) |>
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
    val_stat <- vapply(
      val_col,
      \(x) {
        if (!is_uid_available(x)) {
          NA_character_
        } else {
          x$uid
        }
      },
      character(1)
    )
    val_lbl <- vapply(
      val_col,
      \(x) {
        if (!is_label_available(x)) {
          NA_character_
        } else {
          x$label
        }
      },
      character(1)
    )
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
postprocess_data_ <- function(
  x,
  form,
  lang,
  select_multiple_label = FALSE,
  cn,
  select_multiple_sep = "_",
  precomputed_choices = NULL,
  precomputed_var_labels = NULL,
  fields = NULL
) {
  x <- dummy_from_form_(x, form, sep = select_multiple_sep)
  x <- extract_geopoint_(x, form)
  x <- extract_geotrace_(x, form)
  x <- extract_geoshape_(x, form)
  x <- type_convert_(x, form)
  x <- remove_list_cols(x)
  if (!is.null(fields)) {
    x <- add_missing_cols_(x, intersect(cn, fields))
  } else {
    x <- add_missing_cols_(x, cn)
  }
  x <- val_labels_from_form_(
    x = x,
    form = form,
    lang = lang,
    precomputed_choices = precomputed_choices
  )
  x <- var_labels_from_form_(
    x = x,
    form = form,
    lang = lang,
    precomputed_var_labels = precomputed_var_labels,
    sep = select_multiple_sep
  )
  if (isTRUE(select_multiple_label)) {
    x <- val_labels_sm_from_form_(x = x, form = form, lang = lang)
  }
  x <- reorder_cols_(x, cn)
}

#' @noRd
kobo_question_types <- function() {
  c(
    "start",
    "end",
    "today",
    "deviceid",
    "phonenumber",
    "username",
    "email",
    "audit",
    "integer",
    "decimal",
    "range",
    "text",
    "select_one",
    "select_multiple",
    "select_one_from_file",
    "select_multiple_from_file",
    "rank",
    "note",
    "geopoint",
    "geotrace",
    "geoshape",
    "date",
    "time",
    "dateTime",
    "datetime",
    "image",
    "audio",
    "background-audio",
    "video",
    "file",
    "barcode",
    "calculate",
    "acknowledge",
    "hidden"
  )
}

#' @noRd
kobo_display_fields <- function() {
  c(
    "label",
    "hint",
    "guidance_hint",
    "constraint_message",
    "required_message",
    "image",
    "media::image",
    "big-image",
    "media::big-image",
    "audio",
    "media::audio",
    "video",
    "media::video"
  )
}

#' @importFrom rlang set_names
#' @noRd
empty_tibble_ <- function(cnames) {
  cnames <- cnames[!is.na(cnames)]
  tibble(!!!cnames, .rows = 0, .name_repair = ~cnames)
}

#' TRUE when columns are named after their labels
#'
#' Columns renamed by `colnames_label = TRUE` keep their question name in the
#' `kobo_name` attribute. Data without it, from older versions, is taken as
#' named after labels when more columns match a label than a question name.
#' @noRd
is_label_cols_ <- function(tbls, form) {
  if (has_kobo_names_(tbls)) {
    return(TRUE)
  }
  cols <- unlist(lapply(tbls, names), use.names = FALSE)
  sum(cols %in% form$label) > sum(cols %in% form$name)
}

#' TRUE when some column keeps its question name in `kobo_name`
#' @noRd
has_kobo_names_ <- function(tbls) {
  any(vapply(tbls, function(tbl) {
    any(vapply(tbl, function(col) !is.null(attr(col, "kobo_name", exact = TRUE)),
               logical(1)))
  }, logical(1)))
}

#' Returns c("original_name" = "label_col", ...) for use with rename()
#'
#' Labels shared by several questions are skipped, since they cannot be mapped
#' back.
#' @noRd
reverse_label_lookup_ <- function(label_cols, form, lang) {
  form_lang <- form[form$lang %in% lang, ]
  name_to_lbl <- setNames(form_lang$label, form_lang$name)
  name_to_lbl <- name_to_lbl[!duplicated(names(name_to_lbl)) & !is.na(name_to_lbl)]
  shared <- name_to_lbl[duplicated(name_to_lbl)]
  name_to_lbl <- name_to_lbl[!name_to_lbl %in% shared]
  in_data <- name_to_lbl[name_to_lbl %in% label_cols]
  setNames(unname(in_data), names(in_data))
}

#' Column names of each table of a repeat group form
#'
#' Scopes are read in each form version, so a field only present in an older
#' version goes to the table it had in that version. A field that moved between
#' tables is listed in both.
#' @noRd
kobo_form_name_to_list_ <- function(x, sep) {
  x$scope <- repeat_scope_(x)
  x <- x[!x$type %in% c("begin_repeat", "end_repeat"), ]
  x <- x[!duplicated(x[c("version", "name", "scope")]), ]
  x <- kobo_form_extra_(x, sep)
  x <- x[!duplicated(x[c("cnames", "scope")]), ]
  split(x$cnames, factor(x$scope, levels = unique(c("main", x$scope))))
}

#' Question names of the columns
#'
#' A column renamed after its label keeps its question name in the
#' `kobo_name` attribute.
#' @noRd
kobo_names_ <- function(x) {
  nm <- names(x)
  vapply(seq_along(nm), function(i) {
    attr(x[[i]], "kobo_name", exact = TRUE) %||% nm[i]
  }, character(1))
}

#' Rename columns and keep their question names in `kobo_name`
#'
#' Every column gets the attribute, renamed or not, so data named after
#' labels stays recognizable when the labels equal the question names.
#' @noRd
rename_kobo_ <- function(x, new_names) {
  orig <- kobo_names_(x)
  cls <- class(x)
  x <- unclass(x)
  for (i in seq_along(x)) {
    attr(x[[i]], "kobo_name") <- orig[i]
  }
  names(x) <- new_names
  class(x) <- cls
  x
}

#' @importFrom labelled var_label
#' @noRd
set_names_from_varlabel_ <- function(x) {
  lbl <- var_label(x)
  nm <- names(x)
  lbl <- vapply(seq_along(nm), function(i) {
    l <- lbl[[i]]
    if (is.null(l) || is.na(l)) nm[i] else l
  }, character(1))
  rename_kobo_(x, make.unique(lbl, sep = "_"))
}

#' @importFrom dplyr mutate case_when
#' @importFrom stats na.omit
#' @noRd
kobo_form_extra_ <- function(form, sep) {
  media_type <- c("image", "video", "audio", "file")
  if ("choices" %in% names(form)) {
    res <- mutate(
      form,
      "value_name" = case_when(
        type %in% "geopoint" ~ list(c(
          "",
          "_latitude",
          "_longitude",
          "_altitude",
          "_precision",
          "_wkt"
        )),
        type %in% "geotrace" ~ list(c("", "_wkt")),
        type %in% "geoshape" ~ list(c("", "_wkt")),
        stri_detect_regex(type, "select_multiple") ~ lapply(
          .data$choices,
          \(ch) c("", paste0(sep, unique(ch$value_name)))
        ),
        .default = list("")
      )
    ) |>
      unnest("value_name") |>
      mutate(cnames = paste0(.data$name, .data$value_name))
  } else {
    res <- mutate(
      form,
      "value_name" = case_when(
        type %in% "geopoint" ~ list(c(
          "",
          "_latitude",
          "_longitude",
          "_altitude",
          "_precision",
          "_wkt"
        )),
        type %in% "geotrace" ~ list(c("", "_wkt")),
        type %in% "geoshape" ~ list(c("", "_wkt")),
        .default = list("")
      )
    ) |>
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
  int_q <- "integer"
  dbl_q <- c("decimal", "range")
  char_q <- c(
    "device_id",
    "phonenumber",
    "username",
    "email",
    "audit",
    "select_one",
    "select_multiple",
    "select_one_from_file",
    "select_multiple_from_file",
    "rank",
    "text",
    "note",
    "barcode",
    "acknowledge",
    "hidden",
    "geopoint",
    "geotrace",
    "geoshape",
    "image",
    "audio",
    "background-audio",
    "video",
    "file"
  )
  form$cols <- case_when(
    form$type %in% datetime_q ~ "T",
    form$type %in% date_q ~ "D",
    form$type %in% time_q ~ "c",
    form$type %in% int_q ~ "i",
    form$type %in% dbl_q ~ "d",
    form$type %in% char_q ~ "c",
    .default = "?"
  )
  v <- form$cols[match(names(x), form$name)]
  sys_types <- c(
    `_id` = "i",
    `formhub/uuid` = "c",
    `__version__` = "c",
    `meta/instanceID` = "c",
    `meta/deprecatedID` = "c",
    `meta/rootUuid` = "c",
    `_xform_id_string` = "c",
    `_uuid` = "c",
    `_status` = "c",
    `_submitted_by` = "c",
    `_submission_time` = "T",
    `_index` = "i",
    `_parent_index` = "i",
    `_parent_table_name` = "c",
    `_validation_status` = "c"
  )
  v <- coalesce(v, unname(sys_types[names(x)]))
  sm_names <- unique(form$name[grepl(
    "select_multiple",
    form$type,
    fixed = TRUE
  )])
  if (length(sm_names) > 0) {
    sm_pat <- paste0("^(", paste(sm_names, collapse = "|"), ")_")
    is_dummy <- grepl(sm_pat, names(x)) & is.na(v)
    v[is_dummy] <- "i"
  }
  paste(coalesce(v, "?"), collapse = "")
}

#' Form used to read the submissions
#'
#' The deployed form comes first, so its labels win. With `all_versions`, the
#' other deployed versions found in the submissions follow, newest first, and
#' add the questions and choices the deployed form lacks.
#' @noRd
kobo_form_version_ <- function(x, asset, all_versions) {
  form <- kobo_form(asset)
  has_version <- sum(grepl("\\_version\\_", names(x))) == 1
  if (!all_versions || !has_version)
    return(form)

  uid <- asset$uid
  form_versions <- kobo_asset_version_list(uid)
  form_versions <- unique(form_versions$uid[form_versions$deployed])
  versions <- intersect(form_versions, unique(x[["__version__"]]))
  versions <- setdiff(versions, unique(form$version))
  if (length(versions) == 0)
    return(form)

  uncached <- versions[!vapply(versions, \(v) has_cached_form_(uid, v), logical(1))]
  if (length(uncached) > 1) {
    asset_vers <- tryCatch(
      fetch_asset_versions_async_(uid, uncached),
      error = function(e) NULL
    )
    if (!is.null(asset_vers)) {
      for (i in seq_along(uncached)) {
        frm <- kobo_form_api_(asset_vers[[i]], uncached[i])
        set_cached_form_(uid, uncached[i], frm)
      }
    } else {
      for (v in uncached) {
        kobo_form(asset, v)
      }
    }
  } else if (length(uncached) == 1) {
    kobo_form(asset, uncached)
  }
  form <- c(list(form), lapply(versions, \(v) get_cached_form_(uid, v)))
  dt2tibble(rbindlist(form, fill = TRUE))
}

#' @importFrom crul AsyncQueue HttpRequest
#' @importFrom RcppSimdJson fparse
#' @importFrom rlang abort
#' @noRd
fetch_asset_versions_async_ <- function(uid, versions) {
  headers <- list(
    Authorization = paste("Token", Sys.getenv("KOBOTOOLBOX_TOKEN"))
  )
  base_url <- Sys.getenv("KOBOTOOLBOX_URL")
  reqs <- lapply(versions, function(v) {
    url <- file.path(
      base_url,
      paste0("api/v2/assets/", uid, "/versions/", v)
    )
    HttpRequest$new(url, headers = headers)$retry(
      "get",
      times = 3L,
      retry_only_on = c(500, 503),
      terminate_on = 404
    )
  })
  res <- AsyncQueue$new(
    .list = reqs,
    bucket_size = length(reqs),
    sleep = 0.05
  )
  res$request()
  status <- res$status_code()
  cond <- status >= 300L
  if (any(cond)) {
    msg <- res$content()[cond]
    abort(error_msg(msg[[1]]), call = NULL)
  }
  parsed <- res$parse(encoding = "UTF-8")
  lapply(seq_along(versions), function(i) {
    asset_ver <- fparse(parsed[[i]], max_simplify_lvl = "list")
    asset_ver$asset_uid <- uid
    structure(asset_ver, class = "kobo_asset_version")
  })
}

#' @noRd
external_file_lookup_ <- function(x) {
  x <- scan(
    text = gsub("value\\=", "name\\=", x),
    what = character(),
    sep = ",",
    quiet = TRUE
  )
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
    mutate(
      value_name = as.character(.data$name),
      value_label = as.character(.data$label),
      value_lang = as.character(lang),
      .keep = "none"
    )
}

#' @importFrom purrr list_rbind
#' @noRd
external_files_choice_ <- function(
  urls,
  lookup = NULL,
  lang = "Labels",
  n_retry = 3L
) {
  headers <- list(
    Authorization = paste("Token", Sys.getenv("KOBOTOOLBOX_TOKEN"))
  )
  reqs <- lapply(urls, function(url) {
    req <- HttpRequest$new(url, headers = headers)
    req$retry(
      "get",
      times = n_retry,
      retry_only_on = c(500, 503),
      terminate_on = 404
    )
  })
  res <- AsyncQueue$new(.list = reqs, bucket_size = 15L, sleep = 0.01)
  res$request()
  cond <- res$status_code() >= 300L
  if (any(cond)) {
    msg <- res$content()[cond]
    abort(error_msg(msg[[1]]), call = NULL)
  }

  res <- res$content()
  res <- lapply(res, \(f) {
    parse_external_csv_(fread(text = rawToChar(f)), lookup, lang)
  })
  res
}
