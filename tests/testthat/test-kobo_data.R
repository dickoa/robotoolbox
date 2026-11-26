test_that("with kobo_data you can read your submissions", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aYuTZn9vegi3Z49MXwKjep"
  vcr::use_cassette("kobo_data", {
    asset <- kobo_asset(uid)
    raw_uid <- kobo_data(uid)
    raw_asset <- kobo_data(asset)
  })
  expect_equal(raw_uid, raw_asset)
  expect_equal(class(raw_uid),
               c("tbl_df", "tbl", "data.frame"))
  expect_error(kobo_data(1L))
  expect_error(kobo_data("bad_uid"))
})

test_that("kobo_data pagination works", {
  skip_on_cran()
  skip_if(!has_live_api(), message = "Requires live API (async requests)")
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aYuTZn9vegi3Z49MXwKjep"
  asset <- kobo_asset(uid)
  raw_uid <- kobo_data(uid)
  raw_paginate <- kobo_data(asset, paginate = TRUE, page_size = 2)
  raw_paginate2 <- kobo_data(asset, paginate = TRUE) ## default
  expect_equal(raw_uid, raw_paginate)
  expect_equal(raw_paginate, raw_paginate2)
})

test_that("kobo_data with multiple languages", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aYuTZn9vegi3Z49MXwKjep"
  vcr::use_cassette("kobo_data_multilang", {
    raw_en <- kobo_data(uid, lang = "English (en)")
    raw_fr <- kobo_data(uid, lang = "Francais (fr)")
    raw_ar <- kobo_data(uid, lang = "Arabic (ar)")
  })

  expect_s3_class(raw_en,
            c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(raw_fr,
            c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(raw_ar,
            c("tbl_df", "tbl", "data.frame"))

})


test_that("repeating groups use the dm package", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aANhxwX9S6BCsiYMgQj9kV"
  vcr::use_cassette("kobo_data_dm", {
    raw_uid <- kobo_data(uid)
  })
  expect_s3_class(raw_uid, "dm")
})

test_that("kobo_submissions is similar to kobo_data", {
  skip_on_cran()
  skip_if(!has_live_api(), message = "Requires live API (async requests)")
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aYuTZn9vegi3Z49MXwKjep"
  asset <- kobo_asset(uid)
  raw_uid1 <- kobo_data(uid)
  raw_asset1 <- kobo_data(asset)
  raw_paginate1 <- kobo_data(asset, paginate = TRUE, page_size = 2)

  raw_uid2 <- kobo_submissions(uid)
  raw_asset2 <- kobo_submissions(asset)
  raw_paginate2 <- kobo_submissions(asset, paginate = TRUE, page_size = 2)

  expect_equal(raw_uid1, raw_uid2)
  expect_equal(raw_asset1, raw_asset2)
  expect_equal(raw_paginate1, raw_paginate2)
})

test_that("kobo_data can use select_multiple labels instead of values", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "atbUaNGu5PWR2u4tNDsYaH"
  vcr::use_cassette("kobo_data_sm_labels", {
    form <- kobo_form(uid)
    cond <- form$type %in% "select_multiple"
    sm_col <- form$name[cond]
    sm_col <- sm_col[1]
    choices <- form$choices[form$name %in% sm_col][[1]]
    label <- unique(choices$value_label)
    raw_label <- kobo_data(uid,
                           select_multiple_label = TRUE)
  })

  expect_true(all(grepl(paste(label,
                              collapse = "|"),
                        na.omit(raw_label[[sm_col]]))))
})

test_that("kobo_data parse geodata columns", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "a9NCKTJxBPKdy49gX57WL5"
  vcr::use_cassette("kobo_data_geodata", {
    form <- kobo_form(uid)
    raw <- kobo_data(uid)
  })

  gp_exists <- form$type %in% "geopoint"
  gt_exists <- form$type %in% "geotrace"
  gs_exists <- form$type %in% "geoshape"

  if (any(gp_exists)) {
    gp_nm <- form$name[form$type %in% "geopoint"][1]
    expect_type(raw[[paste0(gp_nm, "_latitude")]], "double")
    expect_type(raw[[paste0(gp_nm, "_longitude")]], "double")
    expect_match(raw[[paste0(gp_nm, "_wkt")]], "^POINT \\(.+")
  }

  if (any(gt_exists)) {
    gt_nm <- form$name[form$type %in% "geotrace"][1]
    expect_match(raw[[paste0(gt_nm, "_wkt")]], "^LINESTRING \\(.+")
  }

  if (any(gs_exists)) {
    gs_nm <- form$name[form$type %in% "geoshape"][1]
    expect_match(raw[[paste0(gs_nm, "_wkt")]], "^POLYGON \\(.+")
  }
})

test_that("with kobo_data you can read form with 0 submission", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aPgHnrLSLFoUUABtUXV6cH"
  vcr::use_cassette("kobo_data_empty", {
    raw <- kobo_data(uid)
  })
  expect_equal(class(raw),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(raw), 0)
})

test_that("with kobo_data you can read data when group names changed", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aDMHypRASH2EuJdvKMx5Mi"
  vcr::use_cassette("kobo_data_group_names", {
    raw <- kobo_data(uid)
  })
  expect_equal(class(raw),
               c("tbl_df", "tbl", "data.frame"))
})

test_that("with kobo_data you have access to validation status", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "apBgxt6Nw4Vcjnn7wYpF33"
  vcr::use_cassette("kobo_data_validation", {
    raw <- kobo_data(uid)
  })
  expect_equal(class(raw[["_validation_status"]]),
               c("haven_labelled", "vctrs_vctr", "character"))
})

test_that("all_versions = FALSE returns every submission and moved questions", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aDMHypRASH2EuJdvKMx5Mi"
  vcr::use_cassette("kobo_data_latest_version", {
    raw1 <- kobo_data(uid, all_versions = FALSE)
    raw2 <- kobo_data(uid, all_versions = TRUE)
  })
  expect_equal(raw1$`_id`, raw2$`_id`)
  expect_equal(length(unique(raw1$`__version__`)), 3L)
  expect_false(anyNA(raw1$hh_sex))
  expect_equal(as.vector(raw1$hh_sex), as.vector(raw2$hh_sex))
})

test_that("kobo_data does not work with non survey assets", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aDorbmteGaKUCbcn865Grh"
  vcr::use_cassette("kobo_data_non_survey", {
    expect_error(kobo_data(uid))
  })
})

test_that("kobo_data has a progress parameter", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aDMHypRASH2EuJdvKMx5Mi"

  vcr::use_cassette("kobo_data_progress", {
    expect_message(kobo_data(uid, progress = TRUE), "Downloading data")
  })
})

test_that("kobo_data can use labels as colnames", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aDMHypRASH2EuJdvKMx5Mi"
  vcr::use_cassette("kobo_data_colnames_label", {
    raw <- kobo_data(uid, colnames_label = TRUE)
  })
  expect_in("What is your name?", names(raw))

  ## works with dm too
  uid_dm <- "aANhxwX9S6BCsiYMgQj9kV"
  vcr::use_cassette("kobo_data_colnames_label_dm", {
    raw_dm <- kobo_data(uid_dm, colnames_label = TRUE)
  })
  expect_in("How many hobbies does ${name} have?", names(raw_dm$demo))
})

test_that("select_multiple column can have different separators", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "atbUaNGu5PWR2u4tNDsYaH"
  kobo_cache_clear(uid)
  vcr::use_cassette("kobo_data_sm_sep", {
    form <- kobo_form(uid)
    cond <- form$type %in% "select_multiple"
    sm_col <- form$name[cond]
    sm_col <- sm_col[1]
    choices <- form$choices[form$name %in% sm_col][[1]]
    label <- unique(choices$value_name)
    raw_label <- kobo_data(uid,
                           select_multiple_sep = "@")
  })
  expected <- paste0(sm_col, "@", label)
  expect_true(all(expected %in% names(raw_label)))
})

test_that("kobo_attachment_download downloads files", {
  skip_on_cran()
  skip_if(!has_live_api(), message = "Requires live API (async requests)")
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aSCXrTdtrxHDnx3n4DHefK"
  dir <- tempdir(check = TRUE)
  kobo_attachment_download(uid, dir,
                           progress = TRUE,
                           overwrite = TRUE)

  ## check overwrite
  kobo_attachment_download(kobo_asset(uid),
                           dir,
                           progress = FALSE,
                           overwrite = FALSE)

  files <- list.files(dir, pattern = "^att.*\\.mp4$")
  expect_true(length(files) > 0)
})

test_that("kobo_attachment_download saves each file under its own submission", {
  skip_on_cran()
  skip_if(!has_live_api(), message = "Requires live API (async requests)")
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip_if(url == "" & token == "", message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aSCXrTdtrxHDnx3n4DHefK"
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  paths <- kobo_attachment_download(uid, dir)
  att <- get_attachment_url_(uid)
  expect_equal(length(unique(paths)), nrow(att))
  headers <- list(Authorization = paste("Token", token))
  for (i in seq_len(nrow(att))) {
    f <- file.path(dir, paste0(att$uid[i], "_", att$media_file_basename[i]))
    direct <- crul::HttpClient$new(att$download_url[i], headers = headers)$get()$content
    expect_identical(readBin(f, "raw", file.size(f)), direct, info = att$uid[i])
    expect_match(att$download_url[i], paste0("/data/", att$`_id`[i], "/"))
  }
  main <- kobo_data(uid)$main
  linked <- unlist(lapply(main$`_attachments`, \(a) {
    if (length(a)) paste0(a$uid, "_", a$media_file_basename)
  }))
  expect_setequal(linked, basename(paths))
})


test_that("kobo_attachment_download fails gracefully with invalid uid", {
  skip_on_cran()
  skip_if(!has_live_api(), message = "Requires live API (async requests)")
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""

  kobo_setup(url = url, token = token)
  skip_if(skip,
          message = "Test server not configured")
  expect_error(kobo_attachment_download("invalid_uid", tempdir()))
})

test_that("kobo_attachment_download handles no attachments", {
  skip_on_cran()
  skip_if(!has_live_api(), message = "Requires live API (async requests)")
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid_no_attachments <- "aREsLnfwNU9L7ePbUjnajg"
  dir <- tempdir()
  existing <- list.files(dir, pattern = "^att", full.names = TRUE)
  if (length(existing) > 0)
    file.remove(existing)
  result <- kobo_attachment_download(uid_no_attachments, dir)
  expect_true(length(list.files(dir, pattern = "^att")) == 0)
})

test_that("kobo_data supports query parameter for server-side filtering", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aYuTZn9vegi3Z49MXwKjep"
  vcr::use_cassette("kobo_data_query", {
    asset <- kobo_asset(uid)
    all_data <- kobo_data(asset)
    filtered <- kobo_data(asset, query = '{"pet_yesno": "0"}')
  })

  expect_s3_class(filtered, "data.frame")
  expect_lt(nrow(filtered), nrow(all_data))
  expect_true(all(filtered$pet_yesno == "0"))
})

test_that("kobo_data supports fields parameter for column selection", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aYuTZn9vegi3Z49MXwKjep"
  vcr::use_cassette("kobo_data_fields", {
    asset <- kobo_asset(uid)
    partial <- kobo_data(asset, fields = c("full_name", "pet_yesno"))
    full <- kobo_data(asset)
  })

  expect_s3_class(partial, "data.frame")
  expect_true("full_name" %in% names(partial))
  expect_true("pet_yesno" %in% names(partial))
  expect_lt(ncol(partial), ncol(full))
})

test_that("kobo_data query with no results returns empty tibble", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aYuTZn9vegi3Z49MXwKjep"
  vcr::use_cassette("kobo_data_query_empty", {
    asset <- kobo_asset(uid)
    empty <- kobo_data(asset, query = '{"pet_yesno": "impossible_value_xyz"}')
  })

  expect_s3_class(empty, "data.frame")
  expect_equal(nrow(empty), 0)
})

test_that("kobo_data validates query parameter", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aYuTZn9vegi3Z49MXwKjep"
  vcr::use_cassette("kobo_data_validate_query", {
    asset <- kobo_asset(uid)
  })

  expect_error(kobo_data(asset, query = 123),
               "`query` must be a single character string")
  expect_error(kobo_data(asset, query = c('{"a": "1"}', '{"b": "2"}')),
               "`query` must be a single character string")
  expect_error(kobo_data(asset, query = "not json"),
               "`query` must be a valid JSON object string")
  expect_error(kobo_data(asset, query = '["array"]'),
               "`query` must be a valid JSON object string")
})

test_that("kobo_data validates fields parameter", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aYuTZn9vegi3Z49MXwKjep"
  vcr::use_cassette("kobo_data_validate_fields", {
    asset <- kobo_asset(uid)
  })

  expect_error(kobo_data(asset, fields = 123),
               "`fields` must be a non-empty character vector")
  expect_error(kobo_data(asset, fields = character(0)),
               "`fields` must be a non-empty character vector")
})

test_that("kobo_data returns every submission when page_size exceeds the server limit", {
  skip_on_cran()
  token <- Sys.getenv("KOBOTOOLBOX_KF_TOKEN")
  skip_if(token == "", message = "kf.kobotoolbox.org not configured")
  old_url <- Sys.getenv("KOBOTOOLBOX_URL")
  old_token <- Sys.getenv("KOBOTOOLBOX_TOKEN")
  old_size <- Sys.getenv("KOBOTOOLBOX_PAGE_SIZE")
  on.exit({
    Sys.setenv(
      KOBOTOOLBOX_URL = old_url,
      KOBOTOOLBOX_TOKEN = old_token,
      KOBOTOOLBOX_PAGE_SIZE = old_size
    )
    .robotoolbox_cache$page_cap <- list()
  })
  .robotoolbox_cache$page_cap <- list()
  kobo_setup(url = "https://kf.kobotoolbox.org", token = token, page_size = 5000L)
  uid <- "akPxKmGBC8kUrRMjodbxGV"
  n <- kobo_asset(uid)$deployment__submission_count
  expect_message(res <- kobo_data(uid, fields = "_id"), "at most 1000")
  expect_equal(nrow(res), n)
  expect_false(anyDuplicated(res$`_id`) > 0)

  res_paged <- kobo_data(uid, fields = "_id", page_size = 600)
  expect_equal(nrow(res_paged), n)
})

## The stored submission count is only a hint

ml_rows <- function(ids) {
  data.frame(
    `_id` = ids,
    `__version__` = rep(asset_ml$deployed_version_id, length(ids)),
    full_name = rep("A", length(ids)),
    check.names = FALSE
  )
}

test_that("kobo_data downloads submissions when the stored count is outdated", {
  checked <- character()
  for (count in list(0L, NULL, "many", "unknown", "3", "0")) {
    for (paginate in list(NULL, TRUE)) {
      asset <- asset_ml
      asset["deployment__submission_count"] <- list(count)
      server <- fake_server(total = 3, rows = ml_rows)
      with_fake_server(server, {
        res <- kobo_data(asset, paginate = paginate)
      })
      expect_equal(as.vector(res$`_id`), 1:3)
      expect_equal(as.vector(res$full_name), rep("A", 3))
      checked <- c(checked, paste(deparse(count), deparse(paginate)))
    }
  }
  expect_length(unique(checked), 12L)
})

test_that("kobo_data uses the filtered count from the server with a query", {
  asset <- asset_ml
  asset$deployment__submission_count <- 0L
  server <- fake_server(total = 2, rows = ml_rows)
  with_fake_server(server, {
    local_mocked_bindings(get_filtered_count_ = function(uid, query) 2L)
    res <- kobo_data(asset, query = '{"full_name": "A"}')
  })
  expect_equal(as.vector(res$`_id`), 1:2)
})

test_that("kobo_data downloads with a query when the server gives no count", {
  asset <- asset_ml
  asset$deployment__submission_count <- 0L
  server <- fake_server(total = 2, rows = ml_rows, count = "many")
  with_fake_server(server, {
    local_mocked_bindings(get_filtered_count_ = function(uid, query) NA_integer_)
    res <- kobo_data(asset, query = '{"full_name": "A"}')
    res2 <- kobo_data(asset, query = '{"full_name": "A"}', paginate = TRUE)
  })
  expect_equal(as.vector(res$`_id`), 1:2)
  expect_equal(as.vector(res2$`_id`), 1:2)
})

## Query validation

test_that("kobo_data sends every JSON object query to the server", {
  local_mocked_bindings(get_filtered_count_ = function(uid, query) 2L)
  for (q in c("{}", " {} ", '\n{"full_name": "A"}\t', '{"a": [1, {"b": 2}]}')) {
    sent <- NULL
    server <- fake_server(total = 2, rows = ml_rows)
    fetch <- server$fetch
    server$fetch <- function(uid, start, limit, query = NULL, fields = NULL, ...) {
      sent <<- query
      fetch(uid, start, limit, query, fields, ...)
    }
    with_fake_server(server, {
      res <- kobo_data(asset_ml, query = q)
    })
    expect_equal(nrow(res), 2L, info = q)
    expect_identical(sent, q, info = q)
  }
})

test_that("kobo_data rejects a query that is not a JSON object", {
  local_mocked_bindings(get_filtered_count_ = function(...) stop("no request expected"))
  bad <- c('[{"a": 1}]', "[]", '["array"]', "1", '"x"', "null", "", "not json",
           '{"a": 1} trailing', '{"a": 1}}', '{"a": }')
  for (q in bad) {
    expect_error(kobo_data(asset_ml, query = q),
                 "`query` must be a valid JSON object string", info = q)
  }
  expect_error(kobo_data(asset_ml, query = NA_character_),
               "`query` must be a single character string")
})

test_that("kobo_data returns an empty table when the server has no submission", {
  asset <- asset_ml
  asset$deployment__submission_count <- 0L
  server <- fake_server(total = 0, rows = ml_rows)
  with_fake_server(server, {
    res <- kobo_data(asset)
  })
  expect_equal(nrow(res), 0L)
  expect_true(all(c("full_name", "pet_yesno") %in% names(res)))
  expect_equal(server$calls$n, 1L)
})

test_that("kobo_data makes no request for a survey that was never deployed", {
  asset <- asset_ml
  asset$deployment__submission_count <- 0L
  asset$has_deployment <- FALSE
  server <- fake_server(total = 3, rows = ml_rows)
  with_fake_server(server, {
    res <- kobo_data(asset)
  })
  expect_equal(nrow(res), 0L)
  expect_true(all(c("full_name", "pet_yesno") %in% names(res)))
  expect_equal(server$calls$n, 0L)
})

test_that("kobo_data returns empty tables for a form with repeats never deployed", {
  asset <- asset_rg
  asset$uid <- "aNeverDeployedForm00000"
  asset$deployment__submission_count <- 0L
  asset$has_deployment <- FALSE
  asset["deployed_version_id"] <- list(NULL)
  asset$deployed_versions <- list(count = 0L, results = list())
  on.exit(kobo_cache_clear(asset$uid))
  expect_equal(unique(kobo_form(asset)$version), asset$version_id)
  res <- kobo_data(asset)
  expect_s3_class(res, "dm")
  expect_equal(
    names(res),
    c("main", "demo", "hobbies_list", "lang_list", "education")
  )
  expect_true(all(vapply(dm::dm_get_tables(res), nrow, integer(1)) == 0L))
  fks <- dm::dm_get_all_fks(res)
  parents <- setNames(fks$parent_table, fks$child_table)
  expect_equal(
    parents[c("demo", "hobbies_list", "lang_list", "education")],
    c(demo = "main", hobbies_list = "demo", lang_list = "demo", education = "main")
  )
  expect_true(all(c("name", "age", "_index", "_parent_index") %in% names(res$demo)))
})

test_that("kobo_data explains a 404 from the data endpoint of an existing project", {
  asset <- asset_ml
  asset["deployment__submission_count"] <- list(NULL)
  local_mocked_bindings(
    fetch_subs_pages_ = function(...) {
      abort("Not found.", class = "robotoolbox_http_404")
    },
    get_filtered_count_ = function(...) {
      abort("Not found.", class = "robotoolbox_http_404")
    }
  )
  expect_error(kobo_data(asset), "not available on the server")
  expect_error(kobo_data(asset, query = '{"_id": 1}'), "not available on the server")
})

test_that("kobo_form works on a form whose questions have no label", {
  asset <- asset_ml
  asset$uid <- "aFormWithoutLabels00000"
  asset$content$translations <- list(NULL)
  asset$content$survey <- lapply(asset$content$survey, function(q) {
    q[setdiff(names(q), "label")]
  })
  on.exit(kobo_cache_clear(asset$uid))
  form <- kobo_form(asset)
  expect_true(all(is.na(form$label)))
  expect_true("full_name" %in% form$name)
})

## Nested repeats

nested_form <- tibble::tibble(
  name = c("member", "visit", "treatment", "dose", "dose_mg", NA, NA, NA, NA,
           "item", "item_name", NA),
  type = c(rep("begin_repeat", 4), "integer", rep("end_repeat", 4),
           "begin_repeat", "text", "end_repeat"),
  lang = "Labels",
  version = "v1"
)

test_that("repeat tables are extracted at every depth", {
  form <- nested_form
  json <- '{"results": [
    {"_id": 1, "member": [
      {"member/visit": [
        {"member/visit/treatment": [
          {"member/visit/treatment/dose": [
            {"member/visit/treatment/dose/dose_mg": 500},
            {"member/visit/treatment/dose/dose_mg": 250}]},
          {"member/visit/treatment/dose": [
            {"member/visit/treatment/dose/dose_mg": 1000}]}]}]},
      {"member/visit": []}],
     "item": [{"item/item_name": "Radio"}]},
    {"_id": 2, "member": [
      {"member/visit": [
        {"member/visit/treatment": [
          {"member/visit/treatment/dose": [
            {"member/visit/treatment/dose/dose_mg": 50}]}]}]}]}]}'
  subs <- RcppSimdJson::fparse(json, max_simplify_lvl = "data_frame")$results
  subs <- rlang::set_names(subs, make_unique_names_)
  subs <- tibble::rowid_to_column(subs, "_index")
  res <- kobo_extract_repeat_tbl(subs, form)

  expect_equal(names(res), c("member", "visit", "treatment", "dose", "item"))
  parents <- vapply(res, function(x) unique(x$`_parent_table_name`), "")
  expect_equal(
    unname(parents),
    c("main", "member", "visit", "treatment", "main")
  )
  expect_equal(res$dose$dose_mg, c(500, 250, 1000, 50))
  expect_equal(res$dose$`_parent_index`, c(1L, 1L, 2L, 3L))
  expect_equal(res$treatment$`_parent_index`, c(1L, 1L, 2L))
  expect_equal(res$visit$`_parent_index`, c(1L, 3L))
  expect_equal(res$member$`_parent_index`, c(1L, 1L, 2L))
})

test_that("kobo_data keeps every level of nested repeats", {
  skip_on_cran()
  token <- Sys.getenv("KOBOTOOLBOX_KF_TOKEN")
  cassette <- file.path(vcr_dir, "kobo_data_nested.yml")
  skip_if(token == "" && !file.exists(cassette),
          message = "kf.kobotoolbox.org not configured")
  if (token == "") {
    token <- "0000000000000000000000000000000000000000"
  }
  old_url <- Sys.getenv("KOBOTOOLBOX_URL")
  old_token <- Sys.getenv("KOBOTOOLBOX_TOKEN")
  on.exit(Sys.setenv(KOBOTOOLBOX_URL = old_url, KOBOTOOLBOX_TOKEN = old_token))
  kobo_setup(url = "https://kf.kobotoolbox.org", token = token)
  uid <- "a9Vt3fe8939DyJZit8rRDt"
  vcr::use_cassette("kobo_data_nested", {
    res <- kobo_data(uid)
  })
  expect_s3_class(res, "dm")
  expect_equal(
    names(res),
    c("main", "member", "visit", "treatment", "dose", "item")
  )
  expect_true(all(dm::dm_examine_constraints(res)$problem == ""))
  dose <- res$dose
  expect_equal(as.vector(dose$dose_mg), c(500, 250, 1000, 500, 500, 250))
  expect_equal(as.vector(dose$`_parent_index`), c(1L, 1L, 2L, 4L, 4L, 4L))
  expect_equal(labelled::var_label(dose$dose_mg), "Dose in mg")
  expect_equal(
    labelled::val_labels(dose$dose_time),
    c(Morning = "am", Evening = "pm")
  )
  expect_equal(as.vector(res$treatment$`_parent_index`), c(1L, 1L, 2L, 3L))
  expect_equal(as.vector(res$visit$`_parent_index`), c(1L, 1L, 3L))
  expect_equal(as.vector(res$member$member_name), c("Awa", "Binta", "Coumba"))
})

## Repeat groups without data

test_that("repeat groups without data give empty tables with their keys", {
  json <- '{"results": [{"_id": 1, "member": []}, {"_id": 2}]}'
  subs <- RcppSimdJson::fparse(json, max_simplify_lvl = "data_frame")$results
  subs <- tibble::rowid_to_column(subs, "_index")
  res <- kobo_extract_repeat_tbl(subs, nested_form)
  expect_equal(names(res), c("member", "visit", "treatment", "dose", "item"))
  for (tbl in res) {
    expect_equal(nrow(tbl), 0L)
    expect_type(tbl$`_index`, "integer")
    expect_type(tbl$`_parent_index`, "integer")
    expect_type(tbl$`_parent_table_name`, "character")
  }

  res <- kobo_extract_repeat_tbl(subs, nested_form, fields = "member")
  expect_equal(names(res), c("member", "visit", "treatment", "dose"))
  res <- kobo_extract_repeat_tbl(subs, nested_form, fields = "_id")
  expect_length(res, 0L)
})

test_that("kobo_data returns every repeat table when no submission has repeat data", {
  rg_rows <- function(ids) {
    data.frame(
      `_id` = ids,
      `__version__` = rep(asset_rg$deployed_version_id, length(ids)),
      today = rep("2026-10-15", length(ids)),
      check.names = FALSE
    )
  }
  rg_rows_empty_list <- function(ids) {
    res <- rg_rows(ids)
    res$demo <- rep(list(list()), length(ids))
    res
  }
  for (rows in list(rg_rows, rg_rows_empty_list)) {
    server <- fake_server(total = 2, rows = rows)
    with_fake_server(server, {
      res <- kobo_data(asset_rg)
    }, version = asset_rg$deployed_version_id)
    expect_equal(
      names(res),
      c("main", "demo", "hobbies_list", "lang_list", "education")
    )
    expect_equal(nrow(res$main), 2L)
    expect_equal(nrow(res$demo), 0L)
    expect_true(all(c("name", "age") %in% names(res$demo)))
    fks <- dm::dm_get_all_fks(res)
    parents <- setNames(fks$parent_table, fks$child_table)
    expect_equal(
      parents[c("demo", "hobbies_list", "lang_list", "education")],
      c(demo = "main", hobbies_list = "demo", lang_list = "demo", education = "main")
    )
    expect_true(all(dm::dm_examine_constraints(res)$problem == ""))

    res_fr <- kobo_lang_set(res, asset_rg, "Francais (fr)")
    expect_equal(dm::dm_get_all_fks(res_fr), fks)
    expect_equal(dm::dm_get_all_pks(res_fr), dm::dm_get_all_pks(res))
  }
})

test_that("kobo_data with fields keeps the columns of empty repeat tables", {
  rows <- function(ids) {
    data.frame(
      `_id` = ids,
      `__version__` = rep(asset_rg$deployed_version_id, length(ids)),
      today = rep("2026-10-15", length(ids)),
      check.names = FALSE
    )
  }
  server <- fake_server(total = 2, rows = rows)
  with_fake_server(server, {
    res <- kobo_data(asset_rg, fields = c("today", "demo"))
  }, version = asset_rg$deployed_version_id)
  expect_equal(names(res), c("main", "demo", "hobbies_list", "lang_list"))
  expect_false("family_count" %in% names(res$main))
  expect_true(all(c("name", "age", "sex") %in% names(res$demo)))
  expect_true("hobbies" %in% names(res$hobbies_list))
})

test_that("kobo_data keeps empty repeat tables from the server", {
  skip_on_cran()
  token <- Sys.getenv("KOBOTOOLBOX_KF_TOKEN")
  cassette <- file.path(vcr_dir, "kobo_data_empty_repeat.yml")
  skip_if(token == "" && !file.exists(cassette),
          message = "kf.kobotoolbox.org not configured")
  if (token == "") {
    token <- "0000000000000000000000000000000000000000"
  }
  old_url <- Sys.getenv("KOBOTOOLBOX_URL")
  old_token <- Sys.getenv("KOBOTOOLBOX_TOKEN")
  on.exit(Sys.setenv(KOBOTOOLBOX_URL = old_url, KOBOTOOLBOX_TOKEN = old_token))
  kobo_setup(url = "https://kf.kobotoolbox.org", token = token)
  uid <- "aoaErT8NRETaeDDaNuJSVz"
  vcr::use_cassette("kobo_data_empty_repeat", {
    res_all <- kobo_data(uid)
    res <- kobo_data(uid, query = '{"hh_name": "A"}')
  })
  expect_equal(names(res_all), c("main", "roster", "phone"))
  expect_equal(as.vector(res_all$roster$person), c("Dia", "Eve"))
  expect_equal(as.vector(res_all$phone$number), "123")
  expect_equal(as.vector(res_all$phone$`_parent_index`), 2L)

  expect_equal(names(res), c("main", "roster", "phone"))
  expect_equal(as.vector(res$main$hh_name), "A")
  expect_equal(nrow(res$roster), 0L)
  expect_equal(nrow(res$phone), 0L)
  expect_true("person" %in% names(res$roster))
  expect_equal(dm::dm_get_all_fks(res), dm::dm_get_all_fks(res_all))
  expect_true(all(dm::dm_examine_constraints(res)$problem == ""))
})


## Fields of older form versions

two_version_form <- tibble::tibble(
  name = c("hh", "group", "new_age", "moved", NA,
           "hh", "moved", "group", "old_age", NA),
  type = c("text", "begin_repeat", "integer", "integer", "end_repeat",
           "text", "integer", "begin_repeat", "integer", "end_repeat"),
  label = c("Household", "Group", "New age", "Moved", NA,
            "Household", "Moved", "Group", "Old age", NA),
  lang = "Labels",
  version = rep(c("vNEW", "vOLD"), each = 5)
)

test_that("column scopes come from every form version", {
  res <- kobo_form_name_to_list_(two_version_form, "_")
  expect_equal(
    res,
    list(main = c("hh", "moved"), group = c("new_age", "moved", "old_age"))
  )
})

test_that("column scopes do not depend on the language of the rows", {
  res <- kobo_form_name_to_list_(kobo_form(asset_rg), "_")
  expect_equal(
    names(res),
    c("main", "demo", "hobbies_list", "lang_list", "education")
  )
  expect_true(all(c("family_count", "family_count_note") %in% res$main))
  expect_false("family_count_note" %in% res$lang_list)
  expect_true("name_individual2" %in% res$education)
})

test_that("kobo_data puts fields of older form versions in their repeat table", {
  asset <- structure(
    list(
      uid = "aOldVersionFields000000",
      asset_type = "survey",
      deployment__submission_count = 2L,
      version_id = "vNEW",
      deployed_version_id = "vNEW",
      content = list(translations = list(NULL))
    ),
    class = "kobo_asset"
  )
  on.exit(kobo_cache_clear(asset$uid))
  rows <- function(ids) {
    res <- data.frame(
      `_id` = ids,
      `__version__` = c("vNEW", "vOLD")[ids],
      hh = c("A", "B")[ids],
      moved = c(NA, 7L)[ids],
      check.names = FALSE
    )
    res$group <- list(
      data.frame(`group/new_age` = 1L, `group/moved` = 5L, check.names = FALSE),
      data.frame(`group/old_age` = 30L, check.names = FALSE)
    )[ids]
    res
  }
  server <- fake_server(total = 2, rows = rows)
  with_fake_server(server, {
    local_mocked_bindings(kobo_form_version_ = function(...) two_version_form)
    res <- kobo_data(asset)
  })
  expect_false("old_age" %in% names(res$main))
  expect_equal(names(res$group)[1:3], c("new_age", "moved", "old_age"))
  expect_equal(as.vector(res$group$old_age), c(NA, 30L))
  expect_equal(as.vector(res$main$moved), c(NA, 7L))
})

## select_multiple labels

sm_asset <- function() {
  ch <- function(name, label) list(list_name = "d", name = name, label = list(label))
  structure(
    list(
      uid = "aSelectMultipleLabels00",
      asset_type = "survey",
      deployment__submission_count = 5L,
      version_id = "v1",
      deployed_version_id = "v1",
      content = list(
        survey = list(list(
          type = "select_multiple", name = "dist", label = list("Distance"),
          select_from_list_name = "d"
        )),
        choices = list(
          ch("1", "Less than 2 km"), ch("2", "2 to 5 km"), ch("3", "More than 5 km"),
          ch("a+b", "Plus"), ch("I", "I have no need"), ch("E", "I lack documents"),
          ch("m", "Member ${member_1}")
        ),
        translations = list(NULL)
      )
    ),
    class = "kobo_asset"
  )
}

test_that("select_multiple codes are replaced by their labels exactly", {
  asset <- sm_asset()
  on.exit(kobo_cache_clear(asset$uid))
  answers <- c("1 3", "2", "a+b I", "E", "m 9", NA)
  rows <- function(ids) {
    data.frame(
      `_id` = ids,
      `__version__` = rep("v1", length(ids)),
      dist = answers[ids],
      check.names = FALSE
    )
  }
  server <- fake_server(total = length(answers), rows = rows)
  with_fake_server(server, {
    local_mocked_bindings(kobo_form_version_ = function(...) kobo_form(asset))
    res <- kobo_data(asset, select_multiple_label = TRUE)
  }, version = "v1")
  expect_equal(
    as.vector(res$dist),
    c("Less than 2 km More than 5 km", "2 to 5 km", "Plus I have no need",
      "I lack documents", "Member ${member_1} 9", NA)
  )
  expect_equal(labelled::var_label(res$dist), "Distance")
  expect_equal(as.vector(res$dist_1), c(1L, 0L, 0L, 0L, 0L, NA))
})

test_that("select_multiple dummy columns keep their labels with any separator", {
  asset <- sm_asset()
  on.exit(kobo_cache_clear(asset$uid))
  rows <- function(ids) {
    data.frame(
      `_id` = ids,
      `__version__` = rep("v1", length(ids)),
      dist = c("1 3", "2")[ids],
      check.names = FALSE
    )
  }
  for (sep in c("@", ".", "__")) {
    server <- fake_server(total = 2, rows = rows)
    with_fake_server(server, {
      local_mocked_bindings(kobo_form_version_ = function(...) kobo_form(asset))
      res <- kobo_data(asset, select_multiple_sep = sep)
      res_label <- kobo_data(asset, select_multiple_sep = sep, colnames_label = TRUE)
    }, version = "v1")
    dummy <- paste0("dist", sep, "1")
    expect_equal(labelled::var_label(res[[dummy]]), "Distance::Less than 2 km")
    expect_true("Distance::Less than 2 km" %in% names(res_label))
    expect_equal(as.vector(res_label[["Distance::More than 5 km"]]), c(1L, 0L))

    res <- kobo_lang_set(res, asset, "Labels")
    expect_equal(labelled::var_label(res[[dummy]]), "Distance::Less than 2 km")
  }
})

test_that("kobo_lang_set keeps select_multiple labels of a dm with any separator", {
  rg_rows <- function(ids) {
    data.frame(
      `_id` = ids,
      `__version__` = rep(asset_rg$deployed_version_id, length(ids)),
      check.names = FALSE
    )
  }
  server <- fake_server(total = 1, rows = rg_rows)
  with_fake_server(server, {
    res <- kobo_data(asset_rg, select_multiple_sep = "@")
  }, version = asset_rg$deployed_version_id)
  dummy <- grep("^langs@", names(res$lang_list), value = TRUE)[1]
  expect_match(labelled::var_label(res$lang_list[[dummy]]), "::")
  res_fr <- kobo_lang_set(res, asset_rg, "Francais (fr)")
  fr <- labelled::var_label(res_fr$lang_list[[dummy]])
  expect_match(fr, "::")
  expect_false(identical(fr, labelled::var_label(res$lang_list[[dummy]])))
})

## all_versions = FALSE reads every submission with the deployed form

versions_q <- function(type, name, label = NULL) {
  res <- list(type = type, name = name)
  if (!is.null(label)) res$label <- list(label)
  res
}

versions_asset <- function() {
  structure(
    list(
      uid = "aAllVersionsFalse000000",
      asset_type = "survey",
      deployment__submission_count = 2L,
      version_id = "vNEW",
      version__content_hash = "hNEW",
      deployed_version_id = "vNEW",
      deployed_versions = list(count = 2L, results = list(
        list(uid = "vNEW", content_hash = "hNEW"),
        list(uid = "vOLD", content_hash = "hOLD")
      )),
      content = list(
        survey = list(
          versions_q("text", "hh", "Household (new)"),
          versions_q("text", "new_q", "New question"),
          versions_q("begin_group", "g2", "Group 2"),
          versions_q("text", "sex", "Sex"),
          list(type = "end_group")
        ),
        translations = list(NULL)
      )
    ),
    class = "kobo_asset"
  )
}

old_version <- structure(
  list(content = list(
    survey = list(
      versions_q("text", "hh", "Household (old)"),
      versions_q("text", "old_q", "Old question"),
      versions_q("begin_group", "g1", "Group 1"),
      versions_q("text", "sex", "Sex"),
      list(type = "end_group")
    ),
    translations = list(NULL)
  )),
  class = "kobo_asset_version"
)

test_that("the deployed form labels the submissions of every version", {
  asset <- versions_asset()
  on.exit(kobo_cache_clear(asset$uid))
  local_mocked_bindings(kobo_asset_version = function(x, version) old_version)
  rows <- function(ids) {
    data.frame(`_id` = ids, `__version__` = "vOLD", hh = c("A", "B")[ids],
               old_q = c("x", "y")[ids], `g1/sex` = c("1", "2")[ids],
               check.names = FALSE)
  }
  with_fake_server(fake_server(total = 2, rows = rows), {
    latest <- kobo_data(asset, all_versions = FALSE)
    all <- kobo_data(asset, all_versions = TRUE)
  }, version = c("vNEW", "vOLD"))
  expect_equal(as.vector(latest$`_id`), 1:2)
  expect_equal(labelled::var_label(latest$hh), "Household (new)")
  expect_equal(labelled::var_label(latest$old_q), "old_q")
  expect_equal(as.vector(latest$old_q), c("x", "y"))
  expect_equal(labelled::var_label(all$hh), "Household (new)")
  expect_equal(labelled::var_label(all$old_q), "Old question")
})

test_that("labels corrected in a deployment without submissions are used", {
  ch <- function(name, label) list(list_name = "agree", name = name, label = list(label))
  opinion <- c(versions_q("select_one", "opinion", "Opinion"),
               list(select_from_list_name = "agree"))
  asset <- versions_asset()
  asset$content$survey <- list(versions_q("text", "hh", "Household"), opinion)
  asset$content$choices <- list(ch("a", "Estoy de acuerdo"), ch("d", "Estoy en desacuerdo"))
  old <- old_version
  old$content$survey <- asset$content$survey
  old$content$choices <- list(ch("a", "Acuerdo"), ch("d", "Desacuerdo"))
  on.exit(kobo_cache_clear(asset$uid))
  local_mocked_bindings(kobo_asset_version = function(x, version) old)
  rows <- function(ids) {
    data.frame(`_id` = ids, `__version__` = "vOLD", hh = c("A", "B")[ids],
               opinion = c("a", "d")[ids], check.names = FALSE)
  }
  for (av in c(TRUE, FALSE)) {
    with_fake_server(fake_server(total = 2, rows = rows), {
      res <- kobo_data(asset, all_versions = av)
    }, version = c("vNEW", "vOLD"))
    expect_equal(as.vector(labelled::to_character(res$opinion)),
                 c("Estoy de acuerdo", "Estoy en desacuerdo"), info = paste("all_versions =", av))
  }
})

test_that("all_versions = FALSE keeps the values of a question moved between versions", {
  asset <- versions_asset()
  on.exit(kobo_cache_clear(asset$uid))
  local_mocked_bindings(kobo_asset_version = function(x, version) old_version)
  rows <- function(ids) {
    data.frame(`_id` = ids, `__version__` = c("vOLD", "vNEW")[ids],
               hh = c("A", "B")[ids], `g1/sex` = c("1", NA)[ids],
               `g2/sex` = c(NA, "2")[ids], check.names = FALSE)
  }
  with_fake_server(fake_server(total = 2, rows = rows), {
    latest <- kobo_data(asset, all_versions = FALSE)
    all <- kobo_data(asset, all_versions = TRUE)
  }, version = c("vNEW", "vOLD"))
  expect_equal(as.vector(latest$sex), c("1", "2"))
  expect_equal(as.vector(all$sex), c("1", "2"))
})

test_that("a repeat group keeps its rows under the parent of each version", {
  rep_open <- function(name) versions_q("begin_repeat", name, name)
  rep_close <- list(type = "end_repeat")
  asset <- versions_asset()
  asset$uid <- "aRepeatParentMoved00000"
  asset$content$survey <- list(
    versions_q("text", "hh", "Household"),
    rep_open("parent"), rep_open("child"), versions_q("text", "value", "Value"),
    rep_close, rep_close,
    rep_open("p_new"), versions_q("text", "who", "Who"),
    rep_open("tools"), versions_q("text", "tool", "Tool"), rep_close, rep_close
  )
  old <- old_version
  old$content$survey <- list(
    versions_q("text", "hh", "Household"),
    rep_open("child"), versions_q("text", "value", "Value"), rep_close,
    rep_open("p_old"), versions_q("text", "who", "Who"),
    rep_open("tools"), versions_q("text", "tool", "Tool"), rep_close, rep_close
  )
  on.exit(kobo_cache_clear(asset$uid))
  local_mocked_bindings(kobo_asset_version = function(x, version) old)
  tbl <- function(...) tibble::tibble(...)
  subs <- tbl(
    `_id` = 1:2, `__version__` = c("vNEW", "vOLD"), hh = c("A", "B"),
    parent = list(tbl(`parent/child` = list(tbl(`parent/child/value` = "new answer"))),
                  NULL),
    child = list(NULL, tbl(`child/value` = "old answer")),
    p_new = list(tbl(`p_new/who` = "Ann", `p_new/tools` = list(
      tbl(`p_new/tools/tool` = c("hammer", "saw")))), NULL),
    p_old = list(NULL, tbl(`p_old/who` = "Bob", `p_old/tools` = list(
      tbl(`p_old/tools/tool` = "drill"))))
  )
  rows <- function(ids) subs[ids, ]
  with_fake_server(fake_server(total = 2, rows = rows), {
    res <- kobo_data(asset)
  }, version = c("vNEW", "vOLD"))

  child <- res$child
  expect_equal(as.vector(child$value), c("new answer", "old answer"))
  expect_equal(as.vector(child$`_parent_table_name`), c("parent", "main"))
  expect_equal(as.vector(child$`_parent_index`), c(1L, NA))
  expect_equal(as.vector(child$`_parent_index_main`), c(NA, 2L))

  tools <- res$tools
  expect_equal(as.vector(tools$tool), c("hammer", "saw", "drill"))
  expect_equal(as.vector(tools$`_parent_table_name`), c("p_new", "p_new", "p_old"))
  expect_equal(as.vector(tools$`_parent_index`), c(1L, 1L, NA))
  expect_equal(as.vector(tools$`_parent_index_p_old`), c(NA, NA, 1L))
  expect_equal(as.vector(res$p_old$who), "Bob")

  fks <- dm::dm_get_all_fks(res)
  expect_setequal(paste(fks$child_table, unlist(fks$child_fk_cols), fks$parent_table),
                  c("parent _parent_index main", "child _parent_index parent",
                    "child _parent_index_main main", "p_new _parent_index main",
                    "tools _parent_index p_new", "tools _parent_index_p_old p_old",
                    "p_old _parent_index main"))
  expect_true(all(dm::dm_examine_constraints(res)$is_key))
})

test_that("repeat groups nested in each other in different versions are an error", {
  rep_open <- function(name) versions_q("begin_repeat", name, name)
  rep_close <- list(type = "end_repeat")
  nest <- function(outer, inner) {
    list(versions_q("text", "hh", "Household"), rep_open(outer), rep_open(inner),
         versions_q("text", "value", "Value"), rep_close, rep_close)
  }
  asset <- versions_asset()
  asset$uid <- "aRepeatNestingCycle0000"
  asset$content$survey <- nest("a", "b")
  old <- old_version
  old$content$survey <- nest("b", "a")
  on.exit(kobo_cache_clear(asset$uid))
  local_mocked_bindings(kobo_asset_version = function(x, version) old)
  rows <- function(ids) {
    data.frame(`_id` = ids, `__version__` = c("vNEW", "vOLD")[ids],
               hh = c("A", "B")[ids], check.names = FALSE)
  }
  with_fake_server(fake_server(total = 2, rows = rows), {
    expect_error(kobo_data(asset), "`a`, `b` are nested in each other")
    expect_s3_class(kobo_data(asset, all_versions = FALSE), "dm")
  }, version = c("vNEW", "vOLD"))
})

test_that("a choice removed from the deployed form keeps its dummy values", {
  ch <- function(name, label) list(list_name = "c", name = name, label = list(label))
  colors <- c(versions_q("select_multiple", "colors", "Colors"),
              list(select_from_list_name = "c"))
  asset <- versions_asset()
  asset$uid <- "aRemovedChoice000000000"
  size <- function(type) c(versions_q(type, "size", "Size"), list(select_from_list_name = "s"))
  sz <- function(name) list(list_name = "s", name = name, label = list(name))
  asset$content$survey <- list(versions_q("text", "hh", "Household"), colors,
                               size("select_multiple"))
  asset$content$choices <- list(ch("r", "Red"), sz("small"))
  old <- old_version
  old$content$survey <- list(versions_q("text", "hh", "Household"), colors,
                             size("select_one"))
  old$content$choices <- list(ch("r", "Red"), ch("b", "Blue"), sz("small"), sz("big"))
  on.exit(kobo_cache_clear(asset$uid))
  local_mocked_bindings(kobo_asset_version = function(x, version) old)
  cases <- list(old_only = c("vOLD", "vOLD"), mixed = c("vOLD", "vNEW"))
  for (case in names(cases)) {
    kobo_cache_clear(asset$uid)
    rows <- function(ids) {
      data.frame(`_id` = ids, `__version__` = cases[[case]][ids],
                 hh = c("A", "B")[ids], colors = c("b", "r")[ids],
                 size = c("big", "small")[ids], check.names = FALSE)
    }
    with_fake_server(fake_server(total = 2, rows = rows), {
      res <- kobo_data(asset)
    }, version = c("vNEW", "vOLD"))
    expect_equal(as.vector(res$colors_b), c(1L, 0L), info = case)
    expect_equal(as.vector(res$colors_r), c(0L, 1L), info = case)
    expect_equal(as.vector(res$size_small), c(0L, 1L), info = case)
    expect_false("size_big" %in% names(res), info = case)
  }
})

test_that("a geopoint moved from the top level into a group keeps its answers", {
  asset <- versions_asset()
  asset$content$survey <- list(
    versions_q("text", "hh", "Household (new)"),
    versions_q("begin_group", "grupo", "Grupo"),
    versions_q("geopoint", "ponto", "Ponto"),
    list(type = "end_group")
  )
  old <- old_version
  old$content$survey <- list(
    versions_q("geopoint", "ponto", "Ponto"),
    versions_q("text", "hh", "Household (old)")
  )
  on.exit(kobo_cache_clear(asset$uid))
  local_mocked_bindings(kobo_asset_version = function(x, version) old)
  rows <- function(ids) {
    data.frame(`_id` = ids, `__version__` = c("vOLD", "vNEW")[ids],
               hh = c("A", "B")[ids], ponto = c("1 2 0 5", NA)[ids],
               `grupo/ponto` = c(NA, "5 6 0 5")[ids], check.names = FALSE)
  }
  for (av in c(TRUE, FALSE)) {
    with_fake_server(fake_server(total = 2, rows = rows), {
      res <- kobo_data(asset, all_versions = av)
    }, version = c("vNEW", "vOLD"))
    expect_equal(as.vector(res$ponto), c("1 2 0 5", "5 6 0 5"), info = paste("all_versions =", av))
    expect_equal(as.vector(res$ponto_latitude), c(1, 5), info = paste("all_versions =", av))
  }
})

## Spatial columns

test_that("geopoints keep their layout and handle missing or short values", {
  form <- tibble::tibble(name = "pt", type = "geopoint")
  x <- tibble::tibble(
    a = 1:7,
    pt = c("14.71 -17.45 10.5 5", "14.71 -17.45", "", NA, "  1  2  3  4 ",
           "1 2 3.5e-4 0", "not a point"),
    z = 1:7
  )
  labelled::var_label(x$pt) <- "Point"
  res <- extract_geopoint_(x, form)
  expect_named(res, c("a", "pt_latitude", "pt_longitude", "pt_altitude",
                      "pt_precision", "pt", "z", "pt_wkt"))
  expect_identical(
    as.vector(res$pt_wkt),
    c("POINT (-17.45 14.71 10.5)", "POINT (-17.45 14.71)", NA, NA,
      "POINT (2 1 3)", "POINT (2 1 3.5e-4)", NA)
  )
  expect_identical(as.vector(res$pt_latitude), c("14.71", "14.71", NA, NA, "1", "1", NA))
  expect_identical(as.vector(res$pt_precision), c("5", NA, NA, NA, "4", "0", NA))
  expect_identical(res$pt, x$pt)
  expect_no_warning(typed <- type_convert_(res, form))
  expect_equal(as.vector(typed$pt_latitude), c(14.71, 14.71, NA, NA, 1, 1, NA))
})

test_that("geotraces and geoshapes give NA for missing or degenerate values", {
  form <- tibble::tibble(name = c("tr", "sh"), type = c("geotrace", "geoshape"))
  x <- tibble::tibble(
    tr = c(NA, "", "1 2 0 0;3 4 0 0;", "1 2;3 4", "1 2 0 0; ;3 4 0 0",
           "1 2 0 0;bad", "1 2 0 0"),
    sh = c(NA, "", "1 2 0 0;3 4 0 0;5 6 0 0;1 2 0 0;", "1 2;3 4;5 6;1 2",
           "1 2 0 0;3 4 0 0;5 6 0 0;1 2 0 0", "x", "1 2 0 0;3 4 0 0;1 2 0 0")
  )
  tr <- extract_geotrace_(x, form)$tr_wkt
  sh <- extract_geoshape_(x, form)$sh_wkt
  expect_identical(as.vector(tr), c(
    NA, NA, "LINESTRING (2 1 0, 4 3 0)", "LINESTRING (2 1, 4 3)",
    "LINESTRING (2 1 0, 4 3 0)", NA, NA
  ))
  expect_identical(as.vector(sh), c(
    NA, NA, "POLYGON ((2 1 0, 4 3 0, 6 5 0, 2 1 0))",
    "POLYGON ((2 1, 4 3, 6 5, 2 1))",
    "POLYGON ((2 1 0, 4 3 0, 6 5 0, 2 1 0))", NA, NA
  ))
})

test_that("spatial columns keep their type when every value is missing", {
  form <- tibble::tibble(
    name = c("pt", "tr", "sh"),
    type = c("geopoint", "geotrace", "geoshape")
  )
  x <- tibble::tibble(pt = NA_character_, tr = NA_character_, sh = NA_character_)
  x <- extract_geoshape_(extract_geotrace_(extract_geopoint_(x, form), form), form)
  res <- type_convert_(x, form)
  expect_type(res$pt_wkt, "character")
  expect_type(res$tr_wkt, "character")
  expect_type(res$sh_wkt, "character")
  expect_type(res$pt_latitude, "double")
  expect_type(res$pt_precision, "double")
})

test_that("select_multiple labels keep the deployed label when versions differ", {
  ch <- function(lbl) {
    tibble::tibble(value_name = c("red", "red_1"), value_label = lbl, value_lang = "English")
  }
  form <- tibble::tibble(
    name = "colors",
    type = "select_multiple colors",
    label = "Colors",
    lang = "English",
    version = c("vNEW", "vOLD"),
    choices = list(ch(c("New red", "Dark red")), ch(c("Old red", "Dark red")))
  )
  labels <- select_multiple_var_label(NULL, form, "English")
  expect_identical(names(labels), c("colors_red", "colors_red_1"))
  expect_identical(labels$colors_red, "Colors::New red")
  expect_identical(labels$colors_red_1, "Colors::Dark red")
})

test_that("superseded version labels do not add a suffix to other choices", {
  ch <- function(nm, lbl) {
    tibble::tibble(value_name = nm, value_label = lbl, value_lang = "English")
  }
  choices <- list(
    ch(c("1", "2"), c("Low", "High")),
    ch("1", "Medium"),
    ch("3", "Medium")
  )
  so <- tibble::tibble(
    name = "level",
    type = "select_one level",
    label = "Level",
    lang = "English",
    version = c("v3", "v2", "v1"),
    choices = choices
  )
  expect_identical(
    precompute_val_labels_(so, "English")$level,
    c(Low = "1", High = "2", Medium = "3")
  )
  sm <- so
  sm$type <- "select_multiple level"
  expect_identical(
    sm_lookup_(sm, "level", "English"),
    c(`1` = "Low", `2` = "High", `3` = "Medium")
  )
})

## A question named like a select_multiple dummy column

collide_asset <- function(q = versions_q("text", "pet_other", "Other pet"),
                          q_first = FALSE, in_repeat = FALSE) {
  ch <- function(list_name, name, label) {
    list(list_name = list_name, name = name, label = list(label))
  }
  sm <- list(type = "select_multiple", name = "pet", label = list("Pet"),
             select_from_list_name = "p")
  qs <- if (q_first) list(q, sm) else list(sm, q)
  if (in_repeat) {
    qs <- c(list(versions_q("begin_repeat", "member", "Member")), qs,
            list(list(type = "end_repeat")))
  }
  structure(
    list(
      uid = "aDummyNameCollision0000",
      asset_type = "survey",
      deployment__submission_count = 2L,
      version_id = "v1",
      deployed_version_id = "v1",
      content = list(
        survey = c(list(versions_q("text", "hh", "Household")), qs,
                   list(versions_q("text", "last", "Last"))),
        choices = list(
          ch("p", "cat", "Cat"), ch("p", "dog", "Dog"), ch("p", "other", "Other"),
          ch("k", "a", "Kind A"), ch("k", "b", "Kind B")
        ),
        translations = list(NULL)
      )
    ),
    class = "kobo_asset"
  )
}

collide_rows <- function(other, notes = NULL) {
  function(ids) {
    d <- data.frame(`_id` = ids, `__version__` = "v1", hh = c("A", "B")[ids],
                    pet = c("cat other", "dog")[ids], pet_other = other[ids],
                    last = c("x", "y")[ids], check.names = FALSE)
    if (!is.null(notes)) d$pet_notes <- notes[ids]
    d
  }
}

collide_data <- function(asset, rows, ...) {
  withr::defer(kobo_cache_clear(asset$uid))
  res <- NULL
  with_fake_server(fake_server(total = 2, rows = rows), {
    local_mocked_bindings(kobo_form_version_ = function(...) kobo_form(asset))
    expect_no_warning(
      expect_message(res <- kobo_data(asset, ...), "pet_other_1")
    )
  }, version = "v1")
  res
}

## Names, values and labels of the form columns, written out by hand in the
## tests below
form_cols <- function(x) {
  x <- x[!grepl("^_|^meta/|^formhub/", names(x))]
  list(
    names = names(x),
    values = lapply(unname(as.list(x)), as.vector),
    labels = vapply(x, function(col) labelled::var_label(col) %||% NA_character_,
                    character(1), USE.NAMES = FALSE)
  )
}

test_that("a question named like a dummy column keeps its answers", {
  for (other in list(c("hamster", "parrot"), c("007", "12"))) {
    res <- collide_data(collide_asset(), collide_rows(other))
    expect_identical(form_cols(res), list(
      names = c("hh", "pet", "pet_cat", "pet_dog", "pet_other", "pet_other_1", "last"),
      values = list(c("A", "B"), c("cat other", "dog"), c(1L, 0L), c(0L, 1L),
                    c(1L, 0L), other, c("x", "y")),
      labels = c("Household", "Pet", "Pet::Cat", "Pet::Dog", "Pet::Other",
                 "Other pet", "Last")
    ))
  }
})

test_that("a question renamed by a dummy column keeps its place in the form", {
  res <- collide_data(collide_asset(q_first = TRUE),
                      collide_rows(c("hamster", "parrot")))
  expect_identical(form_cols(res), list(
    names = c("hh", "pet_other_1", "pet", "pet_cat", "pet_dog", "pet_other", "last"),
    values = list(c("A", "B"), c("hamster", "parrot"), c("cat other", "dog"),
                  c(1L, 0L), c(0L, 1L), c(1L, 0L), c("x", "y")),
    labels = c("Household", "Other pet", "Pet", "Pet::Cat", "Pet::Dog",
               "Pet::Other", "Last")
  ))
})

test_that("a column absent from the form is not typed as a dummy column", {
  res <- collide_data(collide_asset(),
                      collide_rows(c("hamster", "parrot"), notes = c("note a", "007")))
  expect_identical(as.vector(res$pet_notes), c("note a", "007"))
  expect_identical(labelled::var_label(res$pet_notes), "pet_notes")
  expect_identical(
    form_cols(res)$names,
    c("hh", "pet", "pet_cat", "pet_dog", "pet_other", "pet_other_1", "last", "pet_notes")
  )
})

test_that("a select_one or integer question renamed by a dummy column keeps its type", {
  so <- list(type = "select_one", name = "pet_other", label = list("Other kind"),
             select_from_list_name = "k")
  res <- collide_data(collide_asset(so), collide_rows(c("a", "b")))
  expect_identical(res$pet_other, structure(c(1L, 0L), label = "Pet::Other"))
  expect_identical(as.vector(res$pet_other_1), c("a", "b"))
  expect_identical(labelled::val_labels(res$pet_other_1), c(`Kind A` = "a", `Kind B` = "b"))
  expect_identical(labelled::var_label(res$pet_other_1), "Other kind")

  int <- versions_q("integer", "pet_other", "Other count")
  res <- collide_data(collide_asset(int), collide_rows(c("3", "4")))
  expect_identical(as.vector(res$pet_other), c(1L, 0L))
  expect_identical(labelled::var_label(res$pet_other), "Pet::Other")
  expect_identical(as.vector(res$pet_other_1), 3:4)
  expect_identical(labelled::var_label(res$pet_other_1), "Other count")
})

test_that("a renamed question keeps its label with every kobo_data option", {
  rows <- collide_rows(c("hamster", "parrot"))
  res <- collide_data(collide_asset(), rows, colnames_label = TRUE)
  expect_identical(
    form_cols(res)$names,
    c("Household", "Pet", "Pet::Cat", "Pet::Dog", "Pet::Other", "Other pet", "Last")
  )
  expect_identical(as.vector(res$`Other pet`), c("hamster", "parrot"))
  expect_identical(as.vector(res$`Pet::Other`), c(1L, 0L))

  res <- collide_data(collide_asset(), rows, select_multiple_label = TRUE)
  expect_identical(as.vector(res$pet), c("Cat Other", "Dog"))
  expect_identical(as.vector(res$pet_other_1), c("hamster", "parrot"))
  expect_identical(labelled::var_label(res$pet_other_1), "Other pet")

  ## No collision with another separator
  asset <- collide_asset()
  withr::defer(kobo_cache_clear(asset$uid))
  with_fake_server(fake_server(total = 2, rows = rows), {
    local_mocked_bindings(kobo_form_version_ = function(...) kobo_form(asset))
    expect_no_message(res <- kobo_data(asset, select_multiple_sep = "/"))
  }, version = "v1")
  expect_identical(
    form_cols(res)$names,
    c("hh", "pet", "pet/cat", "pet/dog", "pet/other", "pet_other", "last")
  )
  expect_identical(as.vector(res$pet_other), c("hamster", "parrot"))
  expect_identical(labelled::var_label(res$pet_other), "Other pet")
})

test_that("kobo_lang_set keeps the labels of a renamed question", {
  so <- list(type = "select_one", name = "pet_other", label = list("Other kind"),
             select_from_list_name = "k")
  asset <- collide_asset(so)
  for (colnames_label in c(FALSE, TRUE)) {
    res <- collide_data(asset, collide_rows(c("a", "b")), colnames_label = colnames_label)
    local_mocked_bindings(kobo_form = function(x, ...) kobo_form.kobo_asset(asset))
    out <- kobo_lang_set(res, asset, kobo_lang(asset)[1])
    dummy <- if (colnames_label) "Pet::Other" else "pet_other"
    question <- if (colnames_label) "Other kind" else "pet_other_1"
    expect_identical(names(out), names(res))
    expect_identical(as.vector(out[[dummy]]), c(1L, 0L))
    expect_identical(labelled::var_label(out[[dummy]]), "Pet::Other")
    expect_null(labelled::val_labels(out[[dummy]]))
    expect_identical(as.vector(out[[question]]), c("a", "b"))
    expect_identical(labelled::var_label(out[[question]]), "Other kind")
    expect_identical(labelled::val_labels(out[[question]]),
                     c(`Kind A` = "a", `Kind B` = "b"))
  }
})

test_that("a question named like a dummy column keeps its answers in a repeat group", {
  rows <- function(ids) {
    d <- data.frame(`_id` = ids, `__version__` = "v1", hh = c("A", "B")[ids],
                    last = c("x", "y")[ids], check.names = FALSE)
    member <- list(
      data.frame(`member/pet` = c("cat other", "dog"),
                 `member/pet_other` = c("hamster", "007"), check.names = FALSE),
      data.frame(`member/pet` = "other", `member/pet_other` = "parrot",
                 check.names = FALSE)
    )
    d$member <- member[ids]
    d
  }
  res <- collide_data(collide_asset(in_repeat = TRUE), rows)
  member <- form_cols(res$member)
  expect_identical(member, list(
    names = c("pet", "pet_cat", "pet_dog", "pet_other", "pet_other_1"),
    values = list(c("cat other", "dog", "other"), c(1L, 0L, 0L), c(0L, 1L, 0L),
                  c(1L, 0L, 1L), c("hamster", "007", "parrot")),
    labels = c("Pet", "Pet::Cat", "Pet::Dog", "Pet::Other", "Other pet")
  ))
  expect_identical(form_cols(res$main)$names, c("hh", "last"))
})

test_that("a dummy column never takes the label of a question of another table", {
  ## `pet` is in the main table, the text question `pet_other` in a repeat
  asset <- collide_asset(in_repeat = TRUE)
  asset$content$survey <- asset$content$survey[c(1, 3, 2, 4, 5, 6)]
  withr::defer(kobo_cache_clear(asset$uid))
  rows <- function(ids) {
    d <- data.frame(`_id` = ids, `__version__` = "v1", hh = c("A", "B")[ids],
                    pet = c("cat other", "dog")[ids], last = c("x", "y")[ids],
                    check.names = FALSE)
    d$member <- list(
      data.frame(`member/pet_other` = c("hamster", "007"), check.names = FALSE),
      data.frame(`member/pet_other` = "parrot", check.names = FALSE)
    )[ids]
    d
  }
  with_fake_server(fake_server(total = 2, rows = rows), {
    local_mocked_bindings(kobo_form_version_ = function(...) kobo_form(asset))
    expect_no_message(res <- kobo_data(asset))
  }, version = "v1")
  expect_identical(res$main$pet_other, structure(c(1L, 0L), label = "Pet::Other"))
  expect_identical(
    res$member$pet_other,
    structure(c("hamster", "007", "parrot"), label = "Other pet")
  )
})

test_that("a renamed column never overwrites a column of the data", {
  form <- tibble::tibble(
    name = "pet", type = "select_multiple p", lang = "Labels",
    choices = list(tibble::tibble(value_name = c("cat", "other")))
  )
  x <- tibble::tibble(pet = c("cat other", "cat"), pet_other = c("u", "v"),
                      pet_other_1 = c("kept", "too"))
  expect_message(res <- fast_dummy_cols(x, form, "pet", "_"), "pet_other_2")
  expect_identical(attr(res, "kobo_displaced"), c(pet_other_2 = "pet_other"))
  attr(res, "kobo_displaced") <- NULL
  expect_identical(res, tibble::tibble(
    pet = c("cat other", "cat"), pet_other_1 = c("kept", "too"),
    pet_other_2 = c("u", "v"), pet_cat = c(1L, 1L), pet_other = c(1L, 0L)
  ))
})

## Checks of kobo_attachment_download() made before any request

test_that("kobo_attachment_download checks the uid and the folder before any request", {
  local_mocked_bindings(get_attachment_url_ = function(uid) stop("request made"))
  missing <- file.path(tempdir(), "no-such-folder-robotoolbox")
  expect_false(dir.exists(missing))
  expect_error(kobo_attachment_download("bad_uid", tempdir()), "Invalid asset uid")
  expect_error(
    kobo_attachment_download("aSCXrTdtrxHDnx3n4DHefK", missing),
    "folder does not exist"
  )
  expect_error(kobo_attachment_download(asset_ml, missing), "folder does not exist")
  expect_error(kobo_attachment_download(asset_ml, NA_character_), "`folder` must be")
  ## With valid arguments the listing is requested
  expect_error(kobo_attachment_download(asset_ml, tempdir()), "request made")
  expect_error(
    kobo_attachment_download("aSCXrTdtrxHDnx3n4DHefK", tempdir()),
    "request made"
  )
})

## A form without choice list

test_that("a form whose only choices come from a file gives its dummy columns", {
  asset <- structure(
    list(
      uid = "aNoChoicesColumn0000000",
      asset_type = "survey",
      deployment__submission_count = 2L,
      version_id = "v1",
      deployed_version_id = "v1",
      content = list(
        survey = list(
          versions_q("text", "hh", "Household"),
          list(type = "select_multiple_from_file", name = "sm",
               label = list("Items"), file = "items.csv")
        ),
        translations = list(NULL)
      )
    ),
    class = "kobo_asset"
  )
  withr::defer(kobo_cache_clear(asset$uid))
  form <- kobo_form(asset)
  expect_false("choices" %in% names(form))
  rows <- function(ids) {
    data.frame(`_id` = ids, `__version__` = "v1", hh = c("A", "B")[ids],
               sm = c("a b", "b")[ids], check.names = FALSE)
  }
  for (select_multiple_label in c(FALSE, TRUE)) {
    with_fake_server(fake_server(total = 2, rows = rows), {
      local_mocked_bindings(kobo_form_version_ = function(...) form)
      expect_no_warning(
        res <- kobo_data(asset, select_multiple_label = select_multiple_label)
      )
    }, version = "v1")
    expect_identical(form_cols(res), list(
      names = c("hh", "sm", "sm_a", "sm_b"),
      values = list(c("A", "B"), c("a b", "b"), c(1L, 0L), c(1L, 1L)),
      labels = c("Household", "Items", "sm_a", "sm_b")
    ))
  }
  local_mocked_bindings(kobo_form = function(x, ...) form)
  expect_no_warning(out <- kobo_lang_set(res, asset, kobo_lang(asset)[1]))
  expect_identical(form_cols(out), form_cols(res))
  expect_no_warning(empty <- kobo_data_empty_(asset, NULL, FALSE, "_", NULL))
  expect_identical(names(empty), c("hh", "sm"))
  expect_identical(nrow(empty), 0L)
})

## Labels of the columns derived from the spatial questions

geo_asset <- function() {
  q <- function(type, name, en = NULL, fr = NULL) {
    res <- list(type = type, name = name)
    if (!is.null(en)) res$label <- list(en, fr)
    res
  }
  structure(
    list(
      uid = "aSpatialLabels000000000",
      asset_type = "survey",
      deployment__submission_count = 1L,
      version_id = "v1",
      deployed_version_id = "v1",
      content = list(
        survey = list(
          q("geopoint", "pt", "Record a location", "Position"),
          q("geotrace", "line", "Path", "Chemin"),
          q("geoshape", "area", "Plot", "Parcelle"),
          q("geopoint", "bare")
        ),
        translations = list("English (en)", "Francais (fr)")
      )
    ),
    class = "kobo_asset"
  )
}

test_that("the columns derived from spatial questions are labeled after their question", {
  asset <- geo_asset()
  withr::defer(kobo_cache_clear(asset$uid))
  rows <- function(ids) {
    data.frame(`_id` = ids, `__version__` = "v1", pt = "1 2 3 4",
               line = "1 2 0 0;3 4 0 0", area = "0 0 0 0;0 1 0 0;1 1 0 0;0 0 0 0",
               bare = "5 6 0 0", check.names = FALSE)
  }
  derived <- c("pt_latitude", "pt_longitude", "pt_altitude", "pt_precision", "pt_wkt",
               "line_wkt", "area_wkt", "bare_latitude", "bare_wkt")
  en <- c("Record a location::latitude", "Record a location::longitude",
          "Record a location::altitude", "Record a location::precision",
          "Record a location::wkt", "Path::wkt", "Plot::wkt", "bare::latitude",
          "bare::wkt")
  fr <- c("Position::latitude", "Position::longitude", "Position::altitude",
          "Position::precision", "Position::wkt", "Chemin::wkt", "Parcelle::wkt",
          "bare::latitude", "bare::wkt")
  labels_of <- function(x, cols) {
    vapply(cols, function(n) labelled::var_label(x[[n]]), character(1), USE.NAMES = FALSE)
  }
  local_mocked_bindings(kobo_form_version_ = function(...) kobo_form(asset))
  with_fake_server(fake_server(total = 1, rows = rows), {
    res <- kobo_data(asset)
    res_fr <- kobo_data(asset, lang = "Francais (fr)")
    res_label <- kobo_data(asset, colnames_label = TRUE)
  }, version = "v1")
  expect_identical(as.vector(res$pt_wkt), "POINT (2 1 3)")
  expect_identical(labels_of(res, derived), en)
  expect_identical(labels_of(res_fr, derived), fr)
  expect_true(all(en %in% names(res_label)))
  expect_identical(as.vector(res_label$`Record a location::latitude`), 1)

  ## kobo_lang_set gives the labels of kobo_data in that language
  local_mocked_bindings(kobo_form = function(x, ...) kobo_form.kobo_asset(asset))
  expect_identical(labels_of(kobo_lang_set(res, asset, "Francais (fr)"), derived), fr)
  expect_identical(labels_of(kobo_lang_set(res_fr, asset, "English (en)"), derived), en)
  back <- kobo_lang_set(res_label, asset, "Francais (fr)")
  expect_true(all(fr %in% names(back)))
})

## Questions of the same name in different groups

test_that("questions of the same name answered in one submission are not merged", {
  x <- tibble::tibble(
    `_id` = 1:3,
    `father/age` = c("40", NA, "51"),
    `mother/age` = c("38", "35", NA),
    `mother/name` = c("A", "B", "C")
  )
  expect_message(res <- dedup_vars_(x), "father/age")
  expect_identical(res, x)

  ## A question moved to another group fills one column per submission
  moved <- tibble::tibble(`_id` = 1:3, `g1/sex` = c("1", NA, NA),
                          `g2/sex` = c(NA, "2", NA), `g3/sex` = c(NA, NA, "1"))
  expect_no_message(res <- dedup_vars_(moved))
  expect_identical(res, tibble::tibble(`_id` = 1:3, `g1/sex` = c("1", "2", "1")))

  ## Both cases under one name
  both <- tibble::tibble(`g1/sex` = c("1", NA), `g2/sex` = c(NA, "2"),
                         `other/sex` = c("9", "8"))
  expect_message(res <- dedup_vars_(both), "other/sex")
  expect_identical(res, tibble::tibble(`g1/sex` = c("1", "2"), `other/sex` = c("9", "8")))

  ## The merged column keeps its place, so the first question keeps the name
  x <- tibble::tibble(`g1/age` = c("10", NA), `g2/age` = c("20", "30"),
                      `g3/age` = c(NA, "15"))
  res <- suppressMessages(dedup_vars_(x))
  names(res) <- make_unique_names_(names(res))
  expect_identical(res, tibble::tibble(age = c("10", "15"), age_1 = c("20", "30")))
})

test_that("kobo_data keeps the answers of questions of the same name in two groups", {
  asset <- versions_asset()
  asset$uid <- "aSameNameTwoGroups00000"
  asset$deployed_versions <- NULL
  asset$content$survey <- list(
    versions_q("begin_group", "father", "Father"),
    versions_q("integer", "age", "Age"),
    list(type = "end_group"),
    versions_q("begin_group", "mother", "Mother"),
    versions_q("integer", "age", "Age"),
    list(type = "end_group")
  )
  withr::defer(kobo_cache_clear(asset$uid))
  rows <- function(ids) {
    data.frame(`_id` = ids, `__version__` = "vNEW", `father/age` = c("40", NA)[ids],
               `mother/age` = c("38", "35")[ids], check.names = FALSE)
  }
  with_fake_server(fake_server(total = 2, rows = rows), {
    local_mocked_bindings(kobo_form_version_ = function(...) kobo_form(asset))
    expect_message(res <- kobo_data(asset), "separate columns")
  }, version = "vNEW")
  expect_identical(as.vector(res$age), c(40L, NA))
  expect_identical(as.vector(res$age_1), c(38, 35))
})

## Usage shown in the help pages

test_that("generics have the arguments and the defaults of their methods", {
  expect_identical(formals(kobo_data), formals(kobo_data.kobo_asset))
  expect_identical(formals(kobo_data), formals(kobo_data.character))
  expect_identical(formals(kobo_submissions), formals(kobo_data))
  expect_identical(formals(kobo_audit), formals(kobo_audit.kobo_asset))
  expect_identical(formals(kobo_audit), formals(kobo_audit.character))
  expect_identical(
    formals(kobo_attachment_download),
    formals(kobo_attachment_download.kobo_asset)
  )
  expect_identical(
    formals(kobo_attachment_download),
    formals(kobo_attachment_download.character)
  )
})

## Progress output

test_that("kobo_data returns the same data with and without progress", {
  rg_rows <- function(ids) {
    data.frame(`_id` = ids, `__version__` = rep(asset_rg$deployed_version_id, length(ids)),
               check.names = FALSE)
  }
  cases <- list(
    flat = list(asset = asset_ml, rows = ml_rows),
    repeats = list(asset = asset_rg, rows = rg_rows)
  )
  for (case in names(cases)) {
    asset <- cases[[case]]$asset
    asset$deployment__submission_count <- 3L
    msgs <- character()
    with_fake_server(fake_server(total = 3, rows = cases[[case]]$rows), {
      quiet <- kobo_data(asset, page_size = 1)
      loud <- withCallingHandlers(
        kobo_data(asset, page_size = 1, progress = TRUE),
        message = function(m) {
          msgs <<- c(msgs, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      )
    }, version = asset$deployed_version_id)
    ## The tables of a dm get new identifiers at each call
    content <- function(x) {
      if (!inherits(x, "dm")) {
        return(x)
      }
      list(as.list(dm::dm_get_tables(x)), dm::dm_get_all_pks(x), dm::dm_get_all_fks(x))
    }
    expect_identical(content(loud), content(quiet), info = case)
    expect_true(any(grepl("Downloading data", msgs)), info = case)
    expect_true(any(grepl("Processing data", msgs)), info = case)
  }
  expect_s3_class(loud, "dm")
})

## Attachment files are written batch by batch

test_that("a failed batch keeps the files of the batches before it and no partial file", {
  dir <- withr::local_tempdir()
  att <- tibble::tibble(`_id` = 1:20, download_url = sprintf("https://fake.kobo.org/f%d", 1:20),
                        uid = sprintf("att%02d", 1:20), media_file_basename = "a.txt")
  n_batch <- 0L
  local_mocked_bindings(async_send_ = function(reqs) {
    n_batch <<- n_batch + 1L
    list(
      status = rep(if (n_batch == 1L) 200L else 500L, length(reqs)),
      content = lapply(reqs, function(r) charToRaw(basename(r$url)))
    )
  })
  expect_error(
    kobo_attachment_download_(att, dir, overwrite = TRUE, n_retry = 1L),
    class = "robotoolbox_http_500"
  )
  expect_identical(n_batch, 2L)
  expect_identical(sort(list.files(dir)), sprintf("att%02d_a.txt", 1:15))
  expect_identical(readLines(file.path(dir, "att07_a.txt"), warn = FALSE), "f7")

  ## A second call with `overwrite = FALSE` asks for the missing files only
  asked <- character()
  local_mocked_bindings(async_send_ = function(reqs) {
    asked <<- c(asked, vapply(reqs, function(r) basename(r$url), ""))
    list(status = rep(200L, length(reqs)),
         content = lapply(reqs, function(r) charToRaw(basename(r$url))))
  })
  paths <- kobo_attachment_download_(att, dir, overwrite = FALSE, n_retry = 1L,
                                     progress = TRUE)
  expect_identical(asked, sprintf("f%d", 16:20))
  expect_identical(basename(paths), sprintf("att%02d_a.txt", 16:20))
  expect_identical(sort(list.files(dir)), sprintf("att%02d_a.txt", 1:20))
})

## Two select_multiple questions colliding

test_that("a select_multiple question renamed by a dummy column keeps its dummy columns", {
  asset <- collide_asset(list(type = "select_multiple", name = "pet_other",
                              label = list("Other kind"), select_from_list_name = "k"))
  rows <- collide_rows(c("a b", "b"))
  expected <- list(
    names = c("hh", "pet", "pet_cat", "pet_dog", "pet_other", "pet_other_1",
              "pet_other_a", "pet_other_b", "last"),
    values = list(c("A", "B"), c("cat other", "dog"), c(1L, 0L), c(0L, 1L), c(1L, 0L),
                  c("a b", "b"), c(1L, 0L), c(1L, 1L), c("x", "y")),
    labels = c("Household", "Pet", "Pet::Cat", "Pet::Dog", "Pet::Other", "Other kind",
               "Other kind::Kind A", "Other kind::Kind B", "Last")
  )
  res <- collide_data(asset, rows)
  expect_identical(form_cols(res), expected)

  ## The question that comes first in the form gives the same columns
  first <- collide_data(collide_asset(asset$content$survey[[3]], q_first = TRUE), rows)
  expect_setequal(form_cols(first)$names, expected$names)
  for (n in expected$names) {
    expect_identical(first[[n]], res[[n]], info = n)
  }

  ## Labels instead of codes, and their translation
  lab <- collide_data(asset, rows, select_multiple_label = TRUE)
  expect_identical(as.vector(lab$pet), c("Cat Other", "Dog"))
  expect_identical(as.vector(lab$pet_other), c(1L, 0L))
  expect_identical(as.vector(lab$pet_other_1), c("Kind A Kind B", "Kind B"))
  expect_identical(labelled::var_label(lab$pet_other_1), "Other kind")
  local_mocked_bindings(kobo_form = function(x, ...) kobo_form.kobo_asset(asset))
  attr(lab$pet_other_1, "kobo_sm_lang") <- "other"
  attr(lab$pet, "kobo_sm_lang") <- "other"
  expect_error(kobo_lang_set(lab, asset, kobo_lang(asset)[1]), NA)
})

## A question named like a derived spatial column

test_that("a question named like a derived spatial column keeps its answers", {
  geo_q <- function(type, name, label) list(type = type, name = name, label = list(label))
  asset <- collide_asset()
  asset$uid <- "aSpatialNameCollision00"
  asset$content$survey <- list(
    geo_q("geotrace", "track", "Track"), geo_q("text", "track_wkt", "Track note"),
    geo_q("geoshape", "area", "Area"), geo_q("integer", "area_wkt", "Area count"),
    geo_q("geopoint", "pt", "Point"), geo_q("text", "pt_latitude", "Latitude note"),
    geo_q("text", "pt_wkt", "Point note"), geo_q("text", "pt_wkt_1", "Other note")
  )
  withr::defer(kobo_cache_clear(asset$uid))
  rows <- function(ids) {
    data.frame(`_id` = ids, `__version__` = "v1", track = "10 20 0 0;11 21 0 0",
               track_wkt = "Keep this answer", area = "0 0 0 0;0 1 0 0;1 1 0 0;0 0 0 0",
               area_wkt = "007", pt = "1 2 3 4", pt_latitude = "north",
               pt_wkt = "a point", pt_wkt_1 = "kept too", check.names = FALSE)
  }
  with_fake_server(fake_server(total = 1, rows = rows), {
    local_mocked_bindings(kobo_form_version_ = function(...) kobo_form(asset))
    expect_no_warning(expect_message(res <- kobo_data(asset), "track_wkt_1"))
  }, version = "v1")
  expect_identical(form_cols(res), list(
    names = c("track", "track_wkt", "track_wkt_1", "area", "area_wkt", "area_wkt_1",
              "pt", "pt_latitude", "pt_longitude", "pt_altitude", "pt_precision",
              "pt_wkt", "pt_latitude_1", "pt_wkt_2", "pt_wkt_1"),
    values = list("10 20 0 0;11 21 0 0", "LINESTRING (20 10 0, 21 11 0)", "Keep this answer",
                  "0 0 0 0;0 1 0 0;1 1 0 0;0 0 0 0", "POLYGON ((0 0 0, 1 0 0, 1 1 0, 0 0 0))",
                  7L, "1 2 3 4", 1, 2, 3, 4, "POINT (2 1 3)", "north", "a point", "kept too"),
    labels = c("Track", "Track::wkt", "Track note", "Area", "Area::wkt", "Area count",
               "Point", "Point::latitude", "Point::longitude", "Point::altitude",
               "Point::precision", "Point::wkt", "Latitude note", "Point note", "Other note")
  ))
})

test_that("polygons must be closed and geometries keep one dimension", {
  shapes <- c("0 0;1 1;0 1;0 0", "0 0;1 1;0 1;2 2", "0 0 5 0;1 1;0 1 2 0;0 0 5 0",
              "0 0 5 0;1 1 5 0;0 1 5 0;0 0 5 0")
  expect_identical(
    wkt_geo_lines_(shapes, "POLYGON ((", "))", min_pts = 4L, ring = TRUE),
    c("POLYGON ((0 0, 1 1, 1 0, 0 0))", NA, NA, "POLYGON ((0 0 5, 1 1 5, 1 0 5, 0 0 5))")
  )
  ## A line needs no closing point
  expect_identical(
    wkt_geo_lines_(c("0 0;1 1;0 1;2 2", "0 0 5 0;1 1"), "LINESTRING (", ")", min_pts = 2L),
    c("LINESTRING (0 0, 1 1, 1 0, 2 2)", NA)
  )
})

## Fields inside a group

test_that("a field is sent to the server under its name and its path", {
  q <- function(type, name, xpath) {
    list(type = type, name = name, label = list(name), `$xpath` = xpath)
  }
  asset <- versions_asset()
  asset$uid <- "aFieldsInsideGroups0000"
  asset$deployed_versions <- NULL
  asset$content$survey <- list(
    q("text", "hh", "hh"),
    q("begin_group", "household", "household"),
    q("integer", "age", "household/age"),
    list(type = "end_group"),
    q("begin_repeat", "member", "member"),
    q("text", "nick", "member/nick"),
    list(type = "end_repeat")
  )
  withr::defer(kobo_cache_clear(asset$uid))
  expect_identical(
    fields_api_(c("age", "hh"), asset),
    list(names = c("age", "hh"), api = c("age", "hh", "household/age"))
  )
  expect_identical(
    fields_api_("household/age", asset),
    list(names = "age", api = "household/age")
  )
  expect_identical(fields_api_("member", asset), list(names = "member", api = "member"))
  expect_identical(fields_api_(NULL, asset), list(names = NULL, api = NULL))
  expect_error(fields_api_("member/nick", asset), "Nested field selection")
  expect_error(fields_api_("nowhere/age", asset), "Nested field selection")
  expect_error(kobo_data(asset, fields = c("age", NA)), "missing or empty")

  ## The server answers under the path, and the column has the question name
  sent <- NULL
  server <- fake_server(total = 2, rows = function(ids) {
    data.frame(`_id` = ids, `__version__` = "vNEW", `household/age` = c("30", "41")[ids],
               check.names = FALSE)
  })
  fetch <- server$fetch
  server$fetch <- function(uid, start, limit, query = NULL, fields = NULL, ...) {
    sent <<- fields
    fetch(uid, start, limit, query, fields, ...)
  }
  for (fields in list("age", "household/age")) {
    with_fake_server(server, {
      local_mocked_bindings(kobo_form_version_ = function(...) kobo_form(asset))
      res <- kobo_data(asset, fields = fields)
    }, version = "vNEW")
    expect_match(sent, '"household/age"', fixed = TRUE)
    expect_identical(as.vector(res$main$age), c(30L, 41L))
    expect_false("hh" %in% names(res$main))
  }
})

## Attachment files

test_that("a file is never written over a folder, and a failed write is an error", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "att_picture.jpg")
  expect_identical(write_file_(charToRaw("one"), path), path)
  expect_identical(write_file_(charToRaw("two"), path), path)
  expect_identical(readLines(path, warn = FALSE), "two")
  expect_identical(list.files(dir), "att_picture.jpg")
  blocked <- file.path(dir, "att_folder.jpg")
  dir.create(blocked)
  expect_error(write_file_(charToRaw("x"), blocked), "a folder has this name")
  expect_identical(list.files(blocked), character())
  expect_identical(sort(list.files(dir)), c("att_folder.jpg", "att_picture.jpg"))
})

## Column types without answers

typed_asset <- function(with_repeat = FALSE) {
  q <- function(type, name, ...) list(type = type, name = name, label = list(name), ...)
  ch <- function(list_name, name) list(list_name = list_name, name = name, label = list(name))
  survey <- list(
    q("text", "hh"), q("integer", "age"), q("decimal", "weight"), q("date", "recorded"),
    q("dateTime", "seen"), q("select_one", "sex", select_from_list_name = "s"),
    q("select_multiple", "pet", select_from_list_name = "p"), q("geopoint", "point"),
    q("geotrace", "track")
  )
  if (with_repeat) {
    survey <- c(survey, list(q("begin_repeat", "member"), q("integer", "member_age"),
                             q("select_multiple", "skill", select_from_list_name = "p"),
                             list(type = "end_repeat")))
  }
  structure(
    list(
      uid = if (with_repeat) "aTypedColumnsRepeat000" else "aTypedColumnsFlat00000",
      asset_type = "survey",
      deployment__submission_count = 2L,
      version_id = "v1",
      deployed_version_id = "v1",
      content = list(
        survey = survey,
        choices = list(ch("s", "f"), ch("s", "m"), ch("p", "cat"), ch("p", "dog")),
        translations = list(NULL)
      )
    ),
    class = "kobo_asset"
  )
}

col_classes <- function(x) lapply(x, class)

test_that("columns have the same types with answers, without answers and without rows", {
  expected <- list(
    hh = "character", age = "integer", weight = "numeric", recorded = "Date",
    seen = c("POSIXct", "POSIXt"), sex = c("haven_labelled", "vctrs_vctr", "character"),
    pet = "character", pet_cat = "integer", pet_dog = "integer", point = "character",
    point_latitude = "numeric", point_longitude = "numeric", point_altitude = "numeric",
    point_precision = "numeric", point_wkt = "character", track = "character",
    track_wkt = "character"
  )
  asset <- typed_asset()
  withr::defer(kobo_cache_clear(asset$uid))
  full_rows <- function(ids) {
    data.frame(`_id` = ids, `__version__` = "v1", hh = "A", age = "18", weight = "61.5",
               recorded = "2026-11-01", seen = "2026-11-01T10:00:00.000+00:00",
               sex = "f", pet = "cat", point = "10 20 0 3", track = "1 2 0 0;3 4 0 0",
               check.names = FALSE)[seq_along(ids), ]
  }
  bare_rows <- function(ids) {
    data.frame(`_id` = ids, `__version__` = rep("v1", length(ids)), hh = "A",
               check.names = FALSE)
  }
  get <- function(rows, total = 2, ...) {
    with_fake_server(fake_server(total = total, rows = if (total > 0) rows), {
      local_mocked_bindings(kobo_form_version_ = function(...) kobo_form(asset))
      kobo_data(asset, ...)
    }, version = "v1")
  }
  full <- get(full_rows)
  bare <- get(bare_rows)
  empty <- get(bare_rows, total = 0)
  expect_identical(col_classes(full)[names(expected)], expected)
  expect_identical(col_classes(bare)[names(expected)], expected)
  expect_identical(col_classes(empty), expected)
  expect_identical(nrow(empty), 0L)
  expect_true(all(is.na(bare$age)) && all(is.na(bare$pet_cat)) && all(is.na(bare$seen)))

  ## The three results bind, in any order
  cols <- names(expected)
  bound <- dplyr::bind_rows(empty, bare[cols], full[cols])
  expect_identical(nrow(bound), 4L)
  expect_identical(col_classes(bound), expected)
  expect_identical(as.vector(bound$age), c(NA, NA, 18L, 18L))
  expect_identical(labelled::val_labels(empty$sex), labelled::val_labels(full$sex))
  expect_identical(labelled::var_label(empty$pet_cat), labelled::var_label(full$pet_cat))

  ## With fields and with labels as names
  some <- get(bare_rows, total = 0, fields = c("age", "recorded"))
  expect_identical(col_classes(some), expected[c("age", "recorded")])
  named <- get(bare_rows, total = 0, colnames_label = TRUE)
  expect_identical(unname(lapply(named, function(col) setdiff(class(col), "labelled"))),
                   unname(expected))
})

test_that("repeat tables have the same types with and without rows", {
  asset <- typed_asset(with_repeat = TRUE)
  withr::defer(kobo_cache_clear(asset$uid))
  rows <- function(ids) {
    d <- data.frame(`_id` = ids, `__version__` = "v1", hh = c("A", "B")[ids],
                    age = c("18", "30")[ids], check.names = FALSE)
    d$member <- list(
      data.frame(`member/member_age` = c("3", "5"), `member/skill` = c("cat", "cat dog"),
                 check.names = FALSE),
      NULL
    )[ids]
    d
  }
  get <- function(total) {
    with_fake_server(fake_server(total = total, rows = if (total > 0) rows), {
      local_mocked_bindings(kobo_form_version_ = function(...) kobo_form(asset))
      kobo_data(asset)
    }, version = "v1")
  }
  full <- get(2)
  empty <- get(0)
  expect_identical(names(empty), names(full))
  form_only <- function(x) x[!grepl("^_|^meta/|^formhub/", names(x))]
  for (tbl in names(full)) {
    expect_identical(col_classes(form_only(empty[[tbl]])), col_classes(form_only(full[[tbl]])),
                     info = tbl)
    expect_identical(nrow(empty[[tbl]]), 0L, info = tbl)
  }
  expect_identical(class(full$member$member_age), "integer")
  expect_identical(class(empty$member$skill_dog), "integer")
  expect_identical(class(full$main$pet_cat), "integer")
  expect_identical(as.vector(full$member$skill_dog), c(0L, 1L))
})
