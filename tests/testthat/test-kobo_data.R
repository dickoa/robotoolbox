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

  if (any(gt_exists)) {
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
  for (count in list(0L, NULL)) {
    asset <- asset_ml
    asset["deployment__submission_count"] <- list(count)
    server <- fake_server(total = 3, rows = ml_rows)
    with_fake_server(server, {
      res <- kobo_data(asset)
    })
    expect_equal(as.vector(res$`_id`), 1:3)
    expect_equal(as.vector(res$full_name), rep("A", 3))
  }
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

## Query validation

test_that("kobo_data sends every JSON object query to the server", {
  local_mocked_bindings(get_filtered_count_ = function(uid, query) 2L)
  for (q in c("{}", " {} ", '\n{"full_name": "A"}\t', '{"a": [1, {"b": 2}]}')) {
    sent <- NULL
    server <- fake_server(total = 2, rows = ml_rows)
    fetch <- server$fetch
    server$fetch <- function(uid, start, limit, query = NULL, fields = NULL) {
      sent <<- query
      fetch(uid, start, limit, query, fields)
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
