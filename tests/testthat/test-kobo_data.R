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

  expect_is(raw_en,
            c("tbl_df", "tbl", "data.frame"))
  expect_is(raw_fr,
            c("tbl_df", "tbl", "data.frame"))
  expect_is(raw_ar,
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
  expect_is(raw_uid, "dm")
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
    expect_is(raw[[paste0(gp_nm, "_latitude")]], "numeric")
    expect_is(raw[[paste0(gp_nm, "_longitude")]], "numeric")
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

test_that("with kobo_data can restrict the data to the latest form version", {
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
  expect_equal(identical(raw1, raw2), FALSE)
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
