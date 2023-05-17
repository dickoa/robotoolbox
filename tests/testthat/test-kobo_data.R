test_that("with kobo_data you can read your submissions", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aYuTZn9vegi3Z49MXwKjep"
  asset <- kobo_asset(uid)
  raw_uid <- kobo_data(uid)
  raw_asset <- kobo_data(asset)
  raw_paginate <- kobo_data(asset, paginate = TRUE, page_size = 2)
  raw_paginate2 <- kobo_data(asset, paginate = TRUE) ## default
  expect_equal(raw_uid, raw_asset)
  expect_equal(raw_uid, raw_paginate)
  expect_equal(raw_paginate, raw_paginate2)
  expect_equal(class(raw_uid),
               c("tbl_df", "tbl", "data.frame"))
  expect_error(kobo_data(1L))
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
  raw_en <- kobo_data(uid, lang = "English (en)")
  raw_fr <- kobo_data(uid, lang = "Francais (fr)")
  raw_ar <- kobo_data(uid, lang = "Arabic (ar)")

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
  raw_uid <- kobo_data(uid)
  expect_is(raw_uid, "dm")
})

test_that("kobo_submissions is similar to kobo_data", {
  skip_on_cran()
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


test_that("kobo_data paginate automatically large data", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "a7XzRuPFn9j5WkT2mR6wbg"
  asset <- kobo_asset(uid)
  raw <- kobo_data(uid)
  raw1 <- kobo_data(uid, paginate = TRUE)

  expect_equal(raw, raw1)
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
  form <- kobo_form(uid)
  cond <- form$type %in% "select_multiple"
  sm_col <- form$name[cond]
  sm_col <- sm_col[1]
  choices <- form$choices[form$name %in% sm_col][[1]]
  label <- unique(choices$value_label)
  raw_label <- kobo_data(uid,
                         select_multiple_label = TRUE)

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
  form <- kobo_form(uid)
  raw <- kobo_data(uid)

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

test_that("with kobo_data and set_names_from_varlabel you can use labels as names", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid_simple <- "aYuTZn9vegi3Z49MXwKjep"
  uid_dm <- "aANhxwX9S6BCsiYMgQj9kV"
  raw_simple <- kobo_data(uid_simple)
  raw_dm <- kobo_data(uid_dm)

  df_simple <- set_names_from_varlabel(raw_simple)
  df_dm <- set_names_from_varlabel(raw_dm)

  expect_equal(class(df_simple),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(class(df_dm), "dm")
  expect_error(set_names_from_varlabel(1L))
})
