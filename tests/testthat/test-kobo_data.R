test_that("with kobo_data you can read your submissions", {
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
