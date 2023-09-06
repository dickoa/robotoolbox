test_that("kobo_form works", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_TRAINING_URL")
  token <- Sys.getenv("KOBOTOOLBOX_TRAINING_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aJzZWSY9VMr5w38KBJPP5L"
  form1 <- kobo_form(uid)
  form2 <- kobo_form(kobo_asset(uid))
  expect_equal(form1, form2)
  expect_equal(class(form1),
               c("tbl_df", "tbl", "data.frame"))
  expect_error(kobo_form(1L))
  expect_error(kobo_form("bad_uid"))
})

test_that("kobo_form has a version argument", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aDMHypRASH2EuJdvKMx5Mi"
  asset <- kobo_asset(uid)
  asset_versions <- kobo_asset_version_list(asset)
  form1 <- kobo_form(uid, version = asset_versions$uid[1])
  form2 <- kobo_form(asset, version = asset_versions$uid[1])
  expect_equal(form1, form2)
  expect_equal(class(form1),
               c("tbl_df", "tbl", "data.frame"))
 })

test_that("kobo_form can load form without choices tab", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_TRAINING_URL")
  token <- Sys.getenv("KOBOTOOLBOX_TRAINING_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "a5Yu94HNrpnPHALYH2r2ZS"
  asset <- kobo_asset(uid)
  form <- kobo_form(uid)
  expect_true(!"choices" %in% names(form))
 })
