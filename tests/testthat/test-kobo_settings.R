test_that("kobo_asset_version_list works", {
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  settings <- kobo_settings()
  expect_is(settings, "kobo_settings")

  testthat::local_edition(3)
  expect_snapshot_output(print.kobo_settings(settings))
})
