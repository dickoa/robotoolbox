test_that("kobo_setup and kobo_settings are working", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  settings <- kobo_settings()
  expect_is(settings, "kobo_settings")
  expect_error(kobo_setup(url = "is_it_an_url",
                          token = token))
  expect_error(kobo_setup(url = url,
                          token = "wrong_token"))

  testthat::local_edition(3)
  expect_snapshot_output(print.kobo_settings(settings))
})
