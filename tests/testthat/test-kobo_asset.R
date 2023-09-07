test_that("kobo_asset works", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")
  kobo_setup(url = url, token = token)
  uid <- "aANhxwX9S6BCsiYMgQj9kV"
  asset1 <- kobo_asset(uid)
  asset2 <- kobo_asset(asset1)
  asset3 <- kobo_asset(asset2)
  expect_equal(asset1, asset2)
  expect_equal(asset1, asset3)
  expect_is(asset1, class = "kobo_asset")
  expect_error(kobo_asset(1L))
  expect_error(kobo_asset("bad_uid"))

  testthat::local_edition(3)
  expect_snapshot_output(print.kobo_asset(asset1))
})
