test_that("kobo_asset_version_list works", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aANhxwX9S6BCsiYMgQj9kV"
  al1 <- kobo_asset_version_list(uid)
  al2 <- kobo_asset_version_list(kobo_asset(uid))

  expect_equal(al1, al2)
  expect_error(kobo_asset_version_list(1L))
  expect_error(kobo_asset_version_list("bad_uid"))
  expect_is(al1, class = c("tbl_df", "tbl", "data.frame"))
  expect_is(al2, class = c("tbl_df", "tbl", "data.frame"))

  cn <- c("uid", "url", "deployed", "date_modified")
  expect_equal(names(al1), cn)
})
