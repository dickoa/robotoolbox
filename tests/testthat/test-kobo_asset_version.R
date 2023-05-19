test_that("kobo_asset_version_list works", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aANhxwX9S6BCsiYMgQj9kV"
  version_uid <- "vcs3hEpGKxBo8G5uQa94oD"
  asset_version1 <- kobo_asset_version(uid, version_uid)
  asset_version2 <- kobo_asset_version(kobo_asset(uid),
                                       version_uid)
  expect_equal(asset_version1, asset_version2)
  expect_is(asset_version1, "kobo_asset_version")
  expect_error(kobo_asset_version(1L, version_uid))
  expect_error(kobo_asset_version("bad_uid", version_uid))
  expect_error(kobo_asset_version(uid, "bad_version_uid"))

  testthat::local_edition(3)
  expect_snapshot_output(print.kobo_asset_version(asset_version1))
})
