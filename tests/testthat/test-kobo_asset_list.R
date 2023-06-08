test_that("kobo_asset_list works", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  al <- kobo_asset_list()
  expect_equal(class(al),
                         c("tbl_df", "tbl", "data.frame"))

  cn <- c("uid", "name", "asset_type", "owner_username", "date_created",
          "date_modified", "deployed", "submissions")

  expect_equal(names(al), cn)
})

test_that("kobo_asset_list works even if the number of assets exceed limit", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  al <- kobo_asset_list(limit = 5)
  expect_equal(class(al),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(al), 5L)
})
