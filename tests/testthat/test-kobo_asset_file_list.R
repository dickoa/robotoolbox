test_that("kobo_asset_file_list works", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_TRAINING_URL")
  token <- Sys.getenv("KOBOTOOLBOX_TRAINING_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aUJbF9hPVWfo3o83S8Ageq"
  fl <- kobo_asset_file_list(uid)
  expect_equal(class(fl),
               c("tbl_df", "tbl", "data.frame"))

  fl_asset <- kobo_asset_file_list(kobo_asset(uid))
  expect_equal(fl, fl_asset)

  cn <- c("uid", "url", "asset", "user", "user__username", "file_type",
          "description", "date_created", "content", "hash", "filename",
          "mimetype")
  expect_equal(names(fl), cn)

  uid_no_file <- "aDdgw9sZZeRUcW7aWryQB6"
  expect_equal(nrow(kobo_asset_file_list(uid_no_file)), 0)
})
