test_that("kobo_asset_file_list works", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_TRAINING_URL")
  token <- Sys.getenv("KOBOTOOLBOX_TRAINING_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aUJbF9hPVWfo3o83S8Ageq"
  vcr::use_cassette("kobo_asset_file_list", {
    fl <- kobo_asset_file_list(uid)
    fl_asset <- kobo_asset_file_list(kobo_asset(uid))
  })
  expect_equal(class(fl),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(fl, fl_asset)

  cn <- c("uid", "url", "asset", "user", "user__username", "file_type",
          "description", "date_created", "content", "hash", "filename",
          "mimetype")
  expect_equal(names(fl), cn)

  vcr::use_cassette("kobo_asset_file_list_empty", {
    uid_no_file <- "aDdgw9sZZeRUcW7aWryQB6"
    fl_empty <- kobo_asset_file_list(uid_no_file)
  })
  expect_equal(nrow(fl_empty), 0)
})
