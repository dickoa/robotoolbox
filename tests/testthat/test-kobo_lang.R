test_that("kobo_lang works", {
  url <- Sys.getenv("KOBOTOOLBOX_TRAINING_URL")
  token <- Sys.getenv("KOBOTOOLBOX_TRAINING_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url,
             token = token)
  uid <- "aJzZWSY9VMr5w38KBJPP5L"
  lng1 <- kobo_lang(uid)
  lng2 <- kobo_lang(kobo_asset(uid))
  expect_equal(lng1, lng2)
  expect_type(lng1, "character")
  expect_error(kobo_lang(1L))
  expect_error(kobo_lang("bad_uid"))
})
