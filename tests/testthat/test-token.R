test_that("kobo_token works", {
  url <- Sys.getenv("KOBOTOOLBOX_TEST_URL")
  user <- Sys.getenv("KOBOTOOLBOX_TEST_USER")
  pwd <- Sys.getenv("KOBOTOOLBOX_TEST_PWD")
  skip <-  url == "" | user == "" | pwd == ""
  skip_if(skip,
          message = "Test server not configured")

  token <- kobo_token(username = user,
                      password = pwd,
                      url = url)

  token2 <- kobo_token(username = user,
                       password = pwd,
                       url = url,
                       overwrite = TRUE)

  expect_is(token, class = "character")
  expect_is(token2, class = "character")
})
