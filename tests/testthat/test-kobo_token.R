test_that("kobo_token works", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  user <- Sys.getenv("KOBOTOOLBOX_PROD_USER")
  pwd <- Sys.getenv("KOBOTOOLBOX_PROD_PWD")
  skip <-  url == "" | user == "" | pwd == ""
  skip_if(skip,
          message = "Test server not configured")

  Sys.setenv("KOBOTOOLBOX_URL" = "")
  expect_error(kobo_token(username = user,
                          password = password,
                          url = "bad_url",
                          overwrite = TRUE))

  Sys.setenv("KOBOTOOLBOX_TOKEN" = "")
  token <- kobo_token(username = user,
                      password = pwd,
                      url = url,
                      overwrite = TRUE)

  token2 <- kobo_token(username = user,
                       password = pwd,
                       url = url)

  expect_equal(token, token2)
  expect_is(token, class = "character")
  expect_is(token2, class = "character")
  expect_equal(Sys.getenv("KOBOTOOLBOX_URL"),
               Sys.getenv("KOBOTOOLBOX_PROD_URL"))
  expect_equal(Sys.getenv("KOBOTOOLBOX_TOKEN"),
               Sys.getenv("KOBOTOOLBOX_PROD_TOKEN"))

  Sys.setenv("KOBOTOOLBOX_TOKEN" = "")
  expect_error(kobo_token(username = user,
                          password = "blablah",
                          url = url,
                          overwrite = TRUE))

  Sys.setenv("KOBOTOOLBOX_URL" =  Sys.getenv("KOBOTOOLBOX_PROD_URL"))
  token3 <- kobo_token(username = user,
                       password = pwd)
  expect_equal(token, token3)

})
