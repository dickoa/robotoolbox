test_that("kobo_token works", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  user <- Sys.getenv("KOBOTOOLBOX_PROD_USER")
  pwd <- Sys.getenv("KOBOTOOLBOX_PROD_PWD")
  skip <-  url == "" | user == "" | pwd == ""
  skip_if(skip,
          message = "Test server not configured")

  Sys.setenv("KOBOTOOLBOX_URL" = "")
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
})

test_that("kobo_token set the kobotoolbox_url env", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  user <- Sys.getenv("KOBOTOOLBOX_PROD_USER")
  pwd <- Sys.getenv("KOBOTOOLBOX_PROD_PWD")
  skip <-  url == "" | user == "" | pwd == ""
  skip_if(skip,
          message = "Test server not configured")

  Sys.setenv("KOBOTOOLBOX_URL" = "")
  Sys.setenv("KOBOTOOLBOX_TOKEN" = "")
  token <- kobo_token(username = user,
                      password = pwd,
                      url = url,
                      overwrite = TRUE)

  expect_is(token, class = "character")
  expect_equal(Sys.getenv("KOBOTOOLBOX_URL"),
               Sys.getenv("KOBOTOOLBOX_PROD_URL"))
  expect_equal(Sys.getenv("KOBOTOOLBOX_TOKEN"),
               Sys.getenv("KOBOTOOLBOX_PROD_TOKEN"))
})
