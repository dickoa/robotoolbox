test_that("kobo_token works", {
  skip_on_cran()
  skip_if(!has_live_api(), message = "Requires live API")
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
  expect_type(token, "character")
  expect_type(token2, "character")
  expect_equal(Sys.getenv("KOBOTOOLBOX_URL"),
               clean_urls(Sys.getenv("KOBOTOOLBOX_PROD_URL")))
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

test_that("kobo_token strips a trailing slash from the server URL", {
  old_url <- Sys.getenv("KOBOTOOLBOX_URL")
  old_tok <- Sys.getenv("KOBOTOOLBOX_TOKEN")
  on.exit({
    Sys.setenv(KOBOTOOLBOX_URL = old_url, KOBOTOOLBOX_TOKEN = old_tok)
  })
  Sys.setenv(KOBOTOOLBOX_TOKEN = "0000000000000000000000000000000000000000",
             KOBOTOOLBOX_URL = "https://kobo.example.org//")
  tok <- kobo_token(url = "https://kobo.example.org/", overwrite = FALSE)
  expect_identical(tok, "0000000000000000000000000000000000000000")
  expect_identical(Sys.getenv("KOBOTOOLBOX_URL"), "https://kobo.example.org")
})

test_that("kobo_token reuses a token for its own server only", {
  old_token <- strrep("a", 40)
  new_token <- strrep("b", 40)
  withr::local_envvar(KOBOTOOLBOX_URL = "https://old.example.org",
                      KOBOTOOLBOX_TOKEN = old_token)
  asked <- character()
  answer <- function(status, body) {
    list(new = function(url, ...) {
      asked <<- c(asked, url)
      list(retry = function(...) list(
        status_code = status,
        content = charToRaw(body),
        raise_for_ct_json = function() NULL,
        parse = function(...) body
      ))
    })
  }
  local_mocked_bindings(HttpClient = answer(200L, sprintf('{"token": "%s"}', new_token)))
  ## Same server, with and without the URL
  expect_identical(kobo_token(), old_token)
  expect_identical(kobo_token(url = "https://old.example.org/"), old_token)
  expect_identical(asked, character())

  ## Another server is asked, and a failure changes nothing
  local_mocked_bindings(HttpClient = answer(401L, '{"detail": "Invalid username/password."}'))
  expect_error(kobo_token("u", "p", url = "https://new.example.org"), "Invalid username")
  expect_identical(asked, "https://new.example.org/token/?format=json")
  expect_identical(Sys.getenv("KOBOTOOLBOX_URL"), "https://old.example.org")
  expect_identical(Sys.getenv("KOBOTOOLBOX_TOKEN"), old_token)

  local_mocked_bindings(HttpClient = answer(200L, sprintf('{"token": "%s"}', new_token)))
  expect_identical(kobo_token("u", "p", url = "https://new.example.org"), new_token)
  expect_identical(Sys.getenv("KOBOTOOLBOX_URL"), "https://new.example.org")
  expect_identical(Sys.getenv("KOBOTOOLBOX_TOKEN"), new_token)
})
