test_that("kobo_setup and kobo_settings are working", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <- url == "" & token == ""
  skip_if(skip, message = "Test server not configured")

  kobo_setup(url = url, token = token)
  settings <- kobo_settings()
  expect_s3_class(settings, "kobo_settings")
  expect_error(kobo_setup(url = "is_it_an_url", token = token))
  expect_error(kobo_setup(url = url, token = "wrong_token"))

  fake <- structure(
    list(
      token = "0000000000000000000000000000000000000000",
      url = "https://kobo.example.org",
      page_size = 1000L
    ),
    class = "kobo_settings"
  )
  testthat::local_edition(3)
  expect_snapshot_output(print(fake))
})

test_that("kobo_setup max page size", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <- url == "" & token == ""
  skip_if(skip, message = "Test server not configured")

  kobo_setup(url = url, token = token)
  settings <- kobo_settings()
  expect_equal(settings$page_size, 1000)

  kobo_setup(url = url, token = token, page_size = 100)
  new_settings <- kobo_settings()
  expect_equal(new_settings$page_size, 100)

  ## restore
  kobo_setup(url = url, token = token, page_size = 1000)
})

## Offline checks of the setup

test_that("assert_url accepts self-hosted servers and rejects malformed URLs", {
  ok <- c(
    "https://kf.kobotoolbox.org", "https://kobo.unhcr.org/", "kobo.unhcr.org",
    "https://kobo.humanitarian.international", "http://localhost:8000",
    "http://192.168.1.10:8080", "https://KOBO.Example.org", "https://data.agency.org/kobo/",
    "https://data.agency.org/~kobo"
  )
  bad <- c("", NA, "https://", "kobo", "https://kobo.", "ftp://kobo.org",
           "https://kobo .org", "https://kobo.org:99999")
  for (u in ok) expect_true(assert_url(u), info = u)
  for (u in bad) expect_false(assert_url(u), info = u)
  expect_false(assert_url(1))
  expect_false(assert_url(character()))
  expect_false(assert_url(c("https://kf.kobotoolbox.org", "https://kobo.unhcr.org")))
})

test_that("an unset token is shown as unset", {
  expect_identical(hide_token(""), "<unset>")
  expect_identical(
    hide_token("0000000000000000000000000000000000000000"),
    "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx00000"
  )
})

test_that("kobo_setup and kobo_data reject a missing page size", {
  expect_error(
    kobo_setup("https://kobo.example.org", "0000000000000000000000000000000000000000", NA_integer_),
    "positive integer"
  )
  expect_error(kobo_data(asset_ml, page_size = NA_integer_), "positive integer")
  expect_error(kobo_data(asset_ml, page_size = c(10, 20)), "positive integer")
})

test_that("requests without a configured server give a setup hint", {
  old <- Sys.getenv("KOBOTOOLBOX_URL")
  Sys.setenv(KOBOTOOLBOX_URL = "")
  on.exit(Sys.setenv(KOBOTOOLBOX_URL = old))
  expect_error(kobo_asset("aNGonMrGApwWBDDBmxnTBS"), "not configured")
  expect_error(kobo_asset_list(), "not configured")
  expect_error(fetch_subs_async_("aNGonMrGApwWBDDBmxnTBS", c(0, 10), c(10, 10)), "not configured")
  expect_error(fetch_asset_versions_async_("aNGonMrGApwWBDDBmxnTBS", "vSrNdY3DZMaeXmUJ98LW2p"), "not configured")
})

test_that("error_msg shows the server message for other errors", {
  msg <- function(detail) charToRaw(sprintf('{"detail": "%s"}', detail))
  expect_identical(error_msg(msg("Not found."))[[1]], "uid (project) not found in this account/server")
  expect_identical(error_msg(msg("Invalid token."))[[1]], "Invalid API token")
  res <- error_msg(msg("You do not have permission to perform this action."))
  expect_identical(res[[1]], "You do not have permission to perform this action.")
  expect_named(res, c("", "i"))
  res <- error_msg(msg("Request was throttled. Expected available in 5 seconds."))
  expect_identical(res[[1]], "Request was throttled. Expected available in 5 seconds.")
  expect_identical(error_msg(charToRaw("<html>oops</html>")),
                   c(i = "Check your uid and/or credentials and try again"))
})

test_that("kobo_setup keeps the page size of the environment variable", {
  withr::local_envvar(KOBOTOOLBOX_URL = "https://k.org", KOBOTOOLBOX_TOKEN = "",
                      KOBOTOOLBOX_PAGE_SIZE = "30000")
  token <- "0000000000000000000000000000000000000000"
  expect_identical(kobo_setup(url = "https://k.org", token = token)$page_size, 30000L)
  expect_identical(Sys.getenv("KOBOTOOLBOX_PAGE_SIZE"), "30000")
  expect_identical(kobo_setup(url = "https://k.org", token = token, page_size = 500)$page_size, 500L)
  Sys.unsetenv("KOBOTOOLBOX_PAGE_SIZE")
  expect_identical(kobo_setup(url = "https://k.org", token = token)$page_size, 1000L)
  expect_null(formals(kobo_setup)$page_size)
  expect_identical(kobo_setup(url = "https://k.org", token = token, page_size = NULL)$page_size, 1000L)
})

test_that("the page size variable is read as a positive integer or refused", {
  token <- "0000000000000000000000000000000000000000"
  withr::local_envvar(KOBOTOOLBOX_URL = "https://k.org", KOBOTOOLBOX_TOKEN = token)
  ## An empty variable counts as not set
  for (value in c("", "  ")) {
    withr::local_envvar(KOBOTOOLBOX_PAGE_SIZE = value)
    expect_identical(api_max_limit_(), 1000L)
    expect_identical(kobo_settings()$page_size, 1000L)
    expect_identical(kobo_setup()$page_size, 1000L)
  }
  withr::local_envvar(KOBOTOOLBOX_PAGE_SIZE = " 500 ")
  expect_identical(api_max_limit_(), 500L)
  for (value in c("abc", "0", "-5", "1e3x")) {
    withr::local_envvar(KOBOTOOLBOX_PAGE_SIZE = value)
    expect_error(api_max_limit_(), "KOBOTOOLBOX_PAGE_SIZE")
    expect_error(kobo_setup(), "KOBOTOOLBOX_PAGE_SIZE")
    ## An explicit page size does not read the variable
    expect_identical(kobo_setup(page_size = 200)$page_size, 200L)
  }
})
