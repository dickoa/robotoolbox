test_that("kobo_form works", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aANhxwX9S6BCsiYMgQj9kV"
  vcr::use_cassette("kobo_form", {
    form1 <- kobo_form(uid)
    form2 <- kobo_form(kobo_asset(uid))
  })
  expect_equal(form1, form2)
  expect_equal(class(form1),
               c("tbl_df", "tbl", "data.frame"))
  expect_error(kobo_form(1L))
  expect_error(kobo_form("bad_uid"))
})

test_that("kobo_form has a version argument", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aREsLnfwNU9L7ePbUjnajg"
  vcr::use_cassette("kobo_form_version", {
    asset <- kobo_asset(uid)
    asset_versions <- kobo_asset_version_list(asset)
    form1 <- kobo_form(uid, version = asset_versions$uid[1])
    form2 <- kobo_form(asset, version = asset_versions$uid[1])
  })
  expect_equal(form1, form2)
  expect_equal(class(form1),
               c("tbl_df", "tbl", "data.frame"))
 })

test_that("kobo_form can load form without choices tab", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aEnSVDdSc82qv84mgWEcvs"
  vcr::use_cassette("kobo_form_no_choices", {
    asset <- kobo_asset(uid)
    form <- kobo_form(uid)
  })
  expect_true(!"choices" %in% names(form))
 })

test_that("kobo_form works with external files", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_TRAINING_URL")
  token <- Sys.getenv("KOBOTOOLBOX_TRAINING_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aUJbF9hPVWfo3o83S8Ageq"
  vcr::use_cassette("kobo_form_external_files", {
    form <- kobo_form(uid)
  })
  # fruits choices are coming from a csv
  ch <- form$choices[form$name %in% "fruits"]
  expect_gte(length(ch), 1)
})
