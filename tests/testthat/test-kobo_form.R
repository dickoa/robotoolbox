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

test_that("kobo_form_version_ finds every version whatever the question names", {
  dep <- "vDDDDDDDDDDDDDDDDDDDDD"
  old <- "vOOOOOOOOOOOOOOOOOOOOO"
  asset <- list(uid = "aFakeFakeFakeFakeFakeF")
  fetched <- character()
  local_mocked_bindings(
    kobo_form = function(x, version = NULL) {
      if (!is.null(version)) fetched <<- c(fetched, version)
      tibble::tibble(name = "q", version = version %||% dep)
    },
    kobo_asset_version_list = function(x) {
      tibble::tibble(uid = c(dep, old), deployed = TRUE)
    },
    has_cached_form_ = function(uid, version) FALSE,
    get_cached_form_ = function(uid, version) {
      tibble::tibble(name = "q_old", version = version)
    }
  )
  subs <- list(
    question = tibble::tibble(`__version__` = c(dep, old), app_version_num = 1:2),
    spanned = tibble::tibble(
      `__version__` = c(dep, dep),
      `meta/formVersions` = c(NA, paste(old, dep))
    ),
    legacy = tibble::tibble(`_version_` = c(old, "20200315"))
  )
  for (n in names(subs)) {
    fetched <- character()
    form <- kobo_form_version_(subs[[n]], asset, all_versions = TRUE)
    expect_identical(fetched, old, info = n)
    expect_identical(form$version, c(dep, old), info = n)
  }
  form <- kobo_form_version_(subs$question, asset, all_versions = FALSE)
  expect_identical(form$version, dep)
})

test_that("rows without a name give no column name", {
  form <- kobo_form(asset_rg)
  expect_true(any(is.na(form$name)))
  expect_false("NA" %in% kobo_form_names_(form, "_"))
  expect_false("NA" %in% unlist(kobo_form_name_to_list_(form, "_")))
})

test_that("deviceid answers are read as text", {
  form <- tibble::tibble(name = c("deviceid", "n"), type = c("deviceid", "integer"))
  x <- tibble::tibble(deviceid = c("353456789012345", "353456789012346"), n = c("1", "2"))
  expect_identical(kobo_type_cols_(x, form), "ci")
  res <- type_convert_(x, form)
  expect_identical(res$deviceid, x$deviceid)
})

test_that("deployed versions come from the asset when it lists them all", {
  asset <- list(uid = "aFakeFakeFakeFakeFakeF", deployed_versions = list(
    count = 2L, `next` = NULL,
    results = list(list(uid = "vNEW"), list(uid = "vOLD"))
  ))
  local_mocked_bindings(kobo_asset_version_list = function(x) {
    tibble::tibble(uid = c("vNEWER", "vNEW", "vOLD"), deployed = TRUE)
  })
  expect_identical(deployed_version_uids_(asset, "vOLD"), c("vNEW", "vOLD"))
  expect_identical(deployed_version_uids_(asset, "vNEWER"), c("vNEWER", "vNEW", "vOLD"))
  asset$deployed_versions$count <- 150L
  expect_identical(deployed_version_uids_(asset, "vOLD"), c("vNEWER", "vNEW", "vOLD"))
  asset$deployed_versions <- NULL
  expect_identical(deployed_version_uids_(asset, "vOLD"), c("vNEWER", "vNEW", "vOLD"))
})
