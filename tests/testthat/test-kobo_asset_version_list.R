test_that("kobo_asset_version_list works", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aANhxwX9S6BCsiYMgQj9kV"
  vcr::use_cassette("kobo_asset_version_list", {
    al1 <- kobo_asset_version_list(uid)
    al2 <- kobo_asset_version_list(kobo_asset(uid))
  })

  expect_equal(al1, al2)
  expect_error(kobo_asset_version_list(1L))
  expect_error(kobo_asset_version_list("bad_uid"))
  expect_s3_class(al1, class = c("tbl_df", "tbl", "data.frame"))
  expect_s3_class(al2, class = c("tbl_df", "tbl", "data.frame"))

  cn <- c("uid", "url", "deployed", "date_modified", "version_number")
  expect_equal(names(al1), cn)
  expect_type(al1$version_number, "character")
})

test_that("kobo_asset_version_list reads version_number when the server sends it", {
  page <- '{"count": 3, "next": null, "results": [
    {"uid": "vCCCCCCCCCCCCCCCCCCCCC", "url": "u3", "version_number": "2.1",
     "date_deployed": false, "date_modified": "2026-10-16T10:09:00.000000Z"},
    {"uid": "vBBBBBBBBBBBBBBBBBBBBB", "url": "u2", "version_number": "2",
     "date_deployed": "2026-10-16T10:08:09.948454Z",
     "date_modified": "2026-10-16T10:08:09.948454Z"},
    {"uid": "vAAAAAAAAAAAAAAAAAAAAA", "url": "u1", "version_number": null,
     "date_deployed": "2026-10-16T10:07:50.483325Z",
     "date_modified": "2026-10-16T10:07:50.483325Z"}
  ]}'
  local_mocked_bindings(xget = function(path, args = list(), ...) page)
  res <- kobo_asset_version_list("aFakeFakeFakeFakeFakeF")
  expect_identical(res$version_number, c("2.1", "2", NA))
  expect_identical(res$deployed, c(FALSE, TRUE, TRUE))
})

test_that("kobo_asset_version_list gives NA version_number on older servers", {
  page <- '{"count": 1, "next": null, "results": [
    {"uid": "vAAAAAAAAAAAAAAAAAAAAA", "url": "u1",
     "date_deployed": "2022-01-06T15:14:19.920643Z",
     "date_modified": "2022-01-06T15:14:20.000000Z"}
  ]}'
  local_mocked_bindings(xget = function(path, args = list(), ...) page)
  res <- kobo_asset_version_list("aFakeFakeFakeFakeFakeF")
  expect_identical(res$version_number, NA_character_)
  expect_identical(res$deployed, TRUE)
})

test_that("kobo_asset_version_list keeps its columns when there is no version", {
  page <- '{"count": 0, "next": null, "results": []}'
  local_mocked_bindings(xget = function(path, args = list(), ...) page)
  res <- kobo_asset_version_list("aFakeFakeFakeFakeFakeF")
  expect_identical(
    names(res),
    c("uid", "url", "deployed", "date_modified", "version_number")
  )
  expect_identical(nrow(res), 0L)
})

test_that("parse_kobo_datetime converts time zone offsets to UTC", {
  x <- c(
    "2024-01-01T10:00:00",
    "2024-01-01T10:00:00Z",
    "2024-01-01T10:00:00.123456Z",
    "2024-01-01T10:00:00+00:00",
    "2024-01-01T10:00:00+02:00",
    "2024-01-01T10:00:00.5-05:00",
    NA
  )
  expect_identical(
    parse_kobo_datetime(x),
    c("2024-01-01 10:00:00", "2024-01-01 10:00:00", "2024-01-01 10:00:00",
      "2024-01-01 10:00:00", "2024-01-01 08:00:00", "2024-01-01 15:00:01", NA)
  )
})

test_that("a version without deployment date is not reported as deployed", {
  expect_identical(
    is_deployed_version_(c("2024-01-01T10:00:00Z", "FALSE", "false", NA, "")),
    c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_false(is_deployed_version_(NULL))
  page <- '{"count": 2, "next": null, "results": [
    {"uid": "vBBBBBBBBBBBBBBBBBBBBB", "url": "u2", "date_deployed": null,
     "date_modified": "2026-10-16T10:08:09Z"},
    {"uid": "vAAAAAAAAAAAAAAAAAAAAA", "url": "u1",
     "date_modified": "2026-10-16T10:07:50Z"}
  ]}'
  local_mocked_bindings(xget = function(path, args = list(), ...) page)
  res <- kobo_asset_version_list("aFakeFakeFakeFakeFakeF")
  expect_identical(res$deployed, c(FALSE, FALSE))
  v <- structure(list(uid = "vA", asset_uid = "aA", date_modified = "2026-10-16T10:07:50Z"),
                 class = "kobo_asset_version")
  expect_output(print(v), "Asset deployed: FALSE")
})
