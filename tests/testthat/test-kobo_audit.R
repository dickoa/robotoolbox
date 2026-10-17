test_that("with kobo_audit you can read your audit log data", {
  skip_on_cran()
  skip_if(!has_live_api(), message = "Requires live API (async requests)")
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aKQB8xLBd3nsJ7EZQmQhZd"
  asset <- kobo_asset(uid)
  raw_uid <- kobo_audit(uid)
  raw_asset <- kobo_audit(asset)
  expect_equal(raw_uid, raw_asset)
  expect_equal(class(raw_uid),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(lapply(raw_uid, class), lapply(empty_audit_(), class))
  expect_error(kobo_audit(1L))
  expect_error(kobo_audit("bad_uid"))

  uid_no_audit <- "aYuTZn9vegi3Z49MXwKjep"
  asset_no_audit <- kobo_asset(uid_no_audit)
  expect_error(kobo_audit(uid_no_audit))

  uid_no_audit_no_version <- "aLPEprhNHZCRzSMJiPXxj7"
  expect_error(kobo_audit(uid_no_audit_no_version))
})

test_that("kobo_audit has a progress parameter", {
  skip_on_cran()
  skip_if(!has_live_api(), message = "Requires live API (async requests)")
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  uid <- "aKQB8xLBd3nsJ7EZQmQhZd"

  expect_message(kobo_audit(uid, progress = TRUE), "Checking audit data availability")
})

## Submissions without audit logs

test_that("kobo_audit returns an empty table when no submission has an audit log", {
  on.exit(kobo_cache_clear(asset_audit$uid))
  photo <- data.frame(download_url = "https://fake.kobo.org/photo",
                      media_file_basename = "photo.jpg")
  cases <- list(
    none = function(ids) list(NULL, NULL)[ids],
    photo = function(ids) list(NULL, photo)[ids]
  )
  for (case in names(cases)) {
    rows <- function(ids) {
      res <- data.frame(`_id` = ids, check.names = FALSE)
      res$`_attachments` <- cases[[case]](ids)
      res
    }
    with_fake_server(fake_server(total = 2, rows = rows), {
      res <- kobo_audit(asset_audit)
    }, version = asset_audit$deployed_version_id)
    expect_identical(res, empty_audit_(), info = case)
  }
  with_fake_server(fake_server(total = 0), {
    res <- kobo_audit(asset_audit)
  }, version = asset_audit$deployed_version_id)
  expect_identical(res, empty_audit_())
})
