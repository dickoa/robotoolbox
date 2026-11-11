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

## Parsing audit files

test_that("parse_audit_ reads files with and without the optional columns", {
  full <- paste(
    "event,node,start,end,latitude,longitude,accuracy,old-value,new-value,user,change-reason",
    "question,/data/q1,1700000000000,1700000005000,14.7,-17.4,5,,a,me,",
    sep = "\n"
  )
  short <- paste(
    "event,node,start,end",
    "form start,,1700000000000,",
    "question,/data/q2,1700000001000,1700000002000",
    sep = "\n"
  )
  meta <- tibble::tibble(`_id` = 1:4, download_url = paste0("u", 1:4))
  res <- parse_audit_(meta, list(full, short, "event,node,start,end", ""))
  expect_named(res, names(empty_audit_()))
  expect_identical(res$`_id`, c(1L, 2L, 2L))
  expect_identical(res$name, c("q1", "", "q2"))
  expect_identical(res$latitude, c(14.7, NA, NA))
  expect_identical(res$user, c("me", NA, NA))
  expect_s3_class(res$start, "POSIXct")
  expect_identical(parse_audit_(meta[4, ], list("")), empty_audit_())
})
