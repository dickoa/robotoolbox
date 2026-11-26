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

test_that("audit timestamps keep their milliseconds as 64-bit integers", {
  ## 1700000000000 ms is 2023-11-14 22:13:20 UTC
  with_end <- paste(
    "event,node,start,end",
    "question,/data/q1,1700000000000,1700000005250",
    sep = "\n"
  )
  no_end <- paste("event,node,start,end", "form start,,1700000001000,", sep = "\n")
  meta <- tibble::tibble(`_id` = 1:2, download_url = c("u1", "u2"))
  expect_no_warning(res <- parse_audit_(meta, list(with_end, no_end)))
  expect_identical(lapply(res, class), lapply(empty_audit_(), class))
  expect_s3_class(res$start_int, "integer64")
  expect_identical(as.character(res$start_int), c("1700000000000", "1700000001000"))
  expect_identical(as.character(res$end_int), c("1700000005250", NA))
  expect_identical(
    format(res$start, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    c("2023-11-14 22:13:20", "2023-11-14 22:13:21")
  )
  expect_identical(as.numeric(res$start), c(1700000000, 1700000001))
  expect_identical(as.numeric(res$end), c(1700000005.25, NA))
  expect_identical(empty_audit_()$start_int, bit64::integer64())
})

test_that("an invalid byte in an audit file does not discard the file", {
  bytes <- c(charToRaw("event,node,start,end,old-value\nquestion,/data/q1,1700000000000,1700000001000,caf"),
             as.raw(0xff), charToRaw("\n"))
  txt <- audit_text_(bytes, 1L)
  expect_false(is.na(txt))
  expect_identical(audit_text_(charToRaw("caf\u00e9"), 1L), "caf\u00e9")
  expect_identical(audit_text_(raw(), 1L), "")
  res <- parse_audit_(tibble::tibble(`_id` = 7L, download_url = "u"), list(txt))
  expect_identical(res$`_id`, 7L)
  expect_identical(res$name, "q1")
  expect_identical(res$`old-value`, "caf<ff>")
})

test_that("a short row of an audit file does not hide the rows after it", {
  meta <- tibble::tibble(`_id` = c(11L, 12L), download_url = c("u1", "u2"))
  short_row <- paste(c(
    "event,node,start,end",
    "question,/form/age,1700000000000,1700000000010",
    "question,/form/name,1700000000011",
    "question,/form/city,1700000000020,1700000000025"
  ), collapse = "\n")
  valid <- "event,node,start,end\nquestion,/form/q,1700000000000,1700000000010"
  expect_no_warning(res <- parse_audit_(meta, list(short_row, valid)))
  expect_identical(res$node, c("/form/age", "/form/name", "/form/city", "/form/q"))
  expect_identical(res$`_id`, c(11L, 11L, 11L, 12L))
  expect_identical(as.character(res$end_int),
                   c("1700000000010", NA, "1700000000025", "1700000000010"))
  expect_identical(lapply(res, class), lapply(empty_audit_(), class))

  ## A file that cannot be read cleanly is reported with its submission
  long_row <- paste(c(
    "event,node,start,end",
    "question,/form/age,1700000000000,1700000000010,extra,more"
  ), collapse = "\n")
  expect_warning(res <- parse_audit_(meta[2, ], list(long_row)), "submission 12")
  expect_identical(res$node, "/form/age")
  ## A file without the audit columns is left out
  expect_warning(res <- parse_audit_(meta[2, ], list("a,b\n1,2")), "submission 12")
  expect_identical(res, empty_audit_())
  long_row <- paste(c(long_row, "question,/form/b,1700000000020,1700000000030"),
                    collapse = "\n")
  expect_warning(res <- parse_audit_(meta[2, ], list(long_row)), "submission 12")
  expect_identical(res$node, c("/form/age", "/form/b"))
  expect_named(res, names(empty_audit_()))
})
