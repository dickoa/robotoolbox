test_that("kobo_asset_list works", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  vcr::use_cassette("kobo_asset_list", {
    al <- kobo_asset_list()
  })
  expect_equal(class(al),
                         c("tbl_df", "tbl", "data.frame"))

  cn <- c("uid", "name", "asset_type", "owner_username", "date_created",
          "date_modified", "deployed", "submissions", "created_by")

  expect_equal(names(al), cn)
})

test_that("kobo_asset_list works even if the number of assets exceed limit", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  vcr::use_cassette("kobo_asset_list_limit", {
    al <- kobo_asset_list(limit = 5)
  })
  expect_equal(class(al),
               c("tbl_df", "tbl", "data.frame"))
  expect_equal(nrow(al), 5L)
})

test_that("kobo_asset_list can use the minimal listing", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url, token = token)
  vcr::use_cassette("kobo_asset_list_minimal", {
    al <- kobo_asset_list(limit = 1000)
    ml <- kobo_asset_list(limit = 1000, minimal = TRUE)
  })
  expect_equal(names(ml), c("uid", "name", "deployment_status"))
  expect_equal(ml$uid, al$uid)
  expect_true(all(ml$deployment_status %in% c("draft", "deployed", "archived", "-")))
})

## Offline tests with a fake listing endpoint

fake_assets <- function(n) {
  data.frame(
    uid = sprintf("aFakeAsset%012d", seq_len(n)),
    name = sprintf("Form %d", seq_len(n)),
    asset_type = rep("survey", n),
    owner__username = rep("user", n),
    date_created = rep("2026-01-01T00:00:00Z", n),
    date_modified = rep("2026-01-02T00:00:00Z", n),
    deployment__active = rep(TRUE, n),
    deployment__submission_count = rep(1L, n),
    deployment_status = rep("deployed", n),
    created_by = rep("user", n)
  )
}

## Serves `assets` page by page. `minimal` is FALSE for a server without the
## minimal listing. With `overlap`, each later page starts with the last
## `overlap` assets of the previous page instead of its own, as tied rows do.
fake_listing <- function(assets, minimal = TRUE, overlap = 0L) {
  calls <- new.env()
  calls$args <- list()
  calls$paths <- character()
  xget <- function(path, args = list(), ...) {
    calls$paths <- c(calls$paths, path)
    calls$args[[length(calls$args) + 1L]] <- args
    is_minimal <- grepl("minimal-list", path)
    if (is_minimal && !minimal) {
      abort("Not found.", class = "robotoolbox_http_404")
    }
    start <- as.integer(args$start %||% "0")
    limit <- as.integer(args$limit)
    idx <- seq_len(nrow(assets))
    idx <- idx[idx > start & idx <= start + limit]
    if (start > 0 && overlap > 0) {
      idx[seq_len(overlap)] <- start - rev(seq_len(overlap)) + 1L
    }
    page <- assets[idx, , drop = FALSE]
    if (is_minimal) {
      page <- page[c("uid", "name", "deployment_status")]
    }
    value <- function(v) {
      if (is.character(v)) sprintf('"%s"', v) else tolower(as.character(v))
    }
    rows <- vapply(seq_len(nrow(page)), function(i) {
      fields <- vapply(page[i, ], value, "")
      paste0("{", paste0('"', names(page), '": ', fields, collapse = ", "), "}")
    }, "")
    sprintf(
      '{%s"next": %s, "results": [%s]}',
      if (is_minimal) "" else sprintf('"count": %d, ', nrow(assets)),
      if (start + limit < nrow(assets)) '"next"' else "null",
      paste(rows, collapse = ", ")
    )
  }
  list(xget = xget, calls = calls)
}

test_that("kobo_asset_list(minimal = TRUE) uses the minimal listing", {
  srv <- fake_listing(fake_assets(5))
  local_mocked_bindings(xget = srv$xget)
  res <- kobo_asset_list(minimal = TRUE)
  expect_equal(names(res), c("uid", "name", "deployment_status"))
  expect_equal(res$uid, fake_assets(5)$uid)
  expect_true(all(grepl("minimal-list", srv$calls$paths)))
  expect_equal(srv$calls$args[[1]]$ordering, asset_ordering_)
})

test_that("kobo_asset_list respects limit without a count from the server", {
  srv <- fake_listing(fake_assets(2500))
  local_mocked_bindings(xget = srv$xget)
  res <- kobo_asset_list(limit = 1200, minimal = TRUE)
  expect_equal(nrow(res), 1200L)
  expect_equal(vapply(srv$calls$args, `[[`, "", "limit"), c("1000", "200"))
  res <- kobo_asset_list(limit = 1e4, minimal = TRUE)
  expect_equal(nrow(res), 2500L)
})

test_that("kobo_asset_list(minimal = TRUE) falls back on servers without it", {
  srv <- fake_listing(fake_assets(3), minimal = FALSE)
  local_mocked_bindings(xget = srv$xget)
  expect_message(
    res <- kobo_asset_list(minimal = TRUE),
    "minimal asset listing"
  )
  expect_equal(names(res), c("uid", "name", "deployment_status"))
  expect_equal(res$uid, fake_assets(3)$uid)
  expect_equal(srv$calls$paths, c("api/v2/assets/minimal-list/", "api/v2/assets/"))
})

test_that("kobo_asset_list(minimal = TRUE) does not hide other errors", {
  srv <- fake_listing(fake_assets(3))
  local_mocked_bindings(xget = function(path, ...) {
    if (grepl("minimal-list", path)) {
      abort("Server error", class = "robotoolbox_http_500")
    }
    srv$xget(path, ...)
  })
  expect_error(kobo_asset_list(minimal = TRUE), "Server error")
})

test_that("kobo_asset_list drops duplicates and warns when assets are missing", {
  srv <- fake_listing(fake_assets(2500), overlap = 3L)
  local_mocked_bindings(xget = srv$xget)
  expect_warning(
    res <- kobo_asset_list(limit = 5000),
    "listed 2494 of 2500 assets"
  )
  expect_false(anyDuplicated(res$uid) > 0)
  expect_silent(res <- kobo_asset_list(limit = 5000, minimal = TRUE))
  expect_false(anyDuplicated(res$uid) > 0)
})

test_that("kobo_asset_list returns typed empty tables", {
  srv <- fake_listing(fake_assets(0))
  local_mocked_bindings(xget = srv$xget)
  res <- kobo_asset_list()
  expect_equal(nrow(res), 0L)
  expect_equal(
    names(res),
    c("uid", "name", "asset_type", "owner_username", "date_created",
      "date_modified", "deployed", "submissions", "created_by")
  )
  res <- kobo_asset_list(minimal = TRUE)
  expect_equal(names(res), c("uid", "name", "deployment_status"))
  expect_equal(nrow(res), 0L)
})

test_that("kobo_asset_list validates limit", {
  expect_error(kobo_asset_list(limit = 0), "positive integer")
  expect_error(kobo_asset_list(limit = "10"), "positive integer")
  expect_error(kobo_asset_list(limit = c(1, 2)), "positive integer")
})
