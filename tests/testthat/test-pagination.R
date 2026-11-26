test_that("plan_missing_pages_ fills gaps left by short pages", {
  res <- plan_missing_pages_(start = 0, got = 1000, total = 2217, chunk = 1000)
  expect_equal(res$start, c(1000, 2000))
  expect_equal(res$limit, c(1000, 217))

  res <- plan_missing_pages_(
    start = c(0, 5000, 10000),
    got = c(1000, 1000, 1000),
    total = 12000,
    chunk = 1000
  )
  expect_equal(res$start, c(1000, 2000, 3000, 4000, 6000, 7000, 8000, 9000, 11000))
  expect_true(all(res$limit == 1000))
})

test_that("plan_missing_pages_ returns no page when nothing is missing", {
  expect_equal(nrow(plan_missing_pages_(0, 2217, 2217, 1000)), 0L)
  expect_equal(nrow(plan_missing_pages_(c(0, 1000, 2000), c(1000, 1000, 217), 2217, 1000)), 0L)
  expect_equal(nrow(plan_missing_pages_(0, 0, 0, 1000)), 0L)
  expect_equal(nrow(plan_missing_pages_(0, 0, 10, 0)), 0L)
})

test_that("plan_missing_pages_ handles pages given out of order", {
  res <- plan_missing_pages_(c(2000, 0, 1000), c(500, 1000, 1000), 2600, 500)
  expect_equal(res$start, c(2500))
  expect_equal(res$limit, c(100))
})

test_that("a single request above the server limit returns every submission", {
  server <- fake_server(total = 2217, cap = 1000)
  with_fake_server(server, {
    expect_message(
      res <- get_subs_pages_("aFake", size = 2217, page_size = 5000),
      "at most 1000"
    )
    expect_equal(res$`_id`, 1:2217)
    expect_equal(server$calls$n, 3L)
    expect_equal(page_cap_get_(), 1000)
  })
})

test_that("paginated requests above the server limit return every submission", {
  server <- fake_server(total = 12000, cap = 1000)
  with_fake_server(server, {
    expect_message(
      res <- get_subs_pages_("aFake", size = 12000, page_size = 5000)
    )
    expect_equal(res$`_id`, 1:12000)
    expect_equal(server$calls$n, 12L)
  })
})

test_that("no extra request is made when the page size fits the server", {
  server <- fake_server(total = 2217, cap = 1000)
  with_fake_server(server, {
    expect_no_message(
      res <- get_subs_pages_("aFake", size = 2217, page_size = 1000)
    )
    expect_equal(res$`_id`, 1:2217)
    expect_equal(server$calls$n, 3L)
    expect_length(server$calls$rounds, 1L)
    expect_equal(page_cap_get_(), Inf)
  })
})

test_that("a server without limit is served in a single request", {
  server <- fake_server(total = 6088)
  with_fake_server(server, {
    res <- get_subs_pages_("aFake", size = 6088, page_size = 30000)
    expect_equal(nrow(res), 6088L)
    expect_equal(server$calls$n, 1L)
  })
})

test_that("the detected limit is reused for the next requests", {
  server <- fake_server(total = 2217, cap = 1000)
  with_fake_server(server, {
    expect_message(get_subs_pages_("aFake", size = 2217, page_size = 5000))
    before <- server$calls$n
    expect_no_message(
      res <- get_subs_pages_("aFake", size = 2217, page_size = 5000)
    )
    expect_equal(nrow(res), 2217L)
    expect_equal(server$calls$n - before, 3L)
    expect_length(server$calls$rounds, 3L)
  })
})

test_that("submissions beyond an outdated submission count are fetched", {
  server <- fake_server(total = 2217, cap = 1000)
  with_fake_server(server, {
    expect_no_message(
      res <- get_subs_pages_("aFake", size = 2000, page_size = 1000)
    )
    expect_equal(res$`_id`, 1:2217)
    expect_equal(page_cap_get_(), Inf)
  })
})

test_that("get_subs_fields_ pages through every record and returns NULL when empty", {
  server <- fake_server(total = 2500, cap = 1000)
  with_fake_server(server, {
    res <- get_subs_fields_("aFake", '["_id"]')
    expect_equal(res$`_id`, 1:2500)
  })
  with_fake_server(fake_server(total = 0), {
    expect_null(get_subs_fields_("aFake", '["_id"]'))
  })
})

test_that("xget_all_ follows pages until the server reports no next page", {
  pages <- c(
    '{"count": 5, "next": "x", "results": [{"uid": "a"}, {"uid": "b"}]}',
    '{"count": 5, "next": "x", "results": [{"uid": "c"}, {"uid": "d"}]}',
    '{"count": 5, "next": null, "results": [{"uid": "e"}]}'
  )
  seen <- list()
  local_mocked_bindings(xget = function(path, args = list(), ...) {
    seen[[length(seen) + 1L]] <<- args
    start <- as.integer(args$start %||% "0")
    pages[start / 2 + 1]
  })
  res <- xget_all_("api/v2/assets/aFake/versions/", page_size = 2L)
  expect_equal(vapply(do.call(c, res), `[[`, "", "uid"), letters[1:5])
  expect_equal(vapply(seen, function(a) a$start %||% "0", ""), c("0", "2", "4"))

  seen <- list()
  xget_all_("api/v2/assets/", n_max = 3, page_size = 2L)
  expect_length(seen, 2L)
  expect_equal(vapply(seen, `[[`, "", "limit"), c("2", "1"))
})

## Capped counts (KoboToolbox KPI 2.026.43 and later)

test_that("parse_api_count_ reads numbers and treats capped counts as unknown", {
  expect_identical(parse_api_count_(2217L), 2217)
  expect_identical(parse_api_count_(2217), 2217)
  expect_identical(parse_api_count_("2217"), 2217)
  expect_identical(parse_api_count_("many"), NA_real_)
  expect_identical(parse_api_count_("unknown"), NA_real_)
  expect_identical(parse_api_count_(NULL), NA_real_)
  expect_identical(parse_api_count_(NA), NA_real_)
  expect_silent(parse_api_count_("many"))
})

test_that("parse_subs_page_ keeps whether the server has a next page", {
  res <- parse_subs_page_('{"count": "many", "next": "x", "results": [{"_id": 1}]}')
  expect_true(res$has_next)
  res <- parse_subs_page_('{"count": 1, "next": null, "results": [{"_id": 1}]}')
  expect_false(res$has_next)
})

test_that("every submission is fetched when the server does not report a count", {
  for (count in c("many", "unknown")) {
    server <- fake_server(total = 2500, cap = 1000, count = count)
    with_fake_server(server, {
      res <- get_subs_pages_("aFake", page_size = 1000)
    })
    expect_equal(res$`_id`, 1:2500, info = count)
    expect_equal(server$calls$n, 3L, info = count)
  }
})

test_that("the server limit is detected when the count is not reported", {
  server <- fake_server(total = 2500, cap = 1000, count = "many")
  with_fake_server(server, {
    expect_message(
      res <- get_subs_pages_("aFake", page_size = 5000),
      "at most 1000"
    )
    expect_equal(res$`_id`, 1:2500)
    expect_equal(page_cap_get_(), 1000)
  })
})

test_that("planned pages are completed when the count is not reported", {
  server <- fake_server(total = 2500, cap = 1000, count = "many")
  with_fake_server(server, {
    res <- get_subs_pages_("aFake", size = 2000, page_size = 1000)
  })
  expect_equal(res$`_id`, 1:2500)

  for (count in c("many", "unknown")) {
    server <- fake_server(total = 12000, cap = 1000, count = count)
    with_fake_server(server, {
      expect_message(
        res <- get_subs_pages_("aFake", size = 12000, page_size = 5000),
        "at most 1000"
      )
    })
    expect_equal(res$`_id`, 1:12000, info = count)
    expect_equal(server$calls$n, 12L, info = count)
  }

  server <- fake_server(total = 7500, cap = 1000, count = "many")
  with_fake_server(server, {
    expect_message(
      res <- get_subs_pages_("aFake", size = 20000, page_size = 5000),
      "at most 1000"
    )
  })
  expect_equal(res$`_id`, 1:7500)
})

test_that("get_filtered_count_ returns NA for a count the server did not compute", {
  for (count in c('"many"', '"unknown"')) {
    local_mocked_bindings(xget = function(path, args = list(), ...) {
      sprintf('{"count": %s, "next": "x", "results": []}', count)
    })
    expect_no_warning(n <- get_filtered_count_("aFake", '{"a": 1}'))
    expect_identical(n, NA_integer_, info = count)
  }
  local_mocked_bindings(xget = function(path, args = list(), ...) {
    '{"count": 12, "next": null, "results": []}'
  })
  expect_identical(get_filtered_count_("aFake", '{"a": 1}'), 12L)
})

## Failed async requests

## Stands for `async_send_()`. Each request of a batch gets the status of its
## position in the batch, and no request is sent. The batches are kept in
## `sent`.
fake_async_send <- function(status = 200L, sent = new.env(),
                            body = '{"detail": "Not found."}') {
  sent$batches <- list()
  function(reqs) {
    sent$reqs <- reqs
    sent$batches[[length(sent$batches) + 1L]] <- reqs
    list(
      status = rep_len(status, length(reqs)),
      content = rep(list(charToRaw(body)), length(reqs))
    )
  }
}

test_that("a failed async request aborts with the class of its HTTP status", {
  withr::local_envvar(KOBOTOOLBOX_URL = "https://fake.kobo.org")
  uid <- "aSCXrTdtrxHDnx3n4DHefK"
  urls <- sprintf("https://fake.kobo.org/file%d", 1:3)
  sites <- list(
    submissions = function() fetch_subs_async_(uid, c(0, 10, 20), c(10, 10, 10)),
    versions = function() fetch_asset_versions_async_(uid, c("v1", "v2", "v3")),
    audit = function() kobo_audit_(uid),
    attachments = function() {
      att <- tibble::tibble(`_id` = 1:3, download_url = urls,
                            uid = c("att1", "att2", "att3"),
                            media_file_basename = "a.jpg")
      kobo_attachment_download_(att, tempdir(), overwrite = TRUE, n_retry = 1L)
    }
  )
  local_mocked_bindings(
    get_audit_url_ = function(uid) tibble::tibble(`_id` = 1:3, download_url = urls)
  )
  checked <- character()
  for (site in names(sites)) {
    ## The first failed request gives the class
    for (status in list(c(200L, 404L, 500L), c(200L, 200L, 500L))) {
      local_mocked_bindings(async_send_ = fake_async_send(status))
      first <- status[status >= 300L][1]
      cnd <- expect_error(sites[[site]](), class = paste0("robotoolbox_http_", first))
      expect_false(inherits(cnd, paste0("robotoolbox_http_", setdiff(c(404L, 500L), first))))
      checked <- c(checked, paste(site, first))
    }
  }
  expect_setequal(checked, c(outer(names(sites), c(404, 500), paste)))
})

test_that("a 404 of the data endpoint is explained on a pull of several pages", {
  local_mocked_bindings(async_send_ = fake_async_send(404L))
  asset <- asset_ml
  asset$deployment__submission_count <- 30L
  withr::local_envvar(KOBOTOOLBOX_URL = "https://fake.kobo.org")
  expect_error(
    kobo_data(asset, page_size = 10),
    "The submissions of this project are not available"
  )
})

## Trailing slashes of the server URL

test_that("request URLs have no double slash when the server URL ends with slashes", {
  uid <- "aSCXrTdtrxHDnx3n4DHefK"
  expect_identical(
    clean_urls(c("https://k.org", "https://k.org/", "https://k.org//")),
    rep("https://k.org", 3)
  )
  for (url in c("https://k.org", "https://k.org/", "https://k.org//")) {
    withr::local_envvar(KOBOTOOLBOX_URL = url)
    expect_identical(server_url_(), "https://k.org")
    expect_identical(cache_server_(), "https://k.org")
    sent <- new.env()
    local_mocked_bindings(async_send_ = fake_async_send(404L, sent))
    expect_error(fetch_subs_async_(uid, c(0, 10), c(10, 10)))
    expect_identical(
      sent$reqs[[1]]$url,
      paste0("https://k.org/api/v2/assets/", uid, "/data.json")
    )
    expect_error(fetch_asset_versions_async_(uid, "vAAAAAAAAAAAAAAAAAAAAA"))
    expect_identical(
      sent$reqs[[1]]$url,
      paste0("https://k.org/", asset_path_(uid, "vAAAAAAAAAAAAAAAAAAAAA"))
    )
  }
})

## Batches of async requests

test_that("async requests are sent in batches and handled in order", {
  checked <- integer()
  for (n in c(0L, 1L, 15L, 16L, 31L)) {
    sent <- new.env()
    local_mocked_bindings(async_send_ = function(reqs) {
      sent$batches[[length(sent$batches) + 1L]] <- unlist(reqs)
      list(status = rep(200L, length(reqs)), content = as.list(unlist(reqs) * 10))
    })
    handled <- integer()
    ticks <- list()
    res <- async_fetch_batches_(
      as.list(seq_len(n)),
      handle = function(content, i) {
        handled <<- c(handled, i)
        content + i
      },
      sleep = 0,
      tick = function(batch) ticks[[length(ticks) + 1L]] <<- unlist(batch)
    )
    expected <- split(seq_len(n), ceiling(seq_len(n) / 15))
    expect_identical(unname(sent$batches %||% list()), unname(lapply(expected, as.integer)))
    expect_identical(res, as.list(seq_len(n) * 11))
    expect_identical(handled, seq_len(n))
    expect_identical(ticks, unname(lapply(expected, function(i) i * 11)))
    checked <- c(checked, n)
  }
  expect_identical(checked, c(0L, 1L, 15L, 16L, 31L))
  ## Without `handle` the contents are returned
  expect_identical(async_fetch_batches_(list(1, 2), sleep = 0), list(10, 20))
})

test_that("a failed batch stops the download before the later batches", {
  for (status in c(404L, 500L, 0L)) {
    sent <- new.env()
    local_mocked_bindings(async_send_ = function(reqs) {
      sent$n <- c(sent$n, length(reqs))
      ok <- length(sent$n) == 1L
      st <- rep(200L, length(reqs))
      if (!ok) st[2] <- status
      list(status = st, content = rep(list(charToRaw('{"detail": "x"}')), length(reqs)))
    })
    handled <- 0L
    expect_error(
      async_fetch_batches_(as.list(1:40), handle = function(content, i) handled <<- handled + 1L,
                           sleep = 0),
      class = paste0("robotoolbox_http_", status)
    )
    expect_identical(sent$n, c(15L, 15L))
    expect_identical(handled, 15L)
  }
})

test_that("the pages are the same with and without a progress callback", {
  rows <- function(ids) data.frame(`_id` = ids, v = ids * 2, check.names = FALSE)
  cases <- list(
    known = list(size = 25, server = function() fake_server(25, rows = rows)),
    outdated = list(size = 10, server = function() fake_server(25, rows = rows)),
    capped = list(size = 25, server = function() fake_server(25, cap = 4, rows = rows)),
    many = list(size = NA, server = function() fake_server(25, rows = rows, count = "many"))
  )
  checked <- character()
  for (case in names(cases)) {
    get <- function(tick) {
      server <- cases[[case]]$server()
      res <- NULL
      with_fake_server(server, {
        suppressMessages(res <- get_subs_pages_("aSCXrTdtrxHDnx3n4DHefK", cases[[case]]$size,
                                                page_size = 10, tick = tick))
      })
      list(res = res, rounds = length(server$calls$rounds))
    }
    seen <- 0
    ticks <- 0L
    plain <- get(NULL)
    with_tick <- get(function(pages) {
      ticks <<- ticks + 1L
      seen <<- seen + n_subs_(pages)
    })
    expect_identical(with_tick$res, plain$res, info = case)
    expect_identical(as.vector(plain$res$`_id`), 1:25, info = case)
    expect_identical(seen, 25, info = case)
    expect_identical(ticks, with_tick$rounds, info = case)
    checked <- c(checked, case)
  }
  expect_setequal(checked, c("known", "outdated", "capped", "many"))
})

test_that("a progress bar accepts more units than its total", {
  f <- function() {
    tick <- progress_ticker_("Downloading", total = 3, size = length)
    tick(1:2)
    tick(1:5)
    tick(1)
    progress_done_(tick)
    tick <- progress_ticker_("Downloading", total = NA)
    tick(1:2)
    progress_done_(tick)
    progress_done_(NULL)
    TRUE
  }
  expect_no_error(expect_true(f()))
  withr::local_options(cli.dynamic = TRUE, cli.progress_show_after = 0)
  expect_no_error(suppressMessages(expect_true(f())))
})

test_that("a progress bar reaches its total before it counts past it", {
  withr::local_options(cli.dynamic = TRUE, cli.progress_show_after = 0,
                       cli.ansi = FALSE, cli.width = 70)
  frames <- function(sizes) {
    out <- character()
    f <- function() {
      tick <- progress_ticker_("Downloading", total = 20, size = length)
      for (n in sizes) tick(seq_len(n))
      progress_done_(tick)
    }
    withCallingHandlers(f(), message = function(m) {
      out <<- c(out, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
    paste(out, collapse = "")
  }
  exact <- frames(c(10, 10))
  expect_match(exact, "50%")
  expect_match(exact, "100%")
  expect_no_match(exact, "done")
  over <- frames(c(10, 10, 5))
  expect_match(over, "100%")
  expect_match(over, "25 done")
})
