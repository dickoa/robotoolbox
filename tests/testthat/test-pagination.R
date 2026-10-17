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
