# tests/testthat/test-cache.R

test_that("kobo_cache_clear works with empty cache", {
  kobo_cache_clear()
  info <- kobo_cache_info()
  expect_equal(info$n_forms, 0)
})

test_that("cache set and get work for forms", {
  kobo_cache_clear()

  fake_form <- data.frame(name = "test", type = "text")
  set_cached_form_("uid123", "v1", fake_form)

  result <- get_cached_form_("uid123", "v1")
  expect_equal(result, fake_form)

  expect_true(has_cached_form_("uid123", "v1"))
  expect_false(has_cached_form_("uid123", "v2"))
  expect_false(has_cached_form_("other", "v1"))

  kobo_cache_clear()
})

test_that("kobo_cache_clear with uid only clears that asset", {
  kobo_cache_clear()

  set_cached_form_("uid1", "v1", "form1")
  set_cached_form_("uid1", "v2", "form2")
  set_cached_form_("uid2", "v1", "form3")

  kobo_cache_clear("uid1")

  expect_null(get_cached_form_("uid1", "v1"))
  expect_null(get_cached_form_("uid1", "v2"))

  expect_equal(get_cached_form_("uid2", "v1"), "form3")

  kobo_cache_clear()
})

test_that("kobo_cache_info returns correct stats", {
  kobo_cache_clear()

  set_cached_form_("a", "1", "f1")
  set_cached_form_("a", "2", "f2")
  set_cached_form_("b", "1", "f3")

  info <- kobo_cache_info()
  expect_equal(info$n_forms, 3)
  expect_true("a_1" %in% info$form_keys)

  kobo_cache_clear()
})

test_that("NULL version uses 'latest' key", {
  kobo_cache_clear()

  set_cached_form_("uid", NULL, "default_form")
  expect_true(has_cached_form_("uid", NULL))
  expect_equal(get_cached_form_("uid", NULL), "default_form")

  info <- kobo_cache_info()
  expect_true("uid_latest" %in% info$form_keys)

  kobo_cache_clear()
})
