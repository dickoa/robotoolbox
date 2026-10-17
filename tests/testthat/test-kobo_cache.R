test_that("kobo_cache_clear works with empty cache", {
  kobo_cache_clear()
  info <- kobo_cache_info()
  expect_equal(info$n_forms, 0)
  expect_true(kobo_cache_clear("aEmptyCache000000000000"))
  expect_true(kobo_cache_clear("aEmptyCache000000000000", type = "langs"))
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
  set_cached_lang_("uid1", "v1", "English")
  set_cached_lang_("uid2", "v1", "French")

  kobo_cache_clear("uid1")

  expect_null(get_cached_form_("uid1", "v1"))
  expect_null(get_cached_form_("uid1", "v2"))
  expect_false(has_cached_lang_("uid1", "v1"))

  expect_equal(get_cached_form_("uid2", "v1"), "form3")
  expect_equal(get_cached_lang_("uid2", "v1"), "French")

  kobo_cache_clear()
})

with_server <- function(url, code) {
  old <- Sys.getenv("KOBOTOOLBOX_URL")
  Sys.setenv(KOBOTOOLBOX_URL = url)
  on.exit(Sys.setenv(KOBOTOOLBOX_URL = old))
  code
}

test_that("kobo_cache_info returns correct stats", {
  kobo_cache_clear()
  old <- Sys.getenv("KOBOTOOLBOX_URL")
  Sys.setenv(KOBOTOOLBOX_URL = "https://kobo.example.org/")
  on.exit(Sys.setenv(KOBOTOOLBOX_URL = old))

  set_cached_form_("a", "1", "f1")
  set_cached_form_("a", "2", "f2")
  set_cached_form_("b", "1", "f3")

  info <- kobo_cache_info()
  expect_equal(info$n_forms, 3)
  expect_true("https://kobo.example.org|a|1" %in% info$form_keys)

  kobo_cache_clear()
})

## Cache keys follow the server and the form version

cache_asset <- function(question = "q1", version = "v1", hash = "h1",
                        deployed = version, deployed_hash = hash,
                        lang = "English") {
  structure(
    list(
      uid = "aCacheKeyTest0000000000",
      asset_type = "survey",
      deployment__submission_count = 1L,
      version_id = version,
      version__content_hash = hash,
      deployed_version_id = deployed,
      deployed_versions = list(
        count = 1L,
        results = list(list(uid = deployed, content_hash = deployed_hash))
      ),
      content = list(
        survey = list(list(type = "text", name = question,
                           label = list(toupper(question)))),
        translations = list(lang)
      )
    ),
    class = "kobo_asset"
  )
}

version_of <- function(question, lang = "English") {
  structure(
    list(content = list(
      survey = list(list(type = "text", name = question,
                         label = list(toupper(question)))),
      translations = list(lang)
    )),
    class = "kobo_asset_version"
  )
}

test_that("a redeployed form is not served from the cache", {
  kobo_cache_clear()
  on.exit(kobo_cache_clear())

  expect_equal(kobo_form(cache_asset("q1"))$name, "q1")
  expect_equal(kobo_lang(cache_asset("q1")), "English")

  new <- cache_asset("q2", version = "v2", hash = "h2", lang = "French")
  expect_equal(kobo_form(new)$name, "q2")
  expect_equal(kobo_lang(new), "French")
  expect_equal(kobo_form(cache_asset("q1"))$name, "q1")
})

test_that("the same uid on two servers gives two cache entries", {
  kobo_cache_clear()
  on.exit(kobo_cache_clear())

  with_server("https://kobo.one.org", {
    expect_equal(kobo_form(cache_asset("q1"))$name, "q1")
  })
  with_server("https://kobo.two.org", {
    other <- cache_asset("other", lang = "French")
    expect_equal(kobo_form(other)$name, "other")
    expect_equal(kobo_lang(other), "French")
  })
  with_server("https://kobo.one.org/", {
    expect_equal(kobo_form(cache_asset("other"))$name, "q1")
    expect_equal(kobo_lang(cache_asset("other", lang = "French")), "English")
  })
})

test_that("kobo_form and kobo_lang use the deployed version, not the draft", {
  kobo_cache_clear()
  on.exit(kobo_cache_clear())
  asset <- cache_asset("draft", version = "v2", hash = "h2",
                       deployed = "v1", deployed_hash = "h1",
                       lang = "Draft language")
  fetched <- character()
  local_mocked_bindings(kobo_asset_version = function(x, version) {
    fetched <<- c(fetched, version)
    version_of("deployed", lang = "English")
  })

  expect_equal(kobo_lang(asset), "English")
  form <- kobo_form(asset)
  expect_equal(form$name, "deployed")
  expect_equal(unique(form$version), "v1")
  expect_equal(kobo_form(asset, "v2")$name, "draft")
  expect_equal(fetched, c("v1", "v1"))

  expect_equal(kobo_form(asset)$name, "deployed")
  expect_equal(kobo_lang(asset), "English")
  expect_length(fetched, 2)
})

test_that("a draft with the deployed content needs no request", {
  kobo_cache_clear()
  on.exit(kobo_cache_clear())
  asset <- cache_asset("q1", version = "v2", hash = "h1",
                       deployed = "v1", deployed_hash = "h1")
  local_mocked_bindings(kobo_asset_version = function(...) {
    stop("no request expected")
  })
  expect_equal(kobo_form(asset)$name, "q1")
  expect_equal(unique(kobo_form(asset)$version), "v1")
  expect_equal(kobo_lang(asset), "English")
})

test_that("a deployed version without question names falls back to the asset", {
  kobo_cache_clear()
  on.exit(kobo_cache_clear())
  asset <- cache_asset("q1", version = "v2", hash = "h2",
                       deployed = "v1", deployed_hash = "h1")
  local_mocked_bindings(kobo_asset_version = function(x, version) {
    res <- version_of("q1")
    res$content$survey[[1]]$name <- NULL
    res
  })
  expect_equal(kobo_form(asset)$name, "q1")
})

test_that("a form never deployed uses its draft", {
  kobo_cache_clear()
  on.exit(kobo_cache_clear())
  asset <- cache_asset("q1")
  asset$deployed_version_id <- NULL
  asset$deployed_versions <- list(count = 0L, results = list())
  local_mocked_bindings(kobo_asset_version = function(...) {
    stop("no request expected")
  })
  expect_equal(unique(kobo_form(asset)$version), "v1")
})

test_that("kobo_data picks up a repeat group added by a redeploy", {
  kobo_cache_clear()
  on.exit(kobo_cache_clear())
  q <- function(type, name) list(type = type, name = name, label = list(name))
  v1 <- cache_asset("hh")
  v2 <- cache_asset("hh", version = "v2", hash = "h2")
  v2$content$survey <- list(
    q("text", "hh"), q("text", "phone"),
    q("begin_repeat", "member"), q("text", "age"), list(type = "end_repeat")
  )
  rows <- function(version) {
    function(ids) {
      res <- data.frame(`_id` = ids, `__version__` = version, hh = "A",
                        check.names = FALSE)
      if (version == "v2") {
        res$phone <- "123"
        res$member <- list(data.frame(`member/age` = c(30L, 5L),
                                      check.names = FALSE))
      }
      res
    }
  }
  with_fake_server(fake_server(total = 1, rows = rows("v1")), {
    old <- kobo_data(v1)
  }, version = "v1")
  with_fake_server(fake_server(total = 1, rows = rows("v2")), {
    new <- kobo_data(v2)
  }, version = "v2")

  expect_false(inherits(old, "dm"))
  expect_s3_class(new, "dm")
  expect_equal(names(new), c("main", "member"))
  expect_equal(as.vector(new$member$age), c(30L, 5L))
  expect_equal(labelled::var_label(new$main$phone), "phone")
})
