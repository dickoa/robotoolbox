## Fake data endpoint that returns at most `cap` records per request
fake_server <- function(total, cap = Inf, rows = NULL) {
  if (is.null(rows)) {
    rows <- function(ids) data.frame(`_id` = ids, check.names = FALSE)
  }
  calls <- new.env()
  calls$n <- 0L
  calls$rounds <- list()
  fetch <- function(uid, start, limit, query = NULL, fields = NULL) {
    calls$n <- calls$n + length(start)
    calls$rounds[[length(calls$rounds) + 1L]] <- data.frame(start, limit)
    Map(function(s, l) {
      n <- max(0, min(l, cap, total - s))
      ids <- if (n > 0) seq(s + 1, s + n) else integer()
      list(count = total, results = rows(ids))
    }, start, limit)
  }
  list(fetch = fetch, calls = calls)
}

with_fake_server <- function(server, code, version = asset_ml$deployed_version_id) {
  old <- Sys.getenv("KOBOTOOLBOX_URL")
  old_cap <- .robotoolbox_cache$page_cap
  Sys.setenv(KOBOTOOLBOX_URL = "https://fake.kobo.org")
  .robotoolbox_cache$page_cap <- list()
  on.exit({
    .robotoolbox_cache$page_cap <- old_cap
    Sys.setenv(KOBOTOOLBOX_URL = old)
  })
  local_mocked_bindings(
    fetch_subs_pages_ = server$fetch,
    kobo_asset_version_list = function(x, ...) {
      tibble::tibble(uid = version, deployed = TRUE)
    }
  )
  code
}
