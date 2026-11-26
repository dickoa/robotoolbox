# Extracted from test-kobo_data.R:2041

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "robotoolbox", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
ml_rows <- function(ids) {
  data.frame(
    `_id` = ids,
    `__version__` = rep(asset_ml$deployed_version_id, length(ids)),
    full_name = rep("A", length(ids)),
    check.names = FALSE
  )
}
nested_form <- tibble::tibble(
  name = c("member", "visit", "treatment", "dose", "dose_mg", NA, NA, NA, NA,
           "item", "item_name", NA),
  type = c(rep("begin_repeat", 4), "integer", rep("end_repeat", 4),
           "begin_repeat", "text", "end_repeat"),
  lang = "Labels",
  version = "v1"
)
two_version_form <- tibble::tibble(
  name = c("hh", "group", "new_age", "moved", NA,
           "hh", "moved", "group", "old_age", NA),
  type = c("text", "begin_repeat", "integer", "integer", "end_repeat",
           "text", "integer", "begin_repeat", "integer", "end_repeat"),
  label = c("Household", "Group", "New age", "Moved", NA,
            "Household", "Moved", "Group", "Old age", NA),
  lang = "Labels",
  version = rep(c("vNEW", "vOLD"), each = 5)
)
sm_asset <- function() {
  ch <- function(name, label) list(list_name = "d", name = name, label = list(label))
  structure(
    list(
      uid = "aSelectMultipleLabels00",
      asset_type = "survey",
      deployment__submission_count = 5L,
      version_id = "v1",
      deployed_version_id = "v1",
      content = list(
        survey = list(list(
          type = "select_multiple", name = "dist", label = list("Distance"),
          select_from_list_name = "d"
        )),
        choices = list(
          ch("1", "Less than 2 km"), ch("2", "2 to 5 km"), ch("3", "More than 5 km"),
          ch("a+b", "Plus"), ch("I", "I have no need"), ch("E", "I lack documents"),
          ch("m", "Member ${member_1}")
        ),
        translations = list(NULL)
      )
    ),
    class = "kobo_asset"
  )
}
versions_q <- function(type, name, label = NULL) {
  res <- list(type = type, name = name)
  if (!is.null(label)) res$label <- list(label)
  res
}
versions_asset <- function() {
  structure(
    list(
      uid = "aAllVersionsFalse000000",
      asset_type = "survey",
      deployment__submission_count = 2L,
      version_id = "vNEW",
      version__content_hash = "hNEW",
      deployed_version_id = "vNEW",
      deployed_versions = list(count = 2L, results = list(
        list(uid = "vNEW", content_hash = "hNEW"),
        list(uid = "vOLD", content_hash = "hOLD")
      )),
      content = list(
        survey = list(
          versions_q("text", "hh", "Household (new)"),
          versions_q("text", "new_q", "New question"),
          versions_q("begin_group", "g2", "Group 2"),
          versions_q("text", "sex", "Sex"),
          list(type = "end_group")
        ),
        translations = list(NULL)
      )
    ),
    class = "kobo_asset"
  )
}
old_version <- structure(
  list(content = list(
    survey = list(
      versions_q("text", "hh", "Household (old)"),
      versions_q("text", "old_q", "Old question"),
      versions_q("begin_group", "g1", "Group 1"),
      versions_q("text", "sex", "Sex"),
      list(type = "end_group")
    ),
    translations = list(NULL)
  )),
  class = "kobo_asset_version"
)
collide_asset <- function(q = versions_q("text", "pet_other", "Other pet"),
                          q_first = FALSE, in_repeat = FALSE) {
  ch <- function(list_name, name, label) {
    list(list_name = list_name, name = name, label = list(label))
  }
  sm <- list(type = "select_multiple", name = "pet", label = list("Pet"),
             select_from_list_name = "p")
  qs <- if (q_first) list(q, sm) else list(sm, q)
  if (in_repeat) {
    qs <- c(list(versions_q("begin_repeat", "member", "Member")), qs,
            list(list(type = "end_repeat")))
  }
  structure(
    list(
      uid = "aDummyNameCollision0000",
      asset_type = "survey",
      deployment__submission_count = 2L,
      version_id = "v1",
      deployed_version_id = "v1",
      content = list(
        survey = c(list(versions_q("text", "hh", "Household")), qs,
                   list(versions_q("text", "last", "Last"))),
        choices = list(
          ch("p", "cat", "Cat"), ch("p", "dog", "Dog"), ch("p", "other", "Other"),
          ch("k", "a", "Kind A"), ch("k", "b", "Kind B")
        ),
        translations = list(NULL)
      )
    ),
    class = "kobo_asset"
  )
}
collide_rows <- function(other, notes = NULL) {
  function(ids) {
    d <- data.frame(`_id` = ids, `__version__` = "v1", hh = c("A", "B")[ids],
                    pet = c("cat other", "dog")[ids], pet_other = other[ids],
                    last = c("x", "y")[ids], check.names = FALSE)
    if (!is.null(notes)) d$pet_notes <- notes[ids]
    d
  }
}
collide_data <- function(asset, rows, ...) {
  withr::defer(kobo_cache_clear(asset$uid))
  res <- NULL
  with_fake_server(fake_server(total = 2, rows = rows), {
    local_mocked_bindings(kobo_form_version_ = function(...) kobo_form(asset))
    expect_no_warning(
      expect_message(res <- kobo_data(asset, ...), "pet_other_1")
    )
  }, version = "v1")
  res
}
form_cols <- function(x) {
  x <- x[!grepl("^_|^meta/|^formhub/", names(x))]
  list(
    names = names(x),
    values = lapply(unname(as.list(x)), as.vector),
    labels = vapply(x, function(col) labelled::var_label(col) %||% NA_character_,
                    character(1), USE.NAMES = FALSE)
  )
}
geo_asset <- function() {
  q <- function(type, name, en = NULL, fr = NULL) {
    res <- list(type = type, name = name)
    if (!is.null(en)) res$label <- list(en, fr)
    res
  }
  structure(
    list(
      uid = "aSpatialLabels000000000",
      asset_type = "survey",
      deployment__submission_count = 1L,
      version_id = "v1",
      deployed_version_id = "v1",
      content = list(
        survey = list(
          q("geopoint", "pt", "Record a location", "Position"),
          q("geotrace", "line", "Path", "Chemin"),
          q("geoshape", "area", "Plot", "Parcelle"),
          q("geopoint", "bare")
        ),
        translations = list("English (en)", "Francais (fr)")
      )
    ),
    class = "kobo_asset"
  )
}

# test -------------------------------------------------------------------------
geo_q <- function(type, name, label) list(type = type, name = name, label = list(label))
asset <- collide_asset()
asset$uid <- "aSpatialNameCollision00"
asset$content$survey <- list(
    geo_q("geotrace", "track", "Track"), geo_q("text", "track_wkt", "Track note"),
    geo_q("geoshape", "area", "Area"), geo_q("integer", "area_wkt", "Area count"),
    geo_q("geopoint", "pt", "Point"), geo_q("text", "pt_latitude", "Latitude note"),
    geo_q("text", "pt_wkt", "Point note"), geo_q("text", "pt_wkt_1", "Other note")
  )
withr::defer(kobo_cache_clear(asset$uid))
rows <- function(ids) {
    data.frame(`_id` = ids, `__version__` = "v1", track = "10 20 0 0;11 21 0 0",
               track_wkt = "Keep this answer", area = "0 0 0 0;0 1 0 0;1 1 0 0;0 0 0 0",
               area_wkt = "007", pt = "1 2 3 4", pt_latitude = "north",
               pt_wkt = "a point", pt_wkt_1 = "kept too", check.names = FALSE)
  }
with_fake_server(fake_server(total = 1, rows = rows), {
    local_mocked_bindings(kobo_form_version_ = function(...) kobo_form(asset))
    expect_no_warning(expect_message(res <- kobo_data(asset), "track_wkt_1"))
  }, version = "v1")
expect_identical(form_cols(res), list(
    names = c("track", "track_wkt", "track_wkt_1", "area", "area_wkt", "area_wkt_1",
              "pt_latitude", "pt_longitude", "pt_altitude", "pt_precision", "pt",
              "pt_wkt", "pt_latitude_1", "pt_wkt_2", "pt_wkt_1"),
    values = list("10 20 0 0;11 21 0 0", "LINESTRING (20 10 0, 21 11 0)", "Keep this answer",
                  "0 0 0 0;0 1 0 0;1 1 0 0;0 0 0 0", "POLYGON ((0 0 0, 1 0 0, 1 1 0, 0 0 0))",
                  7L, 1, 2, 3, 4, "1 2 3 4", "POINT (2 1 3)", "north", "a point", "kept too"),
    labels = c("Track", "Track::wkt", "Track note", "Area", "Area::wkt", "Area count",
               "Point::latitude", "Point::longitude", "Point::altitude", "Point::precision",
               "Point", "Point::wkt", "Latitude note", "Point note", "Other note")
  ))
