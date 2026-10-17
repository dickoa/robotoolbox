test_that("kobo_lang works", {
  skip_on_cran()
  url <- Sys.getenv("KOBOTOOLBOX_PROD_URL")
  token <- Sys.getenv("KOBOTOOLBOX_PROD_TOKEN")
  skip <-  url == "" & token == ""
  skip_if(skip,
          message = "Test server not configured")

  kobo_setup(url = url,
             token = token)
  uid <- "aYuTZn9vegi3Z49MXwKjep"
  kobo_cache_clear(uid)
  vcr::use_cassette("kobo_lang", {
    lng1 <- kobo_lang(uid)
    lng2 <- kobo_lang(kobo_asset(uid))
  })
  expect_equal(lng1, lng2)
  expect_type(lng1, "character")
  expect_error(kobo_lang(1L))
  expect_error(kobo_lang("bad_uid"))
})

## Language switch on data from kobo_data

lang_asset <- function(with_repeat = FALSE) {
  q <- function(type, name, en = NULL, fr = NULL, ...) {
    res <- list(type = type, name = name, ...)
    if (!is.null(en)) res$label <- list(en, fr)
    res
  }
  survey <- list(
    q("text", "first", "Name", "Prenom"),
    q("text", "second", "Name", "Nom"),
    q("select_multiple", "colors", "Colors", "Couleurs", select_from_list_name = "c"),
    q("text", "position", "first", "premier"),
    list(type = "select_one", name = "avail", select_from_list_name = "y")
  )
  if (with_repeat) {
    survey <- c(survey, list(
      q("begin_repeat", "kids", "Kids", "Enfants"),
      q("text", "kid", "Name", "Prenom"),
      q("select_multiple", "kid_colors", "Colors", "Couleurs", select_from_list_name = "c"),
      list(type = "end_repeat")
    ))
  }
  ch <- function(name, en, fr) list(list_name = "c", name = name, label = list(en, fr))
  structure(
    list(
      uid = if (with_repeat) "aLangSwitchRepeat000000" else "aLangSwitchFlat00000000",
      asset_type = "survey",
      deployment__submission_count = 2L,
      version_id = "v1",
      deployed_version_id = "v1",
      content = list(
        survey = survey,
        choices = list(
          ch("r", "Red", "Rouge"), ch("b", "Blue", "Bleu"),
          ch("l", "Light blue", "Bleu clair"),
          list(list_name = "y", name = "1", label = list("Available"))
        ),
        translations = list("English (en)", "French (fr)")
      )
    ),
    class = "kobo_asset"
  )
}

lang_rows <- function(ids) {
  res <- data.frame(
    `_id` = ids,
    `__version__` = rep("v1", length(ids)),
    first = c("Alice", "Binta")[ids],
    second = c("Smith", "Diallo")[ids],
    colors = c("r l b", "b")[ids],
    position = c("1", "2")[ids],
    avail = c("1", "1")[ids],
    check.names = FALSE
  )
  res$kids <- list(
    data.frame(`kids/kid` = c("Kid A", "Kid B"), `kids/kid_colors` = c("l", "r b"),
               check.names = FALSE),
    NULL
  )[ids]
  res
}

lang_data <- function(asset, ..., rows = lang_rows, total = 2) {
  server <- fake_server(total = total, rows = rows)
  with_fake_server(server, {
    local_mocked_bindings(kobo_form_version_ = function(...) kobo_form(asset))
    res <- kobo_data(asset, ...)
  }, version = "v1")
  res
}

test_that("kobo_lang_set maps label columns through their question names", {
  asset <- lang_asset()
  on.exit(kobo_cache_clear(asset$uid))
  en <- lang_data(asset, lang = "English (en)", colnames_label = TRUE)
  expect_true(all(c("Name", "Name_1", "Colors::Red") %in% names(en)))
  expect_equal(kobo_lang_get(en, asset), "English (en)")

  fr <- kobo_lang_set(en, asset, "French (fr)")
  expect_true(all(c("Prenom", "Nom", "Couleurs::Rouge", "premier") %in% names(fr)))
  expect_false(any(c("Name", "Name_1") %in% names(fr)))
  expect_equal(as.vector(fr$Prenom), c("Alice", "Binta"))
  expect_equal(as.vector(fr$Nom), c("Smith", "Diallo"))
  expect_equal(as.vector(fr[["Couleurs::Rouge"]]), c(1L, 0L))
  expect_equal(kobo_lang_get(fr, asset), "French (fr)")

  expect_identical(kobo_lang_set(fr, asset, "English (en)"), en)
  expect_identical(fr, lang_data(asset, lang = "French (fr)", colnames_label = TRUE))
})

test_that("kobo_lang_set keeps question names when columns are not labels", {
  asset <- lang_asset()
  on.exit(kobo_cache_clear(asset$uid))
  en <- lang_data(asset, lang = "English (en)")
  fr <- kobo_lang_set(en, asset, "French (fr)")
  expect_equal(names(fr), names(en))
  expect_equal(labelled::var_label(fr$first), "Prenom")
  expect_equal(labelled::var_label(fr$colors_r), "Couleurs::Rouge")
  expect_equal(kobo_lang_get(fr, asset), "French (fr)")
  expect_null(labelled::val_labels(fr$avail))
  expect_identical(kobo_lang_set(fr, asset, "English (en)"), en)
  expect_identical(fr, lang_data(asset, lang = "French (fr)"))
})

test_that("kobo_lang_set switches the language of a dm both ways", {
  asset <- lang_asset(with_repeat = TRUE)
  on.exit(kobo_cache_clear(asset$uid))
  en <- lang_data(asset, lang = "English (en)", colnames_label = TRUE)
  expect_true(all(c("Name", "Name_1") %in% names(en$main)))
  expect_true("Name" %in% names(en$kids))

  fr <- kobo_lang_set(en, asset, "French (fr)")
  expect_equal(as.vector(fr$main$Nom), c("Smith", "Diallo"))
  expect_equal(as.vector(fr$kids$Prenom), c("Kid A", "Kid B"))
  expect_equal(kobo_lang_get(fr, asset), "French (fr)")
  expect_equal(dm::dm_get_all_fks(fr), dm::dm_get_all_fks(en))

  back <- kobo_lang_set(fr, asset, "English (en)")
  expect_identical(dm::dm_get_tables(back), dm::dm_get_tables(en))
})

test_that("kobo_lang_set reads label columns without question names", {
  asset <- lang_asset()
  on.exit(kobo_cache_clear(asset$uid))
  en <- lang_data(asset, lang = "English (en)", colnames_label = TRUE)
  for (i in seq_along(en)) attr(en[[i]], "kobo_name") <- NULL
  fr <- kobo_lang_set(en, asset, "French (fr)")
  expect_true(all(c("Couleurs", "premier") %in% names(fr)))
  expect_equal(as.vector(fr$premier), c("1", "2"))
  expect_equal(as.vector(fr$Name), c("Alice", "Binta"))
})

test_that("kobo_lang_set renames label columns equal to their question names", {
  asset <- lang_asset()
  asset$uid <- "aLangSameNames000000000"
  asset$content$survey <- list(
    list(type = "text", name = "name", label = list("name", "Nom"))
  )
  on.exit(kobo_cache_clear(asset$uid))
  rows <- function(ids) {
    data.frame(`_id` = ids, `__version__` = rep("v1", length(ids)),
               name = c("Alice", "Binta")[ids], check.names = FALSE)
  }
  for (total in c(2, 0)) {
    info <- paste("submissions:", total)
    en <- lang_data(asset, lang = "English (en)", colnames_label = TRUE,
                    rows = rows, total = total)
    fr <- kobo_lang_set(en, asset, "French (fr)")
    expect_true("Nom" %in% names(fr), info = info)
    expect_identical(fr, lang_data(asset, lang = "French (fr)", colnames_label = TRUE,
                                   rows = rows, total = total), info = info)
    expect_identical(kobo_lang_set(fr, asset, "English (en)"), en, info = info)
    expect_equal(kobo_lang_get(fr, asset), "French (fr)", info = info)
  }
})

test_that("kobo_lang_set translates select_multiple answers given as labels", {
  for (with_repeat in c(FALSE, TRUE)) {
    asset <- lang_asset(with_repeat = with_repeat)
    info <- paste("repeat:", with_repeat)
    kobo_cache_clear(asset$uid)
    en <- lang_data(asset, lang = "English (en)", select_multiple_label = TRUE)
    fr <- kobo_lang_set(en, asset, "French (fr)")
    direct <- lang_data(asset, lang = "French (fr)", select_multiple_label = TRUE)
    if (with_repeat) {
      expect_equal(as.vector(fr$kids$kid_colors), c("Bleu clair", "Rouge Bleu"),
                   info = info)
      fr <- dm::dm_get_tables(fr)
      direct <- dm::dm_get_tables(direct)
      back <- dm::dm_get_tables(kobo_lang_set(dm::as_dm(fr), asset, "English (en)"))
      en <- dm::dm_get_tables(en)
    } else {
      back <- kobo_lang_set(fr, asset, "English (en)")
    }
    main <- if (with_repeat) fr$main else fr
    expect_equal(as.vector(main$colors), c("Rouge Bleu clair Bleu", "Bleu"), info = info)
    expect_identical(fr, direct, info = info)
    expect_identical(back, en, info = info)
    kobo_cache_clear(asset$uid)
  }
})

test_that("kobo_lang_set stops on select_multiple labels it cannot split", {
  asset <- lang_asset()
  asset$uid <- "aLangAmbiguous000000000"
  asset$content$choices[[3]]$label <- list("Red Blue", "Violet")
  on.exit(kobo_cache_clear(asset$uid))
  en <- lang_data(asset, lang = "English (en)", select_multiple_label = TRUE)
  expect_equal(as.vector(en$colors), c("Red Red Blue Blue", "Blue"))
  expect_error(kobo_lang_set(en, asset, "French (fr)"), "colors")
})

test_that("kobo_lang_set stops on an unlabeled code spelled like a label", {
  asset <- lang_asset()
  asset$uid <- "aLangUnlabeledCode00000"
  on.exit(kobo_cache_clear(asset$uid))
  coded <- function(colors) {
    function(ids) {
      res <- lang_rows(ids)
      res$colors <- colors[ids]
      res
    }
  }
  en <- lang_data(asset, lang = "English (en)", select_multiple_label = TRUE,
                  rows = coded(c("zz b", "r")))
  fr <- kobo_lang_set(en, asset, "French (fr)")
  expect_equal(as.vector(fr$colors), c("zz Bleu", "Rouge"))
  expect_identical(fr, lang_data(asset, lang = "French (fr)", select_multiple_label = TRUE,
                                 rows = coded(c("zz b", "r"))))
  expect_identical(kobo_lang_set(fr, asset, "English (en)"), en)

  asset$content$choices <- c(asset$content$choices,
                             list(list(list_name = "c", name = "g", label = list(NULL, "Vert"))))
  kobo_cache_clear(asset$uid)
  en <- lang_data(asset, lang = "English (en)", select_multiple_label = TRUE,
                  rows = coded(c("g b", "r")))
  fr <- kobo_lang_set(en, asset, "French (fr)")
  expect_equal(as.vector(en$colors), c("g Blue", "Red"))
  expect_equal(as.vector(fr$colors), c("Vert Bleu", "Rouge"))
  expect_identical(fr, lang_data(asset, lang = "French (fr)", select_multiple_label = TRUE,
                                 rows = coded(c("g b", "r"))))
  expect_identical(kobo_lang_set(fr, asset, "English (en)"), en)

  en <- lang_data(asset, lang = "English (en)", select_multiple_label = TRUE,
                  rows = coded(c("Red", "b")))
  expect_equal(as.vector(en$colors), c("Red", "Blue"))
  expect_error(kobo_lang_set(en, asset, "French (fr)"), "colors")
})
