#' Examples of KoboToolbox assets and list of assets
#'
#' Examples of KoboToolbox assets and list of assets.
#'
#' @name dummy_asset
#' @aliases asset_list
#' @family example datasets
#' @format asset_list: a `data.frame` of 28 rows and 7 columns with a list of API assets
"asset_list"

#' @rdname dummy_asset
#' @format asset_ml: A `kobo_asset` object on a survey using multiple languages.
"asset_ml"

#' @rdname dummy_asset
#' @format asset_rg: A `kobo_asset` object on a survey using repeat groups.
"asset_rg"

#' @rdname dummy_asset
#' @format asset_spatial: A `kobo_asset` object on a survey showcasing gps questions.
"asset_spatial"

#' @rdname dummy_asset
#' @format asset_sm_label: A `kobo_asset` object to showcase select multiple labels.
"asset_sm_label"

#' @rdname dummy_asset
#' @format asset_audit: A `kobo_asset` object on a survey with audit logging enabled.
"asset_audit"

#' Examples of KoboToolbox submissions data
#'
#' Examples of KoboToolbox submissions data.
#'
#' @name dummy_data
#' @aliases data_ml_en
#' @family example datasets
#' @format data_ml_en: A `data.frame` with submissions from [asset_ml]
#' in English.
"data_ml_en"

#' @rdname dummy_data
#' @format data_ml_fr: A `data.frame` with submissions from [asset_ml]
#' in French.
"data_ml_fr"

#' @rdname dummy_data
#' @format data_ml_ar: A `data.frame` with submissions from [asset_ml]
#' in Arabic
"data_ml_ar"

#' @rdname dummy_data
#' @format data_ml_default: A `data.frame` with submissions from [asset_ml]
#' with the default language.
"data_ml_default"

#' @rdname dummy_data
#' @format data_ml_vlabel: A `data.frame` with submissions from [asset_ml]
#' using variable labels as column names.
"data_ml_vlabel"

#' @rdname dummy_data
#' @format data_rg: A `dm` object with submissions from [asset_rg]
"data_rg"

#' @rdname dummy_data
#' @format data_spatial: A `data.frame` with submissions from
#' the [asset_spatial] KoboToolbox API asset.
"data_spatial"

#' @rdname dummy_data
#' @format data_sm: A `data.frame` with submissions from
#' [asset_sm_label] with no labels for the `select_multiple` question.
"data_sm"

#' @rdname dummy_data
#' @format data_sm_label: A `data.frame` with submissions from
#' [asset_sm_label] with labels for the `select_multiple` question.
"data_sm_label"

#' @rdname dummy_data
#' @format data_audit: A `data.frame` with submissions from [asset_audit].
"data_audit"
