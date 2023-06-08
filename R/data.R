#' Examples of KoboToolbox assets and list of assets
#'
#' Examples of KoboToolbox assets and list of assets.
#'
#' @rdname dummy_asset
#' @format asset_list: a \code{data.frame} of 28 rows and 7 columns with a list of API assets
"asset_list"

#' @rdname dummy_asset
#' @format asset_ml: A \code{kobo_asset} object on a survey using multiple languages.
"asset_ml"

#' @rdname dummy_asset
#' @format asset_rg: A \code{kobo_asset} object on a survey using repeat groups.
"asset_rg"

#' @rdname dummy_asset
#' @format asset_spatial: A \code{kobo_asset} object on a survey showcasing gps questions.
"asset_spatial"

#' @rdname dummy_asset
#' @format asset_sm_label: A \code{kobo_asset} object to showcase select multiple labels.
"asset_sm_label"

#' @rdname dummy_asset
#' @format asset_audit: A \code{kobo_asset} object on a survey with audit logging enabled.
"asset_audit"

#' Examples of KoboToolbox submissions data
#'
#' Examples of KoboToolbox submissions data.
#'
#' @rdname dummy_data
#' @format data_ml: A \code{data.frame} with submissions from \code{\link{asset_ml}}
#' in English.
"data_ml_en"

#' @rdname dummy_data
#' @format data_ml_fr: A \code{data.frame} with submissions from \code{\link{asset_ml}}
#' in French.
"data_ml_fr"

#' @rdname dummy_data
#' @format data_ml_ar: A \code{data.frame} with submissions from \code{\link{asset_ml}}
#' in Arabic
"data_ml_ar"

#' @rdname dummy_data
#' @format data_ml_default: A \code{data.frame} with submissions from \code{\link{asset_ml}}
#' with the default language.
"data_ml_default"

#' @rdname dummy_data
#' @format data_ml_vlabel: A \code{data.frame} with submissions from \code{\link{asset_ml}}
#' using variable labels as column names.
"data_ml_vlabel"

#' @rdname dummy_data
#' @format data_rg: A \code{dm} object with submissions from \code{\link{asset_rg}}
"data_rg"

#' @rdname dummy_data
#' @format data_spatial: A \code{data.frame} with submissions from
#' the \code{\link{asset_spatial}} KoboToolbox API asset.
"data_spatial"

#' @rdname dummy_data
#' @format data_sm: A \code{data.frame} with submissions from
#' \code{\link{asset_sm_label}} with no labels for the `select_multiple` question.
"data_sm"

#' @rdname dummy_data
#' @format data_sm_label: A \code{data.frame} with submissions from
#' \code{\link{asset_sm_label}} with labels for the `select_multiple` question.
"data_sm_label"

#' @rdname dummy_data
#' @format data_audit: A \code{data.frame} with submissions from \code{\link{asset_audit}}.
"data_audit"
