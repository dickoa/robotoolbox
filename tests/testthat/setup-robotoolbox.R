library("vcr")

vcr_dir <- vcr::vcr_test_path("fixtures")

has_prod <- nzchar(Sys.getenv("KOBOTOOLBOX_PROD_TOKEN"))
has_training <- nzchar(Sys.getenv("KOBOTOOLBOX_TRAINING_TOKEN"))
has_cassettes <- dir.exists(vcr_dir) && length(list.files(vcr_dir)) > 0

# When no real API credentials, fall back to cassettes
if (!has_prod && !has_training) {
  if (has_cassettes) {
    fake_token <- "0000000000000000000000000000000000000000"
    Sys.setenv(
      "KOBOTOOLBOX_TOKEN" = fake_token,
      "KOBOTOOLBOX_PROD_TOKEN" = fake_token,
      "KOBOTOOLBOX_PROD_URL" = "https://kobo.unhcr.org",
      "KOBOTOOLBOX_TRAINING_TOKEN" = fake_token,
      "KOBOTOOLBOX_TRAINING_URL" = "https://kobo-trn.unhcr.org"
    )
  } else {
    stop("No API key nor cassettes, tests cannot be run.",
         call. = FALSE)
  }
}

invisible(vcr::vcr_configure(
  dir = vcr_dir,
  preserve_exact_body_bytes = TRUE,
  filter_request_headers = list(Authorization = "My bearer token is safe")
))

# Helper: TRUE when running with real API credentials
has_live_api <- function() has_prod || has_training
