library("vcr")

vcr_dir <- vcr::vcr_test_path("fixtures")

if (!nzchar(Sys.getenv("KOBOTOOLBOX_TOKEN"))) {
  if (dir.exists(vcr_dir)) {
    Sys.setenv("KOBOTOOLBOX_TOKEN" = "foobar")
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
