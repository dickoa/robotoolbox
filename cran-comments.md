## Submission 1.9.0

This release follows 1.6.2, the version on CRAN. It fixes silent data loss,
adapts the package to recent KoboToolbox API changes, and makes `kobo_data()`
faster on large forms.

KoboToolbox servers cap the number of records returned per request and
ignore larger requests without an error. Since KoboToolbox KPI version
2.026.12, the cap applies to every API endpoint. When users raised `page_size`
above that cap, `kobo_data()`, `kobo_attachment_download()` and `kobo_audit()`
returned incomplete results. This version detects the cap from the server
response and requests the missing records. Version and file listings, which
returned at most 1,000 and 100 results, now page through all results. Large
projects can also report their submission count as `"many"` or `"unknown"`,
and the package then follows the next page links until the last submission.

### Key changes

- Fixed incomplete results when `page_size` exceeds the server limit.
- Fixed truncated results in `kobo_asset_version_list()` and
  `kobo_asset_file_list()`.
- Fixed answers, repeat tables and labels that `kobo_data()` dropped or got
  wrong on forms with several versions or deeply nested repeat groups.
- Spatial columns give `NA` instead of invalid WKT.
- New `minimal` argument and `created_by` column in `kobo_asset_list()`, and
  a new `version_number` column in `kobo_asset_version_list()`.
- `kobo_data()` is about a third faster on large forms with many versions.
- Every request retries on status 429, 500, 502, 503 and 504, waits as long
  as the server asks (at most 60 seconds), and sends a User-Agent.
- `glue` removed from `Imports`. `%||%` is now imported from `rlang`, so
  the package no longer relies on the base R version introduced in R 4.4.

## Test environments

- Arch Linux x86_64 GNU/Linux, kernel 7.2.14, R 4.6.2 Patched (2026-10-30)

## R CMD check results

0 errors | 0 warnings | 1 note

The note concerns the `BugReports` field. The issue tracker of the GitLab
project lives at `/-/work_items`, which is the correct URL. The suggestion to
use a different path comes from a heuristic that assumes a GitHub-style
layout.

## Reverse dependencies

Reverse dependencies: checked, 0 found.
