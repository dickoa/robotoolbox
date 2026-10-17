## Submission 1.8.0

This release fixes silent data loss and adapts the package to recent
KoboToolbox API changes.

KoboToolbox servers cap the number of records returned per request and
ignore larger requests without an error. Since KoboToolbox KPI version
2.026.12, the cap applies to every API endpoint. When users raised `page_size`
above that cap, `kobo_data()`, `kobo_attachment_download()` and `kobo_audit()`
returned incomplete results. Version 1.7.0 detects the cap from the server
response and requests the missing records. Version and file listings, which
returned at most 1,000 and 100 results, now page through all results.

### Key changes

- Fixed incomplete results when `page_size` exceeds the server limit.
- Fixed truncated results in `kobo_asset_version_list()` and
  `kobo_asset_file_list()`.
- New `minimal` argument and `created_by` column in `kobo_asset_list()`.
- `glue` removed from `Imports`. `%||%` is now imported from `rlang`, so
  the package no longer relies on the base R version introduced in R 4.4.

## Test environments

- Arch Linux x86_64 GNU/Linux, kernel 7.2.9, R 4.6.1 Patched (2026-06-28)

## R CMD check results

0 errors | 0 warnings | 1 note

The note concerns the `BugReports` field. The issue tracker of the GitLab
project lives at `/-/work_items`, which is the correct URL. The suggestion to
use a different path comes from a heuristic that assumes a GitHub-style
layout.

## Reverse dependencies

Reverse dependencies: checked, 0 found.
