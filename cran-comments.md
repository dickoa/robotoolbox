## Submission 1.5.0

This is a major update to comply with **breaking changes to the KoboToolbox
API v2** introduced in KoboToolbox KPI version 2.026.03 (March 2026). The
`/api/v2/assets/{uid}/data` endpoint now enforces a maximum of 1,000 records
per request (previously ~30,000). Without this update, users will silently
receive truncated data.

### Key changes

- Pagination logic updated: datasets with >1,000 submissions are automatically
  paginated in 1,000-record chunks.
- Datasets with ≤1,000 submissions are fetched in a single request
  (`limit=1000`).
- `kobo_setup()` now accepts a `page_size` parameter (default `1000`) to
  configure the maximum number of submissions per API request. Users with
  private servers that allow higher limits can set this to a larger value.
- `page_size` is validated and capped at the configured maximum.
- Internal helpers now paginate to avoid silent data loss on large datasets.
- New session-level caching and language switching features.
- Bug fixes for attachment downloads and audit log retrieval.

## Test environments

- Arch Linux x86_64 GNU/Linux, kernel 6.19.6, R 4.5.3 Patched (2026-03-11)

## R CMD check results

0 errors | 0 warnings | 0 notes
