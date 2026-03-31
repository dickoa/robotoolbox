## Submission 1.6.1

Patch release fixing a regression introduced in 1.6.0 where `dm` child tables
(repeat groups) were inflated with hundreds of spurious all-NA columns from
unrelated tables. This also fixes several smaller bugs found during review.

### Key changes

- Fixed repeat group scope tracking: child tables now contain only their own
  columns, matching the KoboToolbox Excel export.
- Empty-result path now returns the correct type (`dm` for repeat-group forms)
  and respects `fields` and `colnames_label` parameters.
- `kobo_lang_get()` and `kobo_lang_set()` now work on data created with
  `colnames_label = TRUE`.
- Pagination edge cases fixed (single-submission crash, off-by-one extra request).
- `validate_query_()` now actually parses JSON via `RcppSimdJson::fparse()`.
- `kobo_token()` validates URL before calling curl.
- Documentation fixes for `kobo_attachment_download()` and `kobo_token()`.

## Test environments

- Arch Linux x86_64 GNU/Linux, kernel 6.19.9, R 4.5.3 Patched (2026-03-11)
- Win-builder (devel, release)
- Github Action (linux, windows, macos)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

Reverse dependencies: checked, 0 found.
