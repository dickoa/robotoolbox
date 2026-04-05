## Submission 1.6.2

Patch release fixing a regression introduced while tightening repeat-group
detection after 1.6.1. The stricter check required repeat-group names to be
present in the downloaded payload before entering the `dm` branch, which caused
some genuine repeat-group forms to be returned as flat tables. Version 1.6.2
restores the simpler form-structure-based check (`begin_repeat` in the form),
so repeat-group forms are again returned as `dm` objects consistently.

### Key changes

- Fixed repeat-group detection so genuine repeat-group surveys return `dm`
  outputs again.
- Clarified `fields` documentation for repeat-group forms:
  main-table fields can be selected individually, top-level repeat groups are
  selected at table level, selected repeat groups are returned in full, and
  nested repeat groups / child columns are not independently selectable.

## Test environments

- Arch Linux x86_64 GNU/Linux, kernel 6.19.9, R 4.5.3 Patched (2026-03-11)
- Win-builder (devel, release)
- Github Action (linux, windows, macos)

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

Reverse dependencies: checked, 0 found.
