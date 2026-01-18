robotoolbox 1.5 (2026-01-17)
======================

### BREAKING CHANGES
- **Default pagination limit reduced from 30,000 to 1,000** to comply with KoboToolbox API changes
  effective January 2026 on public servers (see https://community.kobotoolbox.org/t/important-changes-to-api-v2-assets-uid-asset-data-result-limits/74610).
- Auto-pagination now triggers at 1,000 submissions (previously 10,000).
- **Attachment filenames now include attachment UID** for uniqueness. Files are now named
  `{att_uid}_{filename}` instead of `{submission_id}_{filename}` to handle cases where the same
  submission has multiple attachments with identical filenames.

### NEW FEATURES
- **Session-level caching** for form metadata and languages significantly improves performance
  for repeated operations. Use `kobo_cache_info()` to view cache status and `kobo_cache_clear()`
  to clear the cache when needed.
- **Language switching without re-downloading data:**
  - `kobo_lang_set(data, asset, lang)` instantly switches variable and value labels to a different
    language using cached form metadata.
  - `kobo_lang_get(data, asset)` detects the current language applied to a dataset.
  - Works with both simple `data.frame` and complex `dm` objects (nested forms).

### BUG FIXES
- Fixed `kobo_audit()` and `kobo_attachment_download()` to handle API changes where the
  `instance` field was removed from the `_attachments` structure.
- Fixed `kobo_attachment_download()` `overwrite = FALSE` check to correctly skip existing files.
- Improved error handling for non-JSON API error responses.
- Fixed `val_labels_from_form_()` to handle forms without `value_version` column.
- Fixed documentation typos in README and vignettes.

### IMPROVEMENTS
- Simplified internal attachment processing using `tidyr::unnest()` for cleaner code.
- Reduced sleep times between paginated API requests for better performance with 1,000 record limit.
- Removed deprecated `vcr::check_cassette_names()` from test setup.

### NOTES
- The default `page_size` is now capped at 1,000 for safety on public KoboToolbox servers.
- **Performance tip for private instances:** Users with private servers that allow higher limits
  can explicitly set `page_size` (e.g., `kobo_data(asset, page_size = 30000)`) for significantly
  better performance on large datasets.
- **Known limitation:** `kobo_audit()` and `kobo_attachment_download()` currently fetch
  attachments from the first 1,000 submissions only. Full pagination support for these
  functions is planned for a future release.

robotoolbox 1.3.4 (2024-12-19)
======================

### NEW FEATURES
- Added `kobo_file_list` to list all media files availabe
- Added `kobo_attachment_download` to download attached files in a specific folder

### BUG FIXES
- if you have a column `var_other` and `select_multiple` choice `other` for a column `var`, the, `var_other` is silently overwritten. It's fixed now and we rename the duplicate.
- text question with numbers are parsed as numbers, it's fixed now.
- `select_multiple` questions with choices with special characters suchs `(`, `)`, `{` or `}` were breaking the split into dummy variables. Bug fixed.
- Fix bug on `paginate` in `kobo_data`
- Fix bug in `kobo_audit` when you have audit data columns are different across submissions.

### MINOR IMPROVEMENTS
- Support for media question types (`media::image`, `media::big-image`, `media::audio`, `media::video`)
- Keep the `_attachments` column in the data.

robotoolbox 1.3.2
======================

### NEW FEATURES

  * released to CRAN

robotoolbox 1.2.0.9075
======================

### BUG FIXES
- Better version order to use the choice labels of the latest form deployed.

### NEW FEATURES
- Added a fonction `kobo_audit` to get all the audit logs data from a form with audit enabled. It comes with a vignette showing how to read audit logs.
- Add a `progress` parameter to `kobo_data`, you can see your progress through custom messages.
- Added a logical parameter `select_multiple_label` to `kobo_data` in order to turn `select_multiple` values to labels. It's useful if you want to work directly with labels in `select_multiple`
- Added a `all_versions` logical parameter to `kobo_data` to check whether or not you want to include data from all form versions.
- Added a `colnames_label` logical parameter to `kobo_data` to check whether or not you want to include variable labels as column names.

robotoolbox 1.1.0.9004
======================

### BUG FIXES

- Fixing the following issue: Using `true` or `false` as values in `select_one` breaks the labels.
- Revert to `type.convert` because of flexibility on boolean/logical
- Add a fonction to merge the variables from different groups due to different versions of the form used during data collection.


robotoolbox 1.1.0.9000
======================

### IMPROVEMENTS

- `robotoolbox` provides WKT columns for `geopoint`, `geotrace` and `geoshape` question types, for easier spatial data analysis.
- Remove unnecessary list-columns and pre-process `_validation_status` column.

### BUG FIXES

- Fix a bug in `kobo_form` when you have a `list_name` in the `choices` tab but no associated question in the `survey` tab.

robotoolbox 1.0.9.9002
======================

### BUG FIXES

- It is now possible to match submissions to form based on the version of the form used even when you're not the owner of the project. This new feature work best with `Kobotoolbox kpi` version `2.023.12` and up.
- When you have child table with differents parent tables, split it and change names.

### MINOR IMPROVEMENTS
- Update `purrr` to 1.0.1

robotoolbox 1.0.7.9001
======================

### MINOR IMPROVEMENTS

- `kobo_data` and `kobo_submissions` are faster for surveys with `repeat group`.
-  Add variables as labels when a variable label is missing
-  Custom labels for `select_multiple` dummy variables


robotoolbox 1.0.6.9000
======================

### BUG FIXES

- `kobo_form` was not working with survey with duplicated columns. Duplicared columns will be deduped.


robotoolbox 1.0.5.9000
======================

### NEW FEATURES

- Update to make it compatible with `tidyr` `1.3.0`
- Hide token when using `kobo_settings` print method

robotoolbox 1.0.4.9000
======================

### NEW FEATURES

- Update code to remove depecrated `.data` arguments in `tidyselect` function. Introduced as of `tidyselect` version `1.2.0`
- Use `readr::type_convert` instead of `utils::type.convert` to get the right date, datetime types

robotoolbox 1.0.1.9000
======================

### NEW FEATURES

- An empty tibble is returned for project with zero submissions
- Fixed bug on `kobo_form`

robotoolbox 1.0.0.9000
======================

### NEW FEATURES

- All columns are exported including columns with no responses
-  Adapt vignette to {dm} new features since version one and above.
-  Add `retry` support in simple queries.

### BUG FIXES


robotoolbox 1.0.0
======================

### NEW FEATURES

### BUG FIXES



robotoolbox 0.9.9001
======================

### NEW FEATURES
- Renaming KoBo to Kobo to match changes made by the Kobotoolbox team
-  All values are turn into dummies including values not used

### BUG FIXES
