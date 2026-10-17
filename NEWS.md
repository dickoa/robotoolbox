robotoolbox 1.8.0 (2026-10-17)
======================

### BUG FIXES
- `kobo_data()` no longer loses submissions when `page_size` exceeds the limit
  of the server. KoboToolbox servers return at most `MAX_API_PAGE_SIZE`
  submissions per request (1,000 on public servers) and silently ignore larger
  requests. With `kobo_setup(page_size = 5000)` on a public server, a form with
  2,217 submissions returned only 1,000. `kobo_data()` now detects the limit from
  the first response, requests the missing submissions in parallel, and uses
  that limit for the rest of the session. No extra request is made when
  `page_size` is within the limit.
- `kobo_attachment_download()` and `kobo_audit()` had the same problem when
  listing attachments, and are fixed the same way. Their listing step now also
  fetches pages in parallel.
- `kobo_data()` returned an empty table when the submission count stored in the
  asset was 0 or missing, without asking the server. Some archived projects
  report no count, so their data could never be read. On one server, 3 of 5
  archived projects came back empty, two of them holding 4,088 and 4,371
  submissions.
  `kobo_data()` now always asks the server and uses the stored count only to
  plan the requests. It also fetches submissions beyond an outdated count.
- The server returns 404 for the data of some old archived projects.
  `kobo_data()` now raises an error that says so, instead of returning an empty
  table.
- `kobo_data()` failed on a form with repeat groups that was never deployed, and
  `kobo_form()` failed on a form whose questions have no label. Both now work.
- `kobo_data()` dropped repeat groups nested more than two levels deep, with
  their data, without a warning. A form with repeats `member`, `visit`,
  `treatment` and `dose` returned only the `member` and `visit` tables. Repeat
  tables are now extracted at every depth, and each one is linked to its parent
  table in the `dm` object.
- `kobo_data()` left a repeat group out of the `dm` when no downloaded
  submission had data for it, so the tables returned depended on the data. A
  `query` matching only households without members returned no `member` table.
  Every repeat group of the form now gives a table, with zero rows when it has
  no data, and tables are listed in form order. With `fields`, repeat tables
  also keep the columns of questions without data. The empty `dm` returned for a
  form without submissions now has its keys, and `kobo_lang_set()` keeps the
  keys of the `dm` it receives, including those of empty tables.
- With `all_versions = TRUE`, a field that only existed inside a repeat group
  in an older form version was also added to the `main` table as an empty
  column. Each field now goes to the table it had in its own form version.
- `kobo_data(select_multiple_label = TRUE)` replaced choice codes with regular
  expressions, one code after another. It failed on labels that refer to other
  questions, such as `${member_1}`, and rewrote labels that contain another
  code. With codes `I` and `E`, the label of `E`, "I don't have the necessary
  documentation", became "I have no need for financial services ... don't have
  the necessary documentation". On two large forms, 293 of 9,507 and 2,587 of
  120,038 answers were affected. Each code is now looked up exactly.
- With a `select_multiple_sep` other than `"_"`, the dummy columns of
  `select_multiple` questions had no variable label, so `colnames_label = TRUE`
  left their names unchanged. On one form, 2,121 dummy columns were affected.
  Labels now follow the separator, and `kobo_lang_set()` detects it from the
  column names.
- `kobo_lang_set()` mapped label columns back to questions through their label
  text. Two questions with the same label, renamed `Name` and `Name_1`, could
  swap or keep the old language, and the `Question::Choice` columns of
  `select_multiple` questions were never renamed. Data whose labels all equal
  their question names was not recognized as named after labels, and kept the
  old names. Every column of `colnames_label = TRUE` data now keeps its
  question name in a `kobo_name` attribute, and `kobo_lang_set()` maps through
  it. Switching language and back returns the original data, and on two
  multilingual forms the result is the same as a new download in that
  language.
- `kobo_lang_set()` left the answers of `select_multiple_label = TRUE` data in
  the old language. It now translates them. An answer that splits into the
  labels of its question in more than one way is an error, since the codes it
  came from cannot be recovered. So is a code kept without a label that is
  spelled like the label of another choice.
- `kobo_data(select_multiple_label = TRUE)` replaced a code by an empty string
  when its choice had no label in the requested language, so `"g b"` became
  `" Blue"`. The code is now kept, as for a code without a choice.
- `kobo_lang_set()` treated data as named after labels when a single column
  name matched any label, then renamed columns the user had not renamed, or
  failed with "All renaming inputs must be named". It also kept value labels
  of the old language for questions without choices in the new one.
- Empty results from `kobo_data()` now have variable labels, and
  `colnames_label = TRUE` renames all their columns as it does with data.
- Without a `version`, `kobo_form()` and `kobo_lang()` read the last saved
  version of the form, which may never have been deployed. On one server, 11 of
  64 deployed forms had such a draft. Empty results from `kobo_data()` then had
  the columns of the draft, and one form gained 25 columns that were not
  deployed and lost 7 that were. Both functions now use the deployed version,
  and the draft only for a form never deployed.
- The form and language cache ignored the server URL and the form version, so
  a form redeployed during the session kept its old schema. With
  `all_versions = FALSE` or with zero rows, `kobo_data()` then dropped a new
  repeat group with its data, and `kobo_lang()` missed a new language. Forms and
  languages are now cached per server, asset and version.
- `kobo_audit()` failed on a form with audit enabled when no submission had an
  audit log. It reported a missing `media_file_basename` column when no
  submission had any attachment, and "must pass in at least one request" for a
  form without submissions or with other attachments only. It now returns a
  table with zero rows and the usual columns.
- `kobo_data()` rejected the empty query `'{}'`, which the server accepts and
  which matches every submission. It accepted an array such as
  `'[{"_id": 1}]'`, which the server answers with an error 500, reported after
  several retries as a uid or credentials problem. `query` is now accepted
  exactly when it is a JSON object.
- `kobo_data(all_versions = FALSE)` was documented as using the data of the
  latest form version, but it returned every submission, read with the form of
  an older version when all submissions came from it. For a question moved to
  another group between versions, it kept the column of the oldest version and
  dropped the answers recorded under the other paths, including those of the
  deployed version. With `FALSE`, every submission is now read with the
  deployed form, and moved questions keep all their answers, as with `TRUE`.
  The documentation says what the argument controls, and how to keep the
  submissions of one version with `query`.
- A question moved from the top level of the form into a group lost all its
  answers with the default `all_versions = TRUE`. The merged column had the
  name of one of the columns it replaced and was removed with them. This is
  the geopoint reported on the KoboToolbox forum in June 2026.
- With `all_versions = TRUE`, labels came from the newest form version that
  had submissions, so labels corrected in a new deployment did not show until
  someone submitted with it. The deployed form now always gives the labels,
  and older versions only add the questions and choices it lacks. Questions
  and repeat groups added in the deployed form appear before they have data.
- With `all_versions = TRUE`, the dummy columns of a `select_multiple`
  question were computed from the choices of the first form version only. A
  choice removed from the deployed form got a column of `NA`, even in the
  submissions that selected it. Dummy columns now cover the choices of every
  form version read in which the question is a `select_multiple`.
- A repeat group whose parent changed between form versions kept only its rows
  under the parent of the deployed form, without a warning. On one form, a
  renamed parent repeat group lost 13 of the 87 rows of its child. With
  `all_versions = TRUE`, the table of such a repeat group now holds the rows
  under every parent. `_parent_index` points to the parent of the deployed
  form, each other parent gets a `_parent_index_<parent>` column with its own
  foreign key, and `_parent_table_name` gives the parent of each row.
- `kobo_asset_version_list()` returned at most 1,000 versions, and
  `kobo_asset_file_list()` at most 100 files. Both now page through every
  result. Since KoboToolbox KPI version 2.026.12, every API endpoint caps
  `limit` at `MAX_API_PAGE_SIZE` and returns 100 results when no `limit` is
  given.
- `kobo_asset_list()` now asks for up to 1,000 assets per request instead of
  100, pages with `start` instead of the deprecated `offset`, and no longer asks
  for search metadata that it discarded.
- `kobo_asset_list()` could list some assets twice and skip others when the
  listing took more than one request. The server sorts assets by deployment
  status and date of last change, and assets with the same values can move
  between pages. On one server, 30 assets shared the same date of last change
  after a migration. Assets are now sorted by date of last change, most recent
  first, then by name, owner, type and deployment date, and duplicates are
  removed. The full listing warns when it returns fewer assets than the server
  reports, which can still happen when two assets match on all these fields.
- `%||%` is now imported from `rlang`. The package relied on the base R
  version, which only exists from R 4.4, whereas `DESCRIPTION` requires R 4.1.

### NEW FEATURES
- `kobo_asset_list()` gains a `minimal` argument. `minimal = TRUE` uses the
  `/api/v2/assets/minimal-list/` endpoint (KoboToolbox KPI 2.026.21 or later)
  and returns only `uid`, `name` and `deployment_status`. It was about seven
  times faster than the full listing on a 92-asset account. Older servers, which
  answer 404 for this endpoint, fall back to the full listing. Other errors are
  reported.
- `kobo_asset_list()` returns a new `created_by` column with the account that
  created each asset (KoboToolbox KPI 2.026.23 or later).

### OTHER CHANGES
- KoboToolbox removed the legacy `/api/v1/` API in KPI version 2.026.23.
  robotoolbox only uses `/api/v2/` and is not affected.
- `glue` is no longer a dependency.
- `BugReports` now points to the GitLab work items page.

robotoolbox 1.6.2 (2026-04-01)
======================

### BUG FIXES
- **Fixed repeat-group detection regression**: a recent change made entry into the
  repeat-group branch too strict and caused some genuine repeat-group forms to
  be returned as flat tables. `kobo_data()` now again relies on the form
  structure (`begin_repeat`) to identify repeat-group surveys and return `dm`
  outputs consistently.

### DOCUMENTATION
- Clarified `fields` behavior for repeat-group forms: repeat-group surveys
  always return a `dm`; main-table fields can be selected individually; top-level
  repeat groups are selected at table level; repeat groups are returned in full;
  nested repeat groups and child columns cannot be selected independently.

robotoolbox 1.6.1 (2026-03-31)
======================

### BUG FIXES
- **Fixed repeat group column assignment in `dm` objects**: child tables were inflated with
  hundreds of spurious all-NA columns belonging to other tables. The internal scope tracking
  (`kobo_form_name_to_list_()`) now correctly maps each field to its repeat group, producing
  child tables that match the KoboToolbox Excel export.
- **Fixed empty-result path in `kobo_data()`**: zero-submission assets now return the correct
  structure (`dm` for repeat-group forms, filtered columns when `fields` is set, label-based
  names when `colnames_label = TRUE`). A spurious `"NA"` column no longer appears.
- **Fixed `kobo_lang_get()` and `kobo_lang_set()` with `colnames_label = TRUE` data**: both
  functions now correctly detect and handle datasets where columns have been renamed to labels.
- **Fixed pagination edge case** where `paginate = TRUE` with a single submission produced an
  invalid `page_size` of 0.
- **Fixed `build_subs_urls()` off-by-one** that generated an extra empty API request when
  submission count was an exact multiple of `page_size`.
- **Fixed `validate_query_()` JSON validation**: now uses `RcppSimdJson::fparse()` to actually
  parse the query string instead of only checking `{`/`}` delimiters.
- **Fixed `kobo_token()` error on missing URL**: now aborts early with a clear message instead
  of a low-level curl error when no URL is provided and `KOBOTOOLBOX_URL` is unset.

### DOCUMENTATION
- Fixed `kobo_attachment_download()` roxygen: `overwrite` default corrected to `TRUE`,
  `folder` description no longer claims a default that does not exist.
- Fixed `kobo_token()` example to call `kobo_token()` instead of `kobo_setup()`.
- `README` and intro vignette now mention the `KOBOTOOLBOX_PAGE_SIZE` environment variable
  as an alternative to the `page_size` parameter.

robotoolbox 1.6.0 (2026-03-27)
======================

### BREAKING CHANGES
- **Default pagination limit reduced from 30,000 to 1,000** to comply with KoboToolbox API changes
  effective with KoboToolbox KPI version 2.026.03 (March 2026) on public servers (see https://community.kobotoolbox.org/t/important-changes-to-api-v2-assets-uid-asset-data-result-limits/74610).
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
- **Server-side filtering with `query` parameter** in `kobo_data()`: pass a MongoDB-style query
  string to filter submissions before download, e.g. `kobo_data(asset, query = '{"status": "approved"}')`.
- **Field selection with `fields` parameter** in `kobo_data()`: fetch only specific fields to
  reduce data transfer, e.g. `kobo_data(asset, fields = c("name", "age"))`.

### BUG FIXES
- Fixed `kobo_audit()` and `kobo_attachment_download()` to handle API changes where the
  `instance` field was removed from the `_attachments` structure.
- Fixed `kobo_attachment_download()` `overwrite = FALSE` check to correctly skip existing files.
- Improved error handling for non-JSON API error responses.
- Fixed `val_labels_from_form_()` to handle forms without `value_version` column.
- Fixed documentation typos in README and vignettes.

### IMPROVEMENTS
- **Significant performance gains for forms with repeat groups**: parallel form version
  fetching (3x faster) and precomputed label application (5x faster processing) reduce
  end-to-end time by ~40% on complex forms.
- `kobo_lang_get()` and `kobo_lang_set()` now accept an asset uid string in addition
  to a `kobo_asset` object, consistent with other package functions.
- Simplified internal attachment processing using `tidyr::unnest()` for cleaner code.
- Reduced sleep times between paginated API requests for better performance with 1,000 record limit.
- Removed deprecated `vcr::check_cassette_names()` from test setup.

### NOTES
- `kobo_setup()` now accepts a `page_size` parameter (default `1000`) to configure the maximum
  number of submissions per API request. Users with private servers that allow higher limits
  can set this to a larger value (e.g., `kobo_setup(..., page_size = 30000)`) to fetch more
  data per request and avoid unnecessary pagination.
- The default `page_size` is capped at 1,000 for safety on public KoboToolbox servers.
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
