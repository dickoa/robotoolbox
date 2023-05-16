robotoolbox 1.2.0.9002
======================

### Improvement
- Added a fonction `kobo_audit` to get all the audit logs data from a form with audit enabled. It comes with a vignette showing how to read audit logs.
- Added a fonction `set_names_from_varlabel` to have variable labels as variables names and have similar export as the Kobotoolbox Excel export tools.
- Added a logical parameter `select_multiple_label` to `kobo_data` in order to turn `select_multiple` values to labels. It's useful if you want to work directly with labels in `select_multiple`

robotoolbox 1.1.0.9004
======================

### Bug fix

- Fixing the following issue: Using `true` or `false` as values in `select_one` breaks the labels.
- Revert to `type.convert` because of flexibility on boolean/logical
- Add a fonction to merge the variables from different groups due to different versions of the form used during data collection.


robotoolbox 1.1.0.9000
======================

### Improvement

- `robotoolbox` provides WKT columns for `geopoint`, `geotrace` and `geoshape` question types, for easier spatial data analysis.
- Remove unnecessary list-columns and pre-process `_validation_status` column.

### BUG FIX

- Fix a bug in `kobo_form` when you have a `list_name` in the `choices` tab but no associated question in the `survey` tab.

robotoolbox 1.0.9.9002
======================

### BUG FIX

- It is now possible to match submissions to form based on the version of the form used even when you're not the owner of the project. This new feature work best with `Kobotoolbox kpi` version `2.023.12` and up.
- When you have child table with differents parent tables, split it and change names.

### Improvement
- Update `purrr` to 1.0.1

robotoolbox 1.0.7.9001
======================

### Improvement

- `kobo_data` and `kobo_submissions` are faster for surveys with `repeat group`.
-  Add variables as labels when a variable label is missing
-  Custom labels for `select_multiple` dummy variables


robotoolbox 1.0.6.9000
======================

### BUG FIX

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
