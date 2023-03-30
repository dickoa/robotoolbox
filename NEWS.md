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
