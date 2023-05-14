
<!-- README.md is generated from README.Rmd. Please edit that file -->

# robotoolbox <img src="man/figures/robotoolbox_hex.png" align="right" width="140" />

[![Project Status: Active - Initial development is in progress, but
there has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![GitLab CI Build
Status](https://gitlab.com/dickoa/robotoolbox/badges/master/pipeline.svg)](https://gitlab.com/dickoa/robotoolbox/-/pipelines)
[![Codecov Code
Coverage](https://codecov.io/gl/dickoa/robotoolbox/branch/master/graph/badge.svg)](https://app.codecov.io/gl/dickoa/robotoolbox)
[![CRAN
status](https://www.r-pkg.org/badges/version/robotoolbox)](https://CRAN.R-project.org/package=robotoolbox)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

`robotoolbox` is an R client to access data from
[KoboToolbox](https://www.kobotoolbox.org/).

## Installation

This package is not on yet on CRAN and to install it, you will need the
[`remotes`](https://github.com/r-lib/remotes) package. You can get
`robotoolbox` from Gitlab or Github (mirror)

``` r
## install.packages("remotes")
remotes::install_gitlab("dickoa/robotoolbox")
```

## robotoolbox: A quick tutorial

The `robotoolbox` package is a client to
[`KoboToolbox API v2`](https://support.kobotoolbox.org/api.html). You
will need to set your API token and specify the `KoboToolbox` server
URL. The easiest way to set up `robotoolbox` is to store the token and
the url in your `.Renviron` which is automatically read by `R` on
startup.

### Getting the API token

You can retrieve your `API token` following the instruction in the
official API documentation: <https://support.kobotoolbox.org/api.html>.

You can also get your token directly from `R` using the `kobo_token`
function.

``` r
kobo_token(username = "xxxxxxxxx",
           password = "xxxxxxxxx",
           url = "https://kobo.unhcr.org")
```

### Setup your session

You can either edit directly the `.Renviron` file or access it by
calling `usethis::edit_r_environ()` (assuming you have the `usethis`
package installed) and entering the following two lines:

``` bash
KOBOTOOLBOX_URL="https://kobo.unhcr.org/"
KOBOTOOLBOX_TOKEN=xxxxxxxxxxxxxxxxxxxxxxxxxx
```

Or use directly the `kobo_setup` function

``` r
kobo_setup(url = "https://kobo.unhcr.org",
           token = "xxxxxxxxxxxxxxxxxxxxxxxxxx")
```

Settings can be checked using `kobo_settings` function.

``` r
library("robotoolbox")
kobo_settings()
## <robotoolbox settings>
##    KoboToolbox URL: https://kobo.unhcr.org/
##    KoboToolbox API Token: xxxxxxxxxxxxxxxxxxxxxxxxxx
```

With the settings done, it is possible to list all `assets` (e.g form,
project, etc) for the account associated to the token and url supplied.

### Access data

``` r
library("dplyr")
l <- kobo_asset_list()
l
# A tibble: 24 x 7
   uid     name     asset_type owner_username date_created
   <chr>   <chr>    <chr>      <chr>          <dttm>
 1 b9kgvd… Proj_A1… survey     xxxxxxxxxxxxx… 2020-04-27 20:34:23
 2 aRFJMp… Proj_A2… survey     xxxxxxxxxxxxx… 2020-04-27 21:21:12
 3 a6qMG7… Proj_A3… survey     xxxxxxxxxxxxx… 2021-05-25 16:59:08
 4 azhrVs… Proj_A4… survey     xxxxxxxxxxxxx… 2021-05-25 13:59:46
 5 aReR58… Proj_A5… survey     xxxxxxxxxxxxx… 2021-06-07 09:15:53
 6 aWaoqy… Proj_A6… survey     xxxxxxxxxxxxx… 2021-05-29 10:46:09
 7 aABU3C… Proj_A7… survey     xxxxxxxxxxxxx… 2020-11-28 15:00:10
 8 aaznyX… Proj_A9… survey     xxxxxxxxxxxxx… 2020-11-28 14:28:48
 9 aCVr2Q… Proj_A9… survey     xxxxxxxxxxxxx… 2021-05-25 13:30:24
10 aPxNao… Proj_A10… survey    xxxxxxxxxxxxx… 2020-04-27 11:37:34
# … with 14 more rows, and 3 more variables:
#   date_modified <dttm>, submissions <int>
glimpse(l)
$ uid            <chr> "b9kgvd7AXQCmo5qyUOBEl", "aRfJMpTSGRLzZ…"
$ name           <chr> "Proj_A1", "Proj_A2", "Proj_A3", "Proj_A…"
$ asset_type     <chr> "survey", "survey", "survey", "survey", …
$ owner_username <chr> "xxxxxxxxxxxxxx", "xxxxxxxxxxxxxxx", "xx…"
$ date_created   <dttm> 2020-04-27 20:34:23, 2020-04-27 21:21:1…
$ date_modified  <dttm> 2021-06-17 01:52:57, 2021-06-17 01:52:5…
$ submissions    <int> 2951, 2679, 2, 1, 0, 0, 287, 73, 0, 274,…
```

The list of `assets` is a `tibble`, you can filter it to select the form
`uuid` that uniquely identify the project you want to open. The function
`kobo_asset` can then be used to get the `asset` from the `uuid`.

``` r
uid <- l |>
  filter(name == "proj_A1") |>
  pull(uid) |>
  first()
uid
## b9agvd9AXQCmo5qyUOBEl

asset <- kobo_asset(uid)
asset
## <robotoolbox asset>  b9agvd9AXQCmo5qyUOBEl
##   Asset Name: proj_A1
##   Asset Type: survey
##   Created: 2021-05-10 07:47:53
##   Last modified: 2021-08-16 12:35:50
##   Submissions: 941
```

Now with the selected `asset`, we can extract the `submissions` using
the `kobo_submissions` function. The `kobo_data` can also be used
instead, it’s an alias of `kobo_submissions`.

``` r
df <- kobo_submissions(asset) ## or df <-  kobo_data(asset)
glimpse(df)
## Rows: 941
## Columns: 17
## $ id                                                         <int> …
## $ start                                                      <dttm> …
## $ end                                                        <dttm> …
## $ today                                                      <date> …
## $ deviceid                                                   <chr> …
## $ test                                                       <chr+lbl> …
## $ round                                                      <date> …
## $ effective_date                                             <date> …
## $ collect_type                                               <chr+lbl> …
## $ covid_module                                               <chr+lbl> …
## $ country                                                    <chr+lbl> …
## $ interviewer_id                                             <chr> …
## $ respondent_is_major                                        <chr+lbl> …
## $ consent                                                    <chr+lbl> …
## $ admin_level_1                                              <chr+lbl> …
## $ admin_level_2                                              <chr+lbl> …
## $ admin_level_3                                              <chr+lbl> …
```

### Languages

`robotoolbox` uses the R package
[`labelled`](https://larmarange.github.io/labelled/) to provide tools to
manipulate variable labels and value labels. You can learn more about
this here:

<https://dickoa.gitlab.io/robotoolbox/articles/language-labelled.html>

### Repeating groups

Repeating groups associate multiple records to a single record in the
`main` table. It’s used to group questions that need to be answered
repeatedly. The package [`dm`](https://cynkra.github.io/dm/) is used to
model such relationship and allow you to safely query and join such
linked data for your analysis.

<https://dickoa.gitlab.io/robotoolbox/articles/repeat-group-data.html>

### Spatial data

`Kobotoolbox` provides three types of question to record spatial data:
`geopoint` for points, `geotrace` for lines and `geoshape` to map close
polygons. `robotoolbox` associates to each spatial column a
[`WKT`](https://libgeos.org/specifications/wkt/) column. It provides a
simple way to use it with various GIS software and `R` package for
spatial data analysis. The `sf` package is the standard for spatial
vector data handling and visualization.

<https://dickoa.gitlab.io/robotoolbox/articles/spatial-data.html>

### Audit logging data

`Kobotoolbox` comes with a feature that records all activities related
to a form submission in a log file. The audit logging metadata is useful
for data quality control, security and workflow management. The
`kobo_audit` function allow you to read `Kobotoolbox` audit logs file.

You can learn more in the following vignette:

<https://dickoa.gitlab.io/robotoolbox/articles/audit-data.html>

### Labels for select_multiple columns and variable labels as column names

We can now get `labels` instead of values for `select_multiple`
questions with `robotoolbox`. Let’s show this new feature, with the
following example:

``` r
data_sm <- kobo_data(uid)
glimpse(data_sm)
```

    #>  Rows: 5
    #>  Columns: 21
    #>  $ start                <chr> "2022-05-09T18:31:40.096-00:00", "2022-05-09T18:31:53.670-00:00", "…
    #>  $ end                  <chr> "2022-05-09T18:35:12.810-00:00", "2022-05-09T18:34:59.061-00:00", "…
    #>  $ today                <chr> "2022-05-09", "2022-05-09", "2022-05-09", "2022-05-09", "2022-05-09"
    #>  $ full_name            <chr> "Rufus", "Romulus", "Remus", "Joe", "Moh"
    #>  $ pet_type             <chr> "3 4", "4", "5", NA, "3 4 5"
    #>  $ pet_type_1           <int> 0, 0, 0, NA, 0
    #>  $ pet_type_2           <int> 0, 0, 0, NA, 0
    #>  $ pet_type_3           <int> 1, 0, 0, NA, 1
    #>  $ pet_type_4           <int> 1, 1, 0, NA, 1
    #>  $ pet_type_5           <int> 0, 0, 1, NA, 1
    #>  $ `_id`                <int> 20939261, 20939265, 20939278, 20939288, 20939301
    #>  $ instanceID           <chr> "uuid:147d4f30-7459-42f7-818f-b44f47b2cca7", "uuid:6f67ede0-c594-4a…
    #>  $ deprecatedID         <chr> "uuid:6840ad57-d9f7-4557-b1f2-11af21e5b0cd", "uuid:3cbdc3ec-bd0a-4a…
    #>  $ uuid                 <chr> "5c0d08e4deda4a7fbc9634f5e8aba62f", "5c0d08e4deda4a7fbc9634f5e8aba6…
    #>  $ `__version__`        <chr> "vjPe5qiVxTmyviYSrQE3x4", "vjPe5qiVxTmyviYSrQE3x4", "vjPe5qiVxTmyvi…
    #>  $ `_xform_id_string`   <chr> "atbUaNGu5PWR2u4tNDsYaH", "atbUaNGu5PWR2u4tNDsYaH", "atbUaNGu5PWR2u…
    #>  $ `_uuid`              <chr> "147d4f30-7459-42f7-818f-b44f47b2cca7", "6f67ede0-c594-4a28-bf7b-c4…
    #>  $ `_status`            <chr> "submitted_via_web", "submitted_via_web", "submitted_via_web", "sub…
    #>  $ `_submission_time`   <chr> "2022-05-09T18:32:03", "2022-05-09T18:32:10", "2022-05-09T18:32:44"…
    #>  $ `_validation_status` <int> NA, NA, NA, NA, NA
    #>  $ `_submitted_by`      <int> NA, NA, NA, NA, NA

We noticed that the column `pet_type` contains values (1 to 5).

Now let’s set the new `select_multiple_label` to `TRUE` to read the
data.

``` r
data_sm_label <- kobo_data(uid,
                           select_multiple_label = TRUE)
glimpse(data_label)
```

``` r
glimpse(data_sm_label)
#>  Rows: 5
#>  Columns: 21
#>  $ start                <chr> "2022-05-09T18:31:40.096-00:00", "2022-05-09T18:31:53.670-00:00", "…
#>  $ end                  <chr> "2022-05-09T18:35:12.810-00:00", "2022-05-09T18:34:59.061-00:00", "…
#>  $ today                <chr> "2022-05-09", "2022-05-09", "2022-05-09", "2022-05-09", "2022-05-09"
#>  $ full_name            <chr> "Rufus", "Romulus", "Remus", "Joe", "Moh"
#>  $ pet_type             <chr> "dog cat", "cat", "turtle", NA, "dog cat turtle"
#>  $ pet_type_1           <int> 0, 0, 0, NA, 0
#>  $ pet_type_2           <int> 0, 0, 0, NA, 0
#>  $ pet_type_3           <int> 1, 0, 0, NA, 1
#>  $ pet_type_4           <int> 1, 1, 0, NA, 1
#>  $ pet_type_5           <int> 0, 0, 1, NA, 1
#>  $ `_id`                <int> 20939261, 20939265, 20939278, 20939288, 20939301
#>  $ instanceID           <chr> "uuid:147d4f30-7459-42f7-818f-b44f47b2cca7", "uuid:6f67ede0-c594-4a…
#>  $ deprecatedID         <chr> "uuid:6840ad57-d9f7-4557-b1f2-11af21e5b0cd", "uuid:3cbdc3ec-bd0a-4a…
#>  $ uuid                 <chr> "5c0d08e4deda4a7fbc9634f5e8aba62f", "5c0d08e4deda4a7fbc9634f5e8aba6…
#>  $ `__version__`        <chr> "vjPe5qiVxTmyviYSrQE3x4", "vjPe5qiVxTmyviYSrQE3x4", "vjPe5qiVxTmyvi…
#>  $ `_xform_id_string`   <chr> "atbUaNGu5PWR2u4tNDsYaH", "atbUaNGu5PWR2u4tNDsYaH", "atbUaNGu5PWR2u…
#>  $ `_uuid`              <chr> "147d4f30-7459-42f7-818f-b44f47b2cca7", "6f67ede0-c594-4a28-bf7b-c4…
#>  $ `_status`            <chr> "submitted_via_web", "submitted_via_web", "submitted_via_web", "sub…
#>  $ `_submission_time`   <chr> "2022-05-09T18:32:03", "2022-05-09T18:32:10", "2022-05-09T18:32:44"…
#>  $ `_validation_status` <int> NA, NA, NA, NA, NA
#>  $ `_submitted_by`      <int> NA, NA, NA, NA, NA
```

We can now see the `labels` instead of the `values` (`dog`, `cat`, etc.)
for the column `pet_type`.

Variable labels has been improved for all the dummy variables related to
the `select_multiple` question (whether or not you use
`select_multiple_label`).

``` r
library(labelled)
var_label(data_sm_label)
#>  $start
#>  [1] "start"
#>  
#>  $end
#>  [1] "end"
#>  
#>  $today
#>  [1] "today"
#>  
#>  $full_name
#>  [1] "What is your name?"
#>  
#>  $pet_type
#>  [1] "What type of pet do you own ?"
#>  
#>  $pet_type_1
#>  [1] "What type of pet do you own ?::rabbit"
#>  
#>  $pet_type_2
#>  [1] "What type of pet do you own ?::chicken"
#>  
#>  $pet_type_3
#>  [1] "What type of pet do you own ?::dog"
#>  
#>  $pet_type_4
#>  [1] "What type of pet do you own ?::cat"
#>  
#>  $pet_type_5
#>  [1] "What type of pet do you own ?::turtle"
#>  
#>  $`_id`
#>  [1] "_id"
#>  
#>  $instanceID
#>  [1] "instanceID"
#>  
#>  $deprecatedID
#>  [1] "deprecatedID"
#>  
#>  $uuid
#>  [1] "uuid"
#>  
#>  $`__version__`
#>  [1] "__version__"
#>  
#>  $`_xform_id_string`
#>  [1] "_xform_id_string"
#>  
#>  $`_uuid`
#>  [1] "_uuid"
#>  
#>  $`_status`
#>  [1] "_status"
#>  
#>  $`_submission_time`
#>  [1] "_submission_time"
#>  
#>  $`_validation_status`
#>  [1] "_validation_status"
#>  
#>  $`_submitted_by`
#>  [1] "_submitted_by"
```

Finally, with the function `set_names_from_varlabel` you can replace all
the columns (not just `select_multiple`) by their label

``` r
data_sm_label |>
  set_names_from_varlabel() |>
  glimpse()
#>  Rows: 5
#>  Columns: 21
#>  $ start                                    <chr> "2022-05-09T18:31:40.096-00:00", "2022-05-09T18…
#>  $ end                                      <chr> "2022-05-09T18:35:12.810-00:00", "2022-05-09T18…
#>  $ today                                    <chr> "2022-05-09", "2022-05-09", "2022-05-09", "2022…
#>  $ `What is your name?`                     <chr> "Rufus", "Romulus", "Remus", "Joe", "Moh"
#>  $ `What type of pet do you own ?`          <chr> "dog cat", "cat", "turtle", NA, "dog cat turtle"
#>  $ `What type of pet do you own ?::rabbit`  <int> 0, 0, 0, NA, 0
#>  $ `What type of pet do you own ?::chicken` <int> 0, 0, 0, NA, 0
#>  $ `What type of pet do you own ?::dog`     <int> 1, 0, 0, NA, 1
#>  $ `What type of pet do you own ?::cat`     <int> 1, 1, 0, NA, 1
#>  $ `What type of pet do you own ?::turtle`  <int> 0, 0, 1, NA, 1
#>  $ `_id`                                    <int> 20939261, 20939265, 20939278, 20939288, 20939301
#>  $ instanceID                               <chr> "uuid:147d4f30-7459-42f7-818f-b44f47b2cca7", "u…
#>  $ deprecatedID                             <chr> "uuid:6840ad57-d9f7-4557-b1f2-11af21e5b0cd", "u…
#>  $ uuid                                     <chr> "5c0d08e4deda4a7fbc9634f5e8aba62f", "5c0d08e4de…
#>  $ `__version__`                            <chr> "vjPe5qiVxTmyviYSrQE3x4", "vjPe5qiVxTmyviYSrQE3…
#>  $ `_xform_id_string`                       <chr> "atbUaNGu5PWR2u4tNDsYaH", "atbUaNGu5PWR2u4tNDsY…
#>  $ `_uuid`                                  <chr> "147d4f30-7459-42f7-818f-b44f47b2cca7", "6f67ed…
#>  $ `_status`                                <chr> "submitted_via_web", "submitted_via_web", "subm…
#>  $ `_submission_time`                       <chr> "2022-05-09T18:32:03", "2022-05-09T18:32:10", "…
#>  $ `_validation_status`                     <int> NA, NA, NA, NA, NA
#>  $ `_submitted_by`                          <int> NA, NA, NA, NA, NA
```

## Meta

- Please [report any issues or
  bugs](https://gitlab.com/dickoa/robotoolbox/-/issues).
- License: MIT
- Please note that this project is released with a [Contributor Code of
  Conduct](CODE_OF_CONDUCT.md). By participating in this project you
  agree to abide by its terms.
