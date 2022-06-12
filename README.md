
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

## Caveats and limitations

`robotoolbox` uses the `labelled` package to manipulate `labels` for
`select_one` question. For `select_multiple` questions, dummy columns
are used. They are based on values not labels. Please feel free to
submit an issue, if you want to have `labels` for `select_multiple`.

`robotoolbox` uses the `Kobotoolbox` API v2. Using the API, if you don’t
own the project, [it is not currently possible to access all versions of
the form used to collect the
data](https://github.com/kobotoolbox/kpi/issues/1164). You can just
access the latest version of the form. It has an impact on how
`robotoolbox` can map the raw data and the information from the
associated form (labels, languages, etc.).

## Meta

-   Please [report any issues or
    bugs](https://gitlab.com/dickoa/robotoolbox/-/issues).
-   License: MIT
-   Please note that this project is released with a [Contributor Code
    of Conduct](CODE_OF_CONDUCT.md). By participating in this project
    you agree to abide by its terms.
