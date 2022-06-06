# Contributing
This contributing guide has been derived from the `tidyverse` boilerplate and `ruODK` contributing guide.

## Non-technical contributions to robotoolbox
Feel free to [report issues](https://gitlabcom/dickoa/robotoolbox/issues):

* Bug reports are for unplanned malfunctions.
* Feature requests are for ideas and new features.

## Technical contributions to `robotoolbox`

If you would like to contribute to the code base, follow the process below.

*  [Prerequisites](#prerequisites)
*  [PR Process](#pr-process)
  *  [Fork, clone, branch](#fork-clone-branch)
  *  [Check](#check)
  *  [Style](#style)
  *  [Document](#document)
  *  [Test](#test)
  *  [NEWS](#news)
  *  [Re-check](#re-check)
*   [Resources](#resources)
*   [Code of Conduct](#code-of-conduct)

This explains how to propose a change to `robotoolbox` via a pull request using
Git and GitLab.

For more general info about contributing to `robotoolbox`, see the
[Resources](#resources) at the end of this document.

### Prerequisites
To test the package, you will need valid credentials for a Kobotoolbox server.
Create an [account request issue](https://gitlab.com/ropensci/robotoolbox/issues/new/choose).

Before you do a pull request, you should always file an issue and make sure
the maintainers agree that it is a problem, and is happy with your basic proposal
for fixing it.
If you have found a bug, follow the issue template to create a minimal
[reprex](https://www.tidyverse.org/help/#reprex).

### Checklists
Some changes have intricate internal and external dependencies, which are easy
to miss and break. These checklists aim to avoid these pitfalls.

### MR process

#### Fork, clone, branch

The first thing you'll need to do is to [fork](https://docs.gitlab.com/ee/user/project/repository/forking_workflow.html) the [`robotoolbox` GitLab repo](https://gitlab.com/dickoa/robotoolbox), and
then clone it locally. We recommend that you create a branch for each MR.

#### Check

Before changing anything, make sure the package still passes the below listed
flavours of `R CMD check` locally for you.

```r
goodpractice::goodpractice(quiet = FALSE, )
devtools::check(cran = TRUE, remote = TRUE, incoming = TRUE)
chk <- rcmdcheck::rcmdcheck(args = c("--as-cran"))
```

#### Style

Match the existing code style. This means you should follow the tidyverse
[style guide](http://style.tidyverse.org). Use the
[styler](https://CRAN.R-project.org/package=styler) package to apply the style
guide automatically.

Be careful to only make style changes to the code you are contributing. If you
find that there is a lot of code that doesn't meet the style guide, it would be
better to file an issue or a separate MR to fix that first.

#### Document

We use [roxygen2](https://cran.r-project.org/package=roxygen2), specifically with the
[Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html),
to create `NAMESPACE` and all `.Rd` files. All edits to documentation
should be done in roxygen comments above the associated function or
object. Then, run `devtools::document()` to rebuild the `NAMESPACE` and `.Rd`
files.

See the `RoxygenNote` in [DESCRIPTION](DESCRIPTION) for the version of
roxygen2 being used.

```r
spelling::spell_check_package()
spelling::spell_check_files("README.Rmd", lang = "en_US")
spelling::update_wordlist()
if (fs::file_info("README.md")$modification_time <
  fs::file_info("README.Rmd")$modification_time) {
  rmarkdown::render("README.Rmd", encoding = "UTF-8", clean = TRUE)
  if (fs::file_exists("README.html")) fs::file_delete("README.html")
}
```

#### Test

```r
devtools::test()
devtools::test_coverage()
```

#### NEWS

For user-facing changes, add a bullet to `NEWS.md` that concisely describes
the change. Small tweaks to the documentation do not need a bullet. The format
should include your GitLab username, and links to relevant issue(s)/PR(s), as
seen below.

```md
* `function_name()` followed by brief description of change (#issue-num, @your-gitlab-user-name)
```

#### Re-check

Before submitting your changes, make sure that the package either still
passes `R CMD check`, or that the warnings and/or notes have not _changed_
as a result of your edits.

```r
pkgcheck::pkgcheck()
```

#### Commit

When you've made your changes, write a clear commit message describing what
you've done. If you've fixed or closed an issue, make sure to include keywords
(e.g. `fixes #101`) at the end of your commit message (not in its
title) to automatically close the issue when the MR is merged.

#### Push and pull

Once you've pushed your commit(s) to a branch in _your_ fork, you're ready to
make the pull request. Pull requests should have descriptive titles to remind
reviewers/maintainers what the MR is about. You can easily view what exact
changes you are proposing using either the [Git diff](http://r-pkgs.had.co.nz/git.html#git-status)
view in RStudio, or the [branch comparison view](https://docs.gitlab.com/ee/user/project/merge_requests/creating_merge_requests.html)
you'll be taken to when you go to create a new MR. If the MR is related to an
issue, provide the issue number and slug in the _description_ using
auto-linking syntax (e.g. `#15`).

#### Check the docs

#### Review, revise, repeat

The latency period between submitting your MR and its review may vary.
When a maintainer does review your contribution, be sure to use the same
conventions described here with any revision commits.

### Resources

*  [Happy Git and Gitlab for the useR](http://happygitwithr.com/) by Jenny Bryan.
*  [Contribute to the tidyverse](https://www.tidyverse.org/contribute/) covers
   several ways to contribute that _don't_ involve writing code.
*  [Contributing Code to the Tidyverse](http://www.jimhester.com/2017/08/08/contributing/) by Jim Hester.
*  [R packages](http://r-pkgs.had.co.nz/) by Hadley Wickham.

### Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to
abide by its terms.

## Maintaining `robotoolbox`

## Package maintenance
