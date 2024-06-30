## Submission 1.4

> Dear maintainer,
>
> This has a DESCRIPTION file with an invalid Language field.
>
> According to 'Writing R Extensions',
>
> A ‘Language’ field can be used to indicate if the package
> documentation is not in English: this should be a comma-separated list
> of standard (not private use or grandfathered) IETF language tags as
> currently defined by RFC 5646 (https://www.rfc-editor.org/rfc/rfc5646,
>
> see also https://en.wikipedia.org/wiki/IETF_language_tag), i.e., use
>
> language subtags which in essence are 2-letter ISO 639-1
> (https://en.wikipedia.org/wiki/ISO_639-1) or 3-letter ISO 639-3
>
> (https://en.wikipedia.org/wiki/ISO_639-3) language codes.
>
>
> So e.g. en_US or en_GB are invalid, and should be replaced by en-US or
> en-GB, respectively.
>
> Please fix with your next regular package update.
The language field was removed since the documentation is in English.

## Resubmission 1.3.2

* Fixed.

> Please always write package names, software names and API (application
programming interface) names in single quotes in your title as well.
e.g: --> 'KoboToolbox'

* Fixed.

> Please add \value to .Rd files regarding exported methods and explain
the functions results in the documentation. Please write about the
structure of the output (class) and also what the output means. (If a
function does not return a value, please document that too, e.g.
\value{No return value, called for side effects} or similar).
Missing Rd-tags:
      set_names_from_varlabel.Rd: \value

## Resubmission 1.3.1
This is a resubmission. In this version:

* I fixed the following NOTE: Wrong URI (CODE_OF_CONDUCT.md) on the README.md file

## Test environments

- Arch Linux x86_64 GNU/Linux, kernel 6.3.5-arch1-1
- Windows R-devel 2023-05-31 r84480 ucrt

## R CMD check results

0 errors | 0 warnings | 0 notes
