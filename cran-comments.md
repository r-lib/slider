## Test environments
* local R installation, R 3.6.0
* ubuntu 16.04 (on travis-ci), devel, R 3.6.0
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

## 0.1.3 Submission

Fixes a few bugs, and maintains compatibility with vctrs.

## 0.1.2 Submission

Updates to be compatible with vctrs 0.2.4.

## 0.1.1 Resubmission

vctrs 0.2.3 is now on CRAN, so the gcc10 warning should no longer occur.

## 0.1.1 Submission

Fixes a multiple declaration C issue uncovered by CRAN's check under gcc10. We
link against the vctrs C API, which also has this issue, so currently gcc10 is going to continue to fail. That is being tracked by the following issue, and we will submit a patch for vctrs shortly. https://github.com/r-lib/vctrs/pull/797

## 0.1.0 Resubmission

### Review 1 - 2020-02-06

> If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

There are no references for the package.

> Should the year in the LICENSE file be updated?

I have updated the license year to 2020.

## 0.1.0 Submission
