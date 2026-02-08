## Submission 2026-02-08

This is a submission of version 0.9.2.

## Test environments
* macOS (R 4.5.2)
* win-builder (devel)

## R CMD check results
0 errors | 0 warnings | 0 notes

## Comments
The test failure previously reported on CRAN (MKL) was due to Monte Carlo variability.
The sample size was increased and a fixed random seed was added to reduce the
chance of recurrence.

All checks pass using devtools::check(remote = TRUE, manual = TRUE) and
devtools::check_win_devel().
