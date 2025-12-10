## Submission 20251214

This is a resubmission for version 0.9.1

Developed with R Version 4.5.2

The test failure reported on CRAN (MKL) was caused by a numerical rounding issue 
leading to a non-symmetric correlation matrix. This has been fixed by ensuring 
correlation matrices are properly symmetrized.

Checks using devtools::check(remote = TRUE, manual = TRUE) and 
devtools::check_win_devel() pass.
