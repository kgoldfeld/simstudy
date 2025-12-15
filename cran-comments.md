## Submission 20251215

This is a second resubmission for version 0.9.1

Developed with R Version 4.5.2

The test failure reported on CRAN (MKL) was caused by a numerical rounding issue 
leading to a non-symmetric correlation matrix. This has been fixed by ensuring 
correlation matrices are properly symmetrized.

In addition, fixed a unit test that incorrectly assumed exact agreement 
between empirical and target correlation matrices. The test has been updated 
to allow for expected Monte Carlo variability and BLAS/LAPACK–dependent 
numerical differences, which should resolve the MKL-specific test failure.

Checks using devtools::check(remote = TRUE, manual = TRUE) and 
devtools::check_win_devel() pass.
