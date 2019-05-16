## Resubmission 2019MAY16

This is a resubmission of version 0.1.13

Developed with R Version 3.6.0.

I did not get any notes from win_build and all rhub checks passed.

###

There is an "Additional issue" listed on the "CRAN Package Check Results": noLD

The error has been resolved by adding a tolerance argument to the incriminating code.  I rechecked using rhub::check(platform = "debian-gcc-devel-nold"). The check results were "OK".

