simstudy
================

# simstudy

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R build
status](https://github.com/kgoldfeld/simstudy/workflows/R-CMD-check/badge.svg?branch=main)](https://github.com/kgoldfeld/simstudy/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/simstudy)](https://CRAN.R-project.org/package=simstudy)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/simstudy)](https://CRAN.R-project.org/package=simstudy)
[![codecov](https://codecov.io/gh/kgoldfeld/simstudy/branch/main/graph/badge.svg)](https://codecov.io/gh/kgoldfeld/simstudy)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- badges: end -->

The `simstudy` package is collection of functions that allow users to
generate simulated data sets in order to explore modeling techniques or
better understand data generating processes. The user defines the
distributions of individual variables, specifies relationships between
covariates and outcomes, and generates data based on these
specifications. The final data sets can represent randomized control
trials, repeated measure designs, cluster randomized trials, or
naturally observed data processes. Other complexities that can be added
include survival data, correlated data, factorial study designs, step
wedge designs, and missing data processes.

Simulation using `simstudy` has two fundamental steps. The user (1)
**defines** the data elements of a data set and (2) **generates** the
data based on these definitions. Additional functionality exists to
simulate observed or randomized **treatment assignment/exposures**, to
create **longitudinal/panel** data, to create
**multi-level/hierarchical** data, to create datasets with **correlated
variables** based on a specified covariance structure, to **merge**
datasets, to create data sets with **missing** data, and to create
non-linear relationships with underlying **spline** curves.

The overarching philosophy of `simstudy` is to create data generating
processes that mimic the typical models used to fit those types of data.
So, the parameterization of some of the data generating processes may
not follow the standard parameterizations for the specific
distributions. For example, in `simstudy` *gamma*-distributed data are
generated based on the specification of a mean \(\mu\) (or \(log(\mu)\))
and a dispersion \(d\), rather than shape \(\alpha\) and rate \(\beta\)
parameters that more typically characterize the *gamma* distribution.
When we estimate the parameters, we are modeling \(\mu\) (or some
function of \((\mu)\)), so we should explicitly recover the `simstudy`
parameters used to generate the model - illuminating the relationship
between the underlying data generating processes and the models.

## Installation

You can install the released version of simstudy from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("simstudy")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kgoldfeld/simstudy")
```

## Example

Here is some simple sample code, much more in the vignettes:

``` r
library(simstudy)

def <- defData(varname="x", formula = 10, variance = 2)
def <- defData(def, varname="y", formula = "3 + 0.5 * x", variance = 1)
dd <- genData(250, def)

dd <- trtAssign(dd, nTrt = 4, grpName = "grp", balanced = TRUE)

dd
#>       id         x        y grp
#>   1:   1  9.521113 7.908014   4
#>   2:   2 12.393069 8.474426   4
#>   3:   3  9.268559 8.806388   1
#>   4:   4  8.541196 7.169543   3
#>   5:   5  9.709809 7.575377   2
#>  ---                           
#> 246: 246  9.858810 8.321467   4
#> 247: 247  8.629382 8.151692   4
#> 248: 248  9.624788 8.770344   2
#> 249: 249 11.215969 6.837802   3
#> 250: 250  9.069658 6.427107   3
```

## Code of Conduct

Please note that the simstudy project is released with a [Contributor
Code of
Conduct](https://kgoldfeld.github.io/simstudy/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
