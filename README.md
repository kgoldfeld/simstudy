
# simstudy

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R build
status](https://github.com/kgoldfeld/simstudy/workflows/R-CMD-check/badge.svg?branch=restructure)](https://github.com/kgoldfeld/simstudy/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/simstudy)](https://CRAN.R-project.org/package=simstudy)
[![codecov](https://codecov.io/gh/kgoldfeld/simstudy/branch/restructure/graph/badge.svg)](https://codecov.io/gh/kgoldfeld/simstudy)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

The simstudy package is collection of functions that allow users to
generate simulated data sets in order to explore modeling techniques or
better understand data generating processes. The user specifies a set of
relationships between covariates, and generates data based on these
specifications. The final data sets can represent data from randomized
control trials, repeated measure (longitudinal) designs, and cluster
randomized trials. Missingness can be generated using various mechanisms
(MCAR, MAR, NMAR).

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

Here is some simple sample code, much more in the vignette:

``` r
library(simstudy)

def <- defData(varname="x", formula = 10, variance = 2)
def <- defData(def, varname="y", formula = "3 + 0.5 * x", variance = 1)
dt <- genData(250, def)

dt <- trtAssign(dt, nTrt = 4, grpName = "grp", balanced = TRUE)

dt
#>       id         x        y grp
#>   1:   1  9.750958 7.253067   1
#>   2:   2 10.290396 7.459266   1
#>   3:   3  8.475827 4.762035   4
#>   4:   4  8.835629 7.875204   4
#>   5:   5 10.996427 8.740167   3
#>  ---                           
#> 246: 246  9.524532 7.303001   2
#> 247: 247 10.038139 7.591058   1
#> 248: 248  6.330682 6.872658   4
#> 249: 249  8.340167 7.684489   2
#> 250: 250 11.634552 8.893969   2
```
