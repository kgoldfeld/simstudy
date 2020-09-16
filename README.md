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
#>       id         x         y grp
#>   1:   1  9.156749  8.895291   4
#>   2:   2 14.101606  9.400687   2
#>   3:   3  9.756959  8.026519   2
#>   4:   4  7.968584  7.937833   3
#>   5:   5  9.325378  7.762094   4
#>  ---                            
#> 246: 246  8.230012  6.794570   1
#> 247: 247  8.976130  7.943784   4
#> 248: 248 11.958703 11.331082   4
#> 249: 249 11.618778  8.406787   3
#> 250: 250 13.104799 10.004097   4
```

## Code of Conduct

Please note that the simstudy project is released with a [Contributor
Code of
Conduct](https://kgoldfeld.github.io/simstudy/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
