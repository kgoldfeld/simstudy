simstudy
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R build
status](https://github.com/kgoldfeld/simstudy/workflows/R-CMD-check/badge.svg?branch=main)](https://github.com/kgoldfeld/simstudy/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/simstudy)](https://CRAN.R-project.org/package=simstudy)
[![status](https://joss.theoj.org/papers/10.21105/joss.02763/status.svg)](https://joss.theoj.org/papers/10.21105/joss.02763)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/simstudy)](https://CRAN.R-project.org/package=simstudy)
[![codecov](https://app.codecov.io/gh/kgoldfeld/simstudy/branch/main/graph/badge.svg)](https://app.codecov.io/gh/kgoldfeld/simstudy)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

The `simstudy` package is a collection of functions that allow users to
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
generated based on the specification of a mean μ (or log(μ)) and a
dispersion \(d\), rather than shape α and rate β parameters that more
typically characterize the *gamma* distribution. When we estimate the
parameters, we are modeling μ (or some function of μ), so we should
explicitly recover the `simstudy` parameters used to generate the model,
thus illuminating the relationship between the underlying data
generating processes and the models. For more details on the package,
use cases, examples, and function reference see the [documentation
page](https://kgoldfeld.github.io/simstudy/articles/simstudy.html).

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
set.seed(1965)

def <- defData(varname="x", formula = 10, variance = 2, dist = "normal")
def <- defData(def, varname="y", formula = "3 + 0.5 * x", variance = 1, dist = "normal")
dd <- genData(250, def)

dd <- trtAssign(dd, nTrt = 4, grpName = "grp", balanced = TRUE)

dd
#>       id         x        y grp
#>   1:   1 11.191960 8.949389   4
#>   2:   2 10.418375 7.372060   4
#>   3:   3  8.512109 6.925844   3
#>   4:   4 11.361632 9.850340   4
#>   5:   5  9.928811 6.515463   4
#>  ---                           
#> 246: 246  8.220609 7.898416   2
#> 247: 247  8.531483 8.681783   2
#> 248: 248 10.507370 8.552350   3
#> 249: 249  8.621339 6.652300   1
#> 250: 250  9.508164 7.083845   3
```

## Contributing & Support

If you find a bug or need help, please file an issue with a
[reprex](https://www.tidyverse.org/help/) on
[Github](https://github.com/kgoldfeld/simstudy/issues). We are happy to
accept contributions to simstudy. More information on how to propose
changes or fix bugs can be found
[here](https://kgoldfeld.github.io/simstudy/CONTRIBUTING.html).

## Code of Conduct

Please note that the simstudy project is released with a [Contributor
Code of
Conduct](https://kgoldfeld.github.io/simstudy/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
