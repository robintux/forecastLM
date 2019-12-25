# USgrid 

<!-- badges: start -->
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/USgrid)](https://cran.r-project.org/package=forecastLM)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

The forecastLM R package is a wrapper for the lm functions, providing a framework for forecasting regular time series data with a linear regression model.

## Installation

Currently, the package available only on Github version:

``` r
remotes::install_github("RamiKrispin/USgrid")
```

## Examples

Forecasting the monthly demand for natural gas in the US:

``` r
# Loading the data from the TSstudio package
library(TSstudio)
data("USgas")

ts_plot(USgas, 
        title = "The Monthly Demand for Natural Gas in the US",
        Ytitle = "Billion Cubic Feet",
        Xtitle = "Source: U.S. Bureau of Transportation Statistics, Natural Gas Consumption [NATURALGAS]",
        Ygrid = TRUE,
        Xgrid = TRUE)
```

<img src="man/figures/USgas_plot.png" width="100%" />
