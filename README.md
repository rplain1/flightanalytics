
<!-- README.md is generated from README.Rmd. Please edit that file -->

# flightanalytics

<!-- badges: start -->
<!-- badges: end -->

The goal of flightanalytics is to …

## Installation

You can install the development version of flightanalytics from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("rplain1/flightanalytics")

## Example

``` r
library(flightanalytics)
get_pgds_arrival_curve()
#> # A tibble: 25 × 4
#>    minutes_prior peak_domestic_8am off_peak_domestic international
#>    <chr>                     <dbl>             <dbl>         <dbl>
#>  1 10                         0.8               0.06          0.22
#>  2 20                         0.26              0.3           0.11
#>  3 30                         0.42              0.48          0.15
#>  4 40                         1.1               0.98          0.28
#>  5 50                         3.08              2.1           0.61
#>  6 60                         6.71              4.03          1.32
#>  7 70                        10.3               6.19          3.08
#>  8 80                        12.9               8.16          5.13
#>  9 90                        13.5               9.59          7.37
#> 10 100                       12.8              10.2           8.93
#> # ℹ 15 more rows
```
