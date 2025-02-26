
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{splitteR}`

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of `{splitteR}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
splitteR::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-02-26 11:55:40 CET"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading splitteR
#> ── R CMD check results ──────────────────────────────── splitteR 0.0.0.9000 ────
#> Duration: 32.6s
#> 
#> ❯ checking for future file timestamps ... NOTE
#>   unable to verify current time
#> 
#> ❯ checking dependencies in R code ... NOTE
#>   import '::' lub ':::' nie jest zadeklarowany z: 'HaDeX'
#> 
#> ❯ checking R code for possible problems ... NOTE
#>   mod_input_data_server : <anonymous>: no visible binding for global
#>     variable 'example_data_alpha'
#>   mod_input_data_server : <anonymous>: no visible global function
#>     definition for 'toggle_id'
#>   mod_input_data_server : <anonymous>: no visible binding for global
#>     variable 'dat'
#>   mod_input_data_server : <anonymous>: no visible binding for global
#>     variable 'str_path'
#>   Undefined global functions or variables:
#>     dat example_data_alpha str_path toggle_id
#> 
#> 0 errors ✔ | 0 warnings ✔ | 3 notes ✖
```

``` r
covr::package_coverage()
#> Error in loadNamespace(x): nie ma pakietu o nazwie 'covr'
```
