
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{autoextract}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of `{autoextract}` like so:

``` r
# install.packages("devtools")
devtools::install_github("AparicioJohan/autoextract")
```

## Run

You can launch the application by running:

``` r
autoextract::run_app()
```

## About

You are reading the doc about version : 0.0.1

This README has been compiled on the

``` r
Sys.time()
#> [1] "2024-11-28 11:07:14 CST"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading autoextract
#> ── R CMD check results ────────────────────────────────── autoextract 0.0.1 ────
#> Duration: 1m 42.5s
#> 
#> 0 errors ✔ | 0 warnings ✔ | 0 notes ✔
```

``` r
covr::package_coverage()
#> autoextract Coverage: 55.71%
#> R/app_config.R: 0.00%
#> R/app_server.R: 0.00%
#> R/app_ui.R: 0.00%
#> R/fct_helpers.R: 0.00%
#> R/run_app.R: 0.00%
#> R/mod_01_grid_resize.R: 76.47%
```
