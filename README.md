
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

<img src='man/figures/logo2.png' align="center"/>

## About

You are reading the doc about version : 0.0.1

This README has been compiled on the

``` r
Sys.time()
#> [1] "2024-12-02 22:48:49 CST"
```

Here are the tests results:

``` r
devtools::check(quiet = TRUE)
#> ℹ Loading autoextract
#> ── R CMD check results ────────────────────────────────── autoextract 0.0.1 ────
#> Duration: 4m 1.3s
#> 
#> ❯ checking dependencies in R code ... NOTE
#>   Namespace in Imports field not imported from: 'fields'
#>     All declared Imports should be used.
#> 
#> 0 errors ✔ | 0 warnings ✔ | 1 note ✖
```
