# mcotear
![mcotear package for R](man/figures/logo.png)

[<img src="man/figures/logo.png" width="250" alt = "mcotear package for r"/>](image.png)

An **R** package developed to document R functions I regularly use in my work, for easy access and documentation.

Installation
------------

Install `mcotear` from Github with `devtools`:

``` r
library(devtools)
devtools::install_github("jjw3952/mcotear")
# or if you want the vignettes
# devtools::install_github("jjw3952/mcotear", build_vignettes = TRUE)
library(mcotear)
```

Usage
-----

Here are some things to try to get started.

``` r
library(mcotear)
(functions <- ls("package:mcotear"))
length(functions)
example(functions[14], character.only = FALSE)
browseVignettes("mcotear")
```
