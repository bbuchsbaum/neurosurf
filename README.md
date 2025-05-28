
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://travis-ci.org/bbuchsbaum/neurosurf.svg?branch=master)](https://travis-ci.org/bbuchsbaum/neurosurf)

# neurosurf

Under development

## Installation

You can install neurosurf from github with:

``` r
# install.packages("devtools")
devtools::install_github("bbuchsbaum/neurosurf")
```

## Vignettes

See examples of use of `neurosurf` in the

[vignettes](https://bbuchsbaum.github.io/neurosurf/articles/index.html).


## Cleaning Up

When removing a surface from a viewer, call the `dispose()` method on the
`NeuroSurface` object to free associated WebGL resources and event listeners.
