# Load a bundle of fsaverage surface variants as a SurfaceSet

Load a bundle of fsaverage surface variants as a SurfaceSet

## Usage

``` r
load_fsaverage_bundle(
  density = c("std.8", "fsaverage5"),
  surfs = NULL,
  default_label = NULL
)
```

## Arguments

- density:

  Surface density: `"std.8"` (default) or `"fsaverage5"`.

- surfs:

  Character vector of surface labels to include. For std.8, any of
  `c("smoothwm","pial","inflated","white","sphere")`; for fsaverage5,
  any of `c("inflated","white","pial")`. Defaults to all surfaces
  available at the chosen density.

- default_label:

  Which label to treat as default when none is specified. Defaults to
  `"smoothwm"` for std.8 and `"inflated"` for fsaverage5.

## Value

A named list with elements `\"lh\"` and `\"rh\"`, each a
[`SurfaceSet`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceSet-class.md)
containing the requested variants.

## Examples

``` r
# \donttest{
bundle <- load_fsaverage_bundle()
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_lh.smoothwm.asc
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_rh.smoothwm.asc
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_lh.pial.asc
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_rh.pial.asc
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_lh.inflated.asc
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_rh.inflated.asc
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_lh.white.asc
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_rh.white.asc
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_lh.sphere.asc
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_rh.sphere.asc
lh_set <- bundle$lh
rh_set <- bundle$rh
fs5 <- load_fsaverage_bundle("fsaverage5")
# }
```
