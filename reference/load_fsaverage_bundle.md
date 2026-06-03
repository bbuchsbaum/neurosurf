# Load a bundle of fsaverage surface variants as a SurfaceSet

Load a bundle of fsaverage surface variants as a SurfaceSet

## Usage

``` r
load_fsaverage_bundle(
  density = "std.8",
  surfs = c("smoothwm", "pial", "inflated", "white", "sphere"),
  default_label = "smoothwm"
)
```

## Arguments

- density:

  Surface density; currently only `\"std.8\"` is supported.

- surfs:

  Character vector of surface labels to include (e.g.,
  `c(\"smoothwm\",\"pial\",\"inflated\",\"white\",\"sphere\")`).

- default_label:

  Which label to treat as default when none is specified.

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
# }
```
