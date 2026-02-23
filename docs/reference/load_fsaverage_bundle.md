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
[`SurfaceSet`](SurfaceSet-class.md) containing the requested variants.

## Examples

``` r
# \donttest{
bundle <- load_fsaverage_bundle()
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpQdM37p/temp_libpatha900461f6b09/neurosurf/extdata/std.8_lh.smoothwm.asc
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpQdM37p/temp_libpatha900461f6b09/neurosurf/extdata/std.8_rh.smoothwm.asc
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpQdM37p/temp_libpatha900461f6b09/neurosurf/extdata/std.8_lh.pial.asc
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpQdM37p/temp_libpatha900461f6b09/neurosurf/extdata/std.8_rh.pial.asc
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpQdM37p/temp_libpatha900461f6b09/neurosurf/extdata/std.8_lh.inflated.asc
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpQdM37p/temp_libpatha900461f6b09/neurosurf/extdata/std.8_rh.inflated.asc
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpQdM37p/temp_libpatha900461f6b09/neurosurf/extdata/std.8_lh.white.asc
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpQdM37p/temp_libpatha900461f6b09/neurosurf/extdata/std.8_rh.white.asc
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpQdM37p/temp_libpatha900461f6b09/neurosurf/extdata/std.8_lh.sphere.asc
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpQdM37p/temp_libpatha900461f6b09/neurosurf/extdata/std.8_rh.sphere.asc
lh_set <- bundle$lh
rh_set <- bundle$rh
# }
```
