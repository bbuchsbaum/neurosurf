# Load fsaverage std.8 surfaces packaged with neurosurf

This convenience helper loads the FreeSurfer fsaverage surfaces that
ship with neurosurf (the `std.8` decimated variant) and returns them as
[`SurfaceGeometry`](SurfaceGeometry-class.md) objects.

## Usage

``` r
load_fsaverage_std8(
  surf = c("smoothwm", "pial", "inflated", "white", "sphere")
)
```

## Arguments

- surf:

  Character string specifying which surface to load. One of
  `"smoothwm"`, `"pial"`, `"inflated"`, `"white"`, or `"sphere"`.
  Defaults to `"smoothwm"`.

## Value

A named list with elements `"lh"` and `"rh"`, each a `SurfaceGeometry`
instance for the requested surface type.

## Examples

``` r
# \donttest{
fs <- load_fsaverage_std8("smoothwm")
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/Rtmp0Nbniq/temp_libpath80e357118cac/neurosurf/extdata/std.8_lh.smoothwm.asc
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/Rtmp0Nbniq/temp_libpath80e357118cac/neurosurf/extdata/std.8_rh.smoothwm.asc
show_surface_plot(fs$lh, fs$rh, views = c("lateral", "medial"))
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: no non-missing arguments to max; returning -Inf
#> Warning: no non-missing arguments to min; returning Inf
#> Warning: no non-missing arguments to max; returning -Inf

# }
```
