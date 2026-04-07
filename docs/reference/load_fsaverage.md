# Fetch fsaverage surfaces

This is a high-level wrapper that mirrors the API of neuromaps'
`fetch_fsaverage()` but is currently limited to the `"std.8"` decimated
fsaverage surfaces that ship with neurosurf. The function name uses
"load" rather than "fetch" to follow common R idioms.

## Usage

``` r
load_fsaverage(
  density = "std.8",
  surf = c("smoothwm", "pial", "inflated", "white", "sphere")
)
```

## Arguments

- density:

  Character string specifying surface density. At present only `"std.8"`
  is supported.

- surf:

  Character string specifying which surface to load. One of
  `"smoothwm"`, `"pial"`, `"inflated"`, `"white"`, or `"sphere"`.
  Defaults to `"smoothwm"`.

## Value

A named list with elements `"lh"` and `"rh"`, each a `SurfaceGeometry`
instance.

## Examples

``` r
# \donttest{
fs <- load_fsaverage(density = "std.8", surf = "inflated")
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpFhvBlN/temp_libpathff9947c5f204/neurosurf/extdata/std.8_lh.inflated.asc
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpFhvBlN/temp_libpathff9947c5f204/neurosurf/extdata/std.8_rh.inflated.asc
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
