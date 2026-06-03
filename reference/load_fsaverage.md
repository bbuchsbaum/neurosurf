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
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_lh.inflated.asc
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_rh.inflated.asc
if (interactive()) {
  show_surface_plot(fs$lh, fs$rh, views = c("lateral", "medial"))
}
# }
```
