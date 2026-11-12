# Load fsaverage std.8 surfaces packaged with neurosurf

This convenience helper loads the FreeSurfer fsaverage surfaces that
ship with neurosurf (the `std.8` decimated variant) and returns them as
[`SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry-class.md)
objects.

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

## Details

The meshes have 642 vertices per hemisphere and share fsaverage (MNI305)
coordinates with the fsaverage5 surfaces of
[`load_fsaverage`](https://bbuchsbaum.github.io/neurosurf/reference/load_fsaverage.md):
a rigid fit between the two white surfaces moves them by less than half
a millimetre and a degree.

## Examples

``` r
# \donttest{
fs <- load_fsaverage_std8("smoothwm")
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_lh.smoothwm.asc
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_rh.smoothwm.asc
if (interactive()) {
  show_surface_plot(fs$lh, fs$rh, views = c("lateral", "medial"))
}
# }
```
