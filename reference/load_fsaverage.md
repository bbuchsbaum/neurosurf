# Load packaged fsaverage surfaces

Loads a FreeSurfer fsaverage template that ships with neurosurf and
returns it as
[`SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry-class.md)
objects. Two densities are bundled: `"fsaverage5"` (10,242 vertices per
hemisphere; inflated and white surfaces) and the legacy `"std.8"` mesh
(642 vertices per hemisphere; smoothwm, pial, inflated, white, and
sphere surfaces). Use fsaverage5 for figures and examples; std.8 is kept
for small tests.

## Usage

``` r
load_fsaverage(density = c("std.8", "fsaverage5"), surf = NULL)
```

## Arguments

- density:

  Surface density: `"std.8"` (default, for backward compatibility) or
  `"fsaverage5"`.

- surf:

  Character string specifying which surface to load. One of
  `"smoothwm"`, `"pial"`, `"inflated"`, `"white"`, or `"sphere"` for
  std.8; `"inflated"` or `"white"` for fsaverage5. Defaults to
  `"smoothwm"` for std.8 and `"inflated"` for fsaverage5.

## Value

A named list with elements `"lh"` and `"rh"`, each a `SurfaceGeometry`
instance.

## Details

The fsaverage5 files are the FreeSurfer fsaverage5 template as
redistributed by nilearn, under the FreeSurfer license.

## See also

\[load_fsaverage_sulc()\] for the matching sulcal depth.

## Examples

``` r
# \donttest{
fs <- load_fsaverage("fsaverage5", "inflated")
nrow(coords(fs$lh))
#> [1] 10242
# }
```
