# Snapshot a surface to a PNG

Convenience helper for vignettes and reports: renders a surface with
\`view_surface()\` onto an off-screen rgl device and saves a PNG. When
\`rgl.useNULL()\` is \`TRUE\` (headless builds), a proper snapshot
requires the \`webshot2\` package; otherwise a blank image is likely and
an empty path is returned.

## Usage

``` r
snapshot_surface(surfgeom, file = NULL, width = 1200, height = 900, ...)
```

## Arguments

- surfgeom:

  A [`SurfaceGeometry`](SurfaceGeometry-class.md) object.

- file:

  Output path for the PNG. Defaults to the current knitr figure path
  when knitting, otherwise a temporary file.

- width, height:

  Device size in pixels (controls render resolution).

- ...:

  Additional arguments passed to [`view_surface`](view_surface.md).

## Value

The file path (invisibly). Callers can use
[`knitr::include_graphics()`](https://rdrr.io/pkg/knitr/man/include_graphics.html)
or read the image via
[`png::readPNG()`](https://rdrr.io/pkg/png/man/readPNG.html). In
headless mode without \`webshot2\`, an empty character vector is
returned.

## Examples

``` r
# \donttest{
fs <- load_fsaverage_std8("inflated")
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpwYGE4M/temp_libpathbf0a5b9dbd99/neurosurf/extdata/std.8_lh.inflated.asc
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpwYGE4M/temp_libpathbf0a5b9dbd99/neurosurf/extdata/std.8_rh.inflated.asc
img <- snapshot_surface(fs$lh, viewpoint = "lateral", specular = "black")
#> file:////private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpbC7ZcQ/filec6ee2824d50d.html screenshot completed
# knitr::include_graphics(img)
# }
```
