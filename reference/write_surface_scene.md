# Write a standalone portable surface report

Write a standalone portable surface report

## Usage

``` r
write_surface_scene(scene, path, self_contained = FALSE, title = scene@id)
```

## Arguments

- scene:

  A `SurfaceScene`.

- path:

  Output directory.

- self_contained:

  If `TRUE`, inline both assets and the browser runtime. Otherwise,
  write the runtime and SHA-addressed assets beside `index.html`.

- title:

  HTML document title.

## Value

The path to `index.html`, invisibly.

## Details

With `self_contained = FALSE`, the function writes `index.html`, the
local surfview runtime, and content-addressed binary assets. With
`self_contained = TRUE`, it inlines the runtime and assets into one HTML
file. Neither mode requires a runtime network connection.

## See also

[`surface_scene`](https://bbuchsbaum.github.io/neurosurf/reference/surface_scene.md),
[`surfwidget`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
