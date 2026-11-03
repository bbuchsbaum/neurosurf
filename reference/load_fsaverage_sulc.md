# Sulcal depth for a packaged fsaverage template

Returns FreeSurfer sulcal depth (`sulc`) for each hemisphere of a
bundled fsaverage template, suitable as the anatomy underlay of
[`surface_figure`](https://bbuchsbaum.github.io/neurosurf/reference/surface_figure.md)
or
[`render_surface_rgba`](https://bbuchsbaum.github.io/neurosurf/reference/render_surface_rgba.md).
Positive values are deeper (sulcal); the renderers detect this polarity
automatically.

## Usage

``` r
load_fsaverage_sulc(density = "fsaverage5")
```

## Arguments

- density:

  Template density; currently `"fsaverage5"`.

## Value

A named list with numeric vectors `"lh"` and `"rh"`, one value per
vertex of
[`load_fsaverage`](https://bbuchsbaum.github.io/neurosurf/reference/load_fsaverage.md)
for the same density.

## Examples

``` r
# \donttest{
sulc <- load_fsaverage_sulc("fsaverage5")
range(sulc$lh)
#> [1] -1.493725  1.806910
# }
```
