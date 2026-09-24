# Sulcal depth for a packaged fsaverage template

Returns FreeSurfer sulcal depth (`sulc`) for each hemisphere of a
bundled fsaverage template, suitable as the anatomy underlay of
\[surface_figure()\] or \[render_surface_rgba()\]. Positive values are
deeper (sulcal); the renderers detect this polarity automatically.

## Usage

``` r
load_fsaverage_sulc(density = "fsaverage5")
```

## Arguments

- density:

  Template density; currently `"fsaverage5"`.

## Value

A named list with numeric vectors `"lh"` and `"rh"`, one value per
vertex of \[load_fsaverage()\] for the same density.

## Examples

``` r
# \donttest{
sulc <- load_fsaverage_sulc("fsaverage5")
range(sulc$lh)
#> [1] -1.493725  1.806910
# }
```
