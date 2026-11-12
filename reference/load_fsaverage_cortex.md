# Cortex mask for a packaged fsaverage template

Returns the cortical domain of each hemisphere of a bundled fsaverage
template: `TRUE` for cortex, `FALSE` for the non-cortical medial wall.
Pass it as `cortex_mask` to
[`surface_figure`](https://bbuchsbaum.github.io/neurosurf/reference/surface_figure.md)
or
[`render_surface_rgba`](https://bbuchsbaum.github.io/neurosurf/reference/render_surface_rgba.md)
so statistical colour never lands on the medial wall.

## Usage

``` r
load_fsaverage_cortex(density = "fsaverage5")
```

## Arguments

- density:

  Template density; currently `"fsaverage5"`.

## Value

A named list with logical vectors `"lh"` and `"rh"`, one value per
vertex of
[`load_fsaverage`](https://bbuchsbaum.github.io/neurosurf/reference/load_fsaverage.md)
for the same density.

## Details

The mask comes from FreeSurfer's Destrieux 2009 parcellation
(`aparc.a2009s`) of fsaverage5, as distributed by TemplateFlow: every
vertex except those labelled `Medial_wall` or `Unknown`. This is the
same domain as FreeSurfer's `?h.cortex.label` up to the parcellation's
boundary. See
`system.file("extdata", "PROVENANCE.md", package = "neurosurf")`.

## Examples

``` r
# \donttest{
cortex <- load_fsaverage_cortex("fsaverage5")
mean(cortex$lh)  # fraction of vertices that are cortex
#> [1] 0.9132982
# }
```
