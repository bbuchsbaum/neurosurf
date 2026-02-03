# Construct a SurfaceSet

Construct a SurfaceSet

## Usage

``` r
surface_set(..., hemi = NULL, default_label = NULL)
```

## Arguments

- ...:

  Named \`SurfaceGeometry\` objects, or a single named list of them.

- hemi:

  Hemisphere code; defaults to the hemi of the first geometry.

- default_label:

  Optional default label; defaults to the first provided label.

## Value

A \`SurfaceSet\` instance.

## Examples

``` r
# Create a simple SurfaceSet with a single geometry
geom <- example_surface_geometry()
ss <- surface_set(inflated = geom)
surface_labels(ss)
#> [1] "inflated"
```
