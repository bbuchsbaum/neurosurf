# Create a Region on Surface

Creates a Region on a Surface from a radius and surface

## Usage

``` r
SurfaceDisk(surf, index, radius, max_order = NULL)
```

## Arguments

- surf:

  a `SurfaceGeometry` or `BrainSurface` or `BrainSurfaceVector`

- index:

  the index of the central surface node. Must be a numeric integer value
  within `1:length(V(surf@graph))`.

- radius:

  the size in mm of the geodesic radius. Must be a single positive
  numeric value.

- max_order:

  maximum number of edges to traverse. default is computed based on
  average edge length.

## Details

The igraph associated with `surf` must have an edge attribute named
`dist` containing numeric weights with no `NA` values.
