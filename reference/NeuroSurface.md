# Construct a NeuroSurface Object

This function creates a new NeuroSurface object, which represents a
single set of data values associated with nodes on a surface geometry.

## Usage

``` r
NeuroSurface(geometry, indices, data)
```

## Arguments

- geometry:

  A `SurfaceGeometry` or `SurfaceSet` object representing the underlying
  surface structure.

- indices:

  An integer vector specifying the indices of valid surface nodes.

- data:

  A numeric vector of data values corresponding to the surface nodes.

## Value

A new object of class `NeuroSurface` containing:

- geometry:

  The input `SurfaceGeometry` object.

- indices:

  The input integer vector of valid node indices.

- data:

  The input numeric vector of data values.

## Details

The NeuroSurface object is designed to store and manipulate a single set
of data values associated with nodes on a surface. This can represent
various neuroimaging measures such as cortical thickness, functional
activation, or any other node-wise metric.

The length of the `data` vector should match the length of the `indices`
vector. Nodes not included in the `indices` vector are considered to
have no data or to be invalid.

## See also

[`SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry.md),
[`NeuroSurfaceVector`](https://bbuchsbaum.github.io/neurosurf/reference/NeuroSurfaceVector.md)

## Examples

``` r
# \donttest{
surf_geom <- example_surface_geometry()
indices <- seq_len(nrow(coords(surf_geom)))  # all vertices
data_values <- rnorm(length(indices))        # Example data
neuro_surf <- NeuroSurface(surf_geom, indices, data_values)
# }
```
