# NeuroSurfaceVector

construct a new NeuroSurfaceVector

## Usage

``` r
NeuroSurfaceVector(geometry, indices, mat)
```

## Arguments

- geometry:

  a `SurfaceGeometry` or `SurfaceSet` instance

- indices:

  an integer vector specifying the valid surface nodes.

- mat:

  a `matrix` of data values (rows=nodes, columns=variables)

## Value

A
[`NeuroSurfaceVector`](https://bbuchsbaum.github.io/neurosurf/reference/NeuroSurfaceVector-class.md)
object containing the geometry, node indices, and data matrix.
