# Extract Surface Node Numbers

Retrieves the node numbers from a surface object.

## Usage

``` r
nodes(x)

# S4 method for class 'SurfaceGeometry'
nodes(x)

# S4 method for class 'NeuroSurface'
nodes(x)

# S4 method for class 'NeuroSurfaceVector'
nodes(x)
```

## Arguments

- x:

  An object representing a surface.

## Value

A vector of node numbers.

## See also

[`vertices`](https://bbuchsbaum.github.io/neurosurf/reference/vertices-methods.md)

## Examples

``` r
geom <- example_surface_geometry()
nodes(geom)
#> [1] 1 2 3 4
```
