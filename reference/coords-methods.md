# Extract Vertex Coordinates

Extracts the 3D coordinates of vertices from a surface object.

## Usage

``` r
# S4 method for class 'ROISurface'
coords(x)

# S4 method for class 'SurfaceGeometry'
coords(x)

# S4 method for class 'igraph'
coords(x)

# S4 method for class 'NeuroSurfaceVector'
coords(x)

# S4 method for class 'NeuroSurface'
coords(x)
```

## Arguments

- x:

  the object to extract coordinates from

## Value

A numeric matrix with three columns (x, y, z) and one row per vertex
