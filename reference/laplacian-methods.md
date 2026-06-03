# Compute Graph Laplacian

Compute Graph Laplacian

## Usage

``` r
laplacian(x, normalized, weights, ...)

# S4 method for class 'SurfaceGeometry,missing,missing'
laplacian(x)

# S4 method for class 'SurfaceGeometry,missing,numeric'
laplacian(x, weights)
```

## Arguments

- x:

  Object to compute Laplacian from

- normalized:

  Logical; whether to normalize the Laplacian

- weights:

  Edge weights for weighted Laplacian matrix

- ...:

  Additional arguments

## Value

A sparse Laplacian matrix of class `dgCMatrix`
