# Get Adjacency Graph

Get Adjacency Graph

## Usage

``` r
adjacency(x, attr, ...)

# S4 method for class 'SurfaceGeometry,numeric'
adjacency(x, attr)

# S4 method for class 'SurfaceGeometry,character'
adjacency(x, attr)

# S4 method for class 'SurfaceGeometry,missing'
adjacency(x)
```

## Arguments

- x:

  Graph structure

- attr:

  Character; edge attribute for weights in igraph object. If absent,
  weights are 0 or 1.

- ...:

  Additional arguments

## Value

A sparse adjacency matrix of class `dgCMatrix`
