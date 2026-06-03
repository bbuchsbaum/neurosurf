# Convert Surface Data to Matrix

Converts surface data to a matrix representation.

## Usage

``` r
# S4 method for class 'ROISurfaceVector'
as.matrix(x)

# S4 method for class 'NeuroSurfaceVector'
as.matrix(x)

# S4 method for class 'BilatNeuroSurfaceVector'
as.matrix(x)
```

## Arguments

- x:

  the object to convert to a matrix

## Value

A numeric matrix with vertices as rows and time points/features as
columns
