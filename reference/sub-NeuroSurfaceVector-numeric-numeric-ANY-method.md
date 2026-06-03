# Subset NeuroSurfaceVector

Extracts a subset of data from a NeuroSurfaceVector.

## Usage

``` r
# S4 method for class 'NeuroSurfaceVector,numeric,numeric,ANY'
x[i, j, ..., drop = TRUE]
```

## Arguments

- x:

  the object

- i:

  first index (rows/vertices)

- j:

  second index (columns/time points)

- ...:

  additional args

- drop:

  whether to drop dimensions

## Value

A numeric matrix or vector of the extracted data subset
