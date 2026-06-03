# Extract Time Series from Surface Vector

Extracts time series data from specific vertices in a surface vector.

## Usage

``` r
# S4 method for class 'NeuroSurfaceVector,numeric'
series(x, i)

# S4 method for class 'NeuroSurfaceVector,integer'
series(x, i)

# S4 method for class 'NeuroSurfaceVector,ROISurface'
series(x, i)

# S4 method for class 'NeuroSurface,numeric'
series(x, i)
```

## Arguments

- x:

  the object to extract the series from

- i:

  the indices of the series to extract

## Value

A matrix where columns are time points and rows are vertex indices
