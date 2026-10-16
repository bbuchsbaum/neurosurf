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

A matrix with time points in rows and the requested vertices in columns,
i.e. `T x length(i)` for a `NeuroSurfaceVector` with `T` time points,
matching the
[`neuroim2::series()`](https://bbuchsbaum.github.io/neuroim2/reference/series-methods.html)
convention for `NeuroVec`. The orientation does not depend on whether
`i` is stored as integer or double; a single index gives a `T x 1`
matrix. For a `NeuroSurface` (one value per vertex) the result is a
`1 x length(i)` matrix.
