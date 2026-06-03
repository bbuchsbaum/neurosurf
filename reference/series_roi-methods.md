# Extract ROI Time Series from Surface Vector

Extracts time series data from a region of interest in a surface vector.

## Usage

``` r
# S4 method for class 'NeuroSurfaceVector,numeric'
series_roi(x, i)

# S4 method for class 'NeuroSurfaceVector,ROISurface'
series_roi(x, i)
```

## Arguments

- x:

  the object to extract the series from

- i:

  the indices of the series to extract

## Value

An
[`ROISurfaceVector`](https://bbuchsbaum.github.io/neurosurf/reference/ROISurfaceVector-class.md)
containing the extracted time series
