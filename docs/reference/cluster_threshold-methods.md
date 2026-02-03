# Apply Cluster-Extent Threshold to Surface Data

Apply Cluster-Extent Threshold to Surface Data

## Usage

``` r
cluster_threshold(x, threshold, size, ...)

# S4 method for class 'NeuroSurfaceVector'
cluster_threshold(x, threshold, size = 10, index = 1)

# S4 method for class 'NeuroSurface'
cluster_threshold(x, threshold, size = 10)
```

## Arguments

- x:

  Object to threshold

- threshold:

  the two-element threshold range to use to define connected components

- size:

  the minimum size of the connected components to keep

- ...:

  Additional arguments

- index:

  the index/column of the underlying data matrix to cluster

## Value

A thresholded surface object of the same class as `x`

## See also

[`conn_comp`](https://bbuchsbaum.github.io/neuroim2/reference/conn_comp-methods.html)
