# Subset an ROISurface Object

Subsets an \`ROISurface\` object by selecting specific vertex indices.

## Usage

``` r
# S4 method for class 'ROISurface,numeric,missing,ANY'
x[i, j, drop]
```

## Arguments

- x:

  The `ROISurface` object to subset.

- i:

  A numeric vector specifying the indices within the ROI to select.

- j:

  Missing (not used for this signature).

- drop:

  Missing or ANY (ignored, always returns an `ROISurface`).

## Value

A new `ROISurface` object containing only the selected vertices and
their associated data from the original ROI.
