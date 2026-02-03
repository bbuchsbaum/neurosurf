# Read Surface Data Sequence

Load one or more surface datasets for both left and right hemispheres.

## Usage

``` r
read_surf_data_seq(leftGeometry, rightGeometry, leftDataNames, rightDataNames)
```

## Arguments

- leftGeometry:

  a [`SurfaceGeometry`](SurfaceGeometry-class.md) instance for the left
  hemisphere

- rightGeometry:

  a [`SurfaceGeometry`](SurfaceGeometry-class.md) instance for the right
  hemisphere

- leftDataNames:

  a `character` vector indicating names of left-hemisphere surface data
  files to be mapped to geometry.

- rightDataNames:

  a `character` vector indicating names of right-hemisphere surface data
  files to be mapped to geometry.

## Value

A list of `BilatNeuroSurfaceVector` objects, one per pair of data files
