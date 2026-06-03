# Compute boundary hull points in world space (C++)

Compute boundary hull points in world space (C++)

## Usage

``` r
compute_hull_world_cpp(vol, dims, grid2world, thresh, n_points, mask)
```

## Arguments

- vol:

  flattened numeric array (dim = dims)

- dims:

  integer vector length 3 (nx, ny, nz)

- grid2world:

  4x4 matrix mapping voxel indices to world coords

- thresh:

  intensity threshold

- n_points:

  max number of hull points (approximate)

- mask:

  optional logical vector same length as vol; if empty, ignored

## Value

numeric matrix (n_points x 3) of (x,y,z) in world coords
