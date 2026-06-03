# Extract sparse matrix triplets from a surface sampler

Converts a precomputed \`surface_sampler\` into sparse matrix triplet
format (i, j, x) suitable for constructing a dgCMatrix or for interop
with other packages (e.g., neurofunctor). The triplets represent a
vertices × voxels projection matrix with normalized Gaussian weights.

## Usage

``` r
sampler_to_triplets(
  sampler,
  sigma = NULL,
  normalize = TRUE,
  min_weight = 1e-10
)
```

## Arguments

- sampler:

  A sampler object returned by
  [`surface_sampler()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_sampler.md).

- sigma:

  Bandwidth for Gaussian kernel weights. If NULL, uses the default from
  the sampler's dthresh (dthresh/2).

- normalize:

  Logical; if TRUE (default), weights for each vertex sum to 1.

- min_weight:

  Minimum weight threshold; entries below this are dropped. Default is
  1e-10 to remove numerical noise.

## Value

A list with class `"vol2surf_triplets"` containing:

- i:

  Integer vector of vertex indices (1-based row indices)

- j:

  Integer vector of voxel indices (1-based column indices into the
  volume's linear index space, corresponding to sampler\$indices)

- x:

  Numeric vector of weights

- dims:

  Integer vector c(n_vertices, n_voxels) for matrix dimensions

- voxel_indices:

  The sampler\$indices mapping j values to volume positions

- n_vertices:

  Number of surface vertices

- n_voxels:

  Number of candidate voxels

- nnz:

  Number of non-zero entries

- params:

  List of parameters used (sigma, normalize, min_weight, dthresh)

## Details

The output triplets define a sparse matrix P where P\[i,j\] is the
weight for vertex i from voxel j. The actual volume voxel index is
`voxel_indices[j]`. To construct a dgCMatrix in R:

`Matrix::sparseMatrix(i = triplets$i, j = triplets$j, x = triplets$x, dims = triplets$dims)`

## See also

[`surface_sampler`](https://bbuchsbaum.github.io/neurosurf/reference/surface_sampler.md),
[`apply_surface_sampler`](https://bbuchsbaum.github.io/neurosurf/reference/apply_surface_sampler.md)

## Examples

``` r
# \donttest{
# Requires a surface sampler
# sampler <- surface_sampler(wm, pial, template_vol)
# triplets <- sampler_to_triplets(sampler)
# }
```
