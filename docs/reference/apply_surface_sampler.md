# Apply a precomputed surface sampler to a volume

Apply a precomputed surface sampler to a volume

## Usage

``` r
apply_surface_sampler(
  sampler,
  vol,
  fun = c("avg", "nn", "mode"),
  sigma = 8,
  fill = 0
)
```

## Arguments

- sampler:

  A sampler object returned by
  [`surface_sampler()`](surface_sampler.md).

- vol:

  A `NeuroVol` with the same grid as the template used to build the
  sampler.

- fun:

  Aggregation function: "avg", "nn", or "mode".

- sigma:

  Bandwidth for Gaussian weights when `fun = "avg"`.

- fill:

  Value used when no valid voxels fall within `dthresh`.

## Value

`NeuroSurface` with mapped data.

## Examples

``` r
# \donttest{
# Requires surface sampler and volume data
# sampler <- surface_sampler(geometry, vol)
# result <- apply_surface_sampler(sampler, vol)
# }
```
