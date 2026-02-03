# Map a volume to surface after SDF-based rigid alignment

This wrapper estimates a rigid transform from the input volume to the
template surface space using a signed distance field (SDF), then reuses
[`vol_to_surf()`](vol_to_surf.md) for the actual sampling in volume
space.

## Usage

``` r
vol_to_surf_sdf(
  surf_wm,
  surf_pial,
  vol,
  sdf_vol,
  mask = NULL,
  n_hull = 2000L,
  thresh = NULL,
  init_par = rep(0, 6),
  outside_penalty = 20,
  method = "L-BFGS-B",
  ...
)
```

## Arguments

- surf_wm:

  White-matter (inner) surface in template space

- surf_pial:

  Pial (outer) surface in template space

- vol:

  NeuroVol in (approximate) template/MNI space

- sdf_vol:

  NeuroVol SDF of the template cortical surface

- mask:

  optional mask for hull extraction and mapping

- n_hull:

  number of hull points to retain (approximate)

- thresh:

  optional intensity threshold for hull computation

- init_par:

  optional initial parameters (tx, ty, tz, rx, ry, rz)

- outside_penalty:

  value used when SDF sampling is outside the grid

- method:

  optimization method passed to roptim (default "L-BFGS-B")

- ...:

  passed through to [`vol_to_surf()`](vol_to_surf.md) (e.g., `fun`,
  `knn`, `sigma`)

## Value

A `NeuroSurface` with values mapped from `vol`

## Examples

``` r
# \donttest{
# Requires surfaces and volumes in template space
# wm <- read_surf_geometry("lh.white")
# pial <- read_surf_geometry("lh.pial")
# vol <- neuroim2::read_vol("mydata.nii")
# sdf <- neuroim2::read_vol("template_sdf.nii")
# result <- vol_to_surf_sdf(wm, pial, vol, sdf)
# }
```
