# Build a reusable surface sampler for multi-frame volumes

Precompute voxel neighbors and distances for each surface vertex so that
repeated volume-to-surface projections (e.g., 4D time series) can be
done quickly without rebuilding nearest-neighbor searches.

## Usage

``` r
surface_sampler(
  surf_wm,
  surf_pial,
  vol_template,
  mask = NULL,
  sampling = c("midpoint", "normal_line", "thickness"),
  n_samples = NULL,
  depth = NULL,
  radius = 3,
  knn = 6,
  dthresh = 16
)
```

## Arguments

- surf_wm:

  White-matter (inner) surface, `SurfaceGeometry`.

- surf_pial:

  Pial (outer) surface, `SurfaceGeometry`.

- vol_template:

  A `NeuroVol` used to define voxel space and candidate voxels (via
  `mask` or non-zero entries).

- mask:

  Optional mask limiting candidate voxels; if `NULL`, all non-zero
  voxels in `vol_template` are used.

- sampling:

  How to place sample points relative to the white/pial pair. Options
  are:

  - "midpoint" (default): original behaviour, samples at the midpoint
    between white and pial.

  - "thickness": samples along the white→pial line at fractions given by
    `depth` or evenly spaced.

  - "normal_line": samples along the vertex normal centred on the
    midpoint, spanning `radius` in both directions (or using `depth`
    offsets).

- n_samples:

  Number of samples per vertex for `sampling != "midpoint"` when `depth`
  is not supplied.

- depth:

  Optional numeric vector controlling sampling positions; interpreted as
  fractions of thickness (for "thickness") or multiples of `radius` (for
  "normal_line").

- radius:

  Radius (in voxel units) for normal-line sampling when
  `sampling = "normal_line"`; also used as the distance scale when
  interpreting `depth` offsets for that mode.

- knn:

  Number of cached nearest voxel candidates per surface sample.

- dthresh:

  Maximum cached candidate distance in volume world-coordinate units.

## Value

A list with class `"surface_sampler"` containing precomputed neighbor
indices and distances for each vertex.

## Examples

``` r
# \donttest{
# Requires white and pial surfaces plus a template volume
# wm <- read_surf_geometry("lh.white")
# pial <- read_surf_geometry("lh.pial")
# template_vol <- neuroim2::read_vol("template.nii")
# sampler <- surface_sampler(wm, pial, template_vol)
# }
```
