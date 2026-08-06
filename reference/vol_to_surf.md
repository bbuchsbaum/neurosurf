# Map values from a 3D volume to a surface in the same coordinate space

This function maps values from a 3D volume to a surface representation,
allowing for different mapping strategies.

## Usage

``` r
vol_to_surf(
  surf_wm,
  surf_pial,
  vol,
  mask = NULL,
  fun = c("avg", "nn", "mode"),
  knn = 6,
  sigma = 8,
  dthresh = sigma * 2,
  fill = 0,
  sampling = c("midpoint", "normal_line", "thickness"),
  n_samples = NULL,
  depth = NULL,
  radius = 3,
  sampler = NULL,
  interpolation = c("legacy", "nearest", "linear"),
  aggregate = NULL,
  na_rm = FALSE,
  surface_smooth_fwhm = 0
)
```

## Arguments

- surf_wm:

  The white matter (inner) surface, typically of class
  `SurfaceGeometry`.

- surf_pial:

  The pial (outer) surface, typically of class `SurfaceGeometry`.

- vol:

  An image volume of type `NeuroVol` that is to be mapped to the
  surface.

- mask:

  A mask defining valid voxels. In the legacy KNN contract, NULL retains
  historical behavior and treats only finite non-zero voxels as
  candidates. Explicit nearest/linear interpolation samples the full
  finite voxel grid, including zeros; an explicit mask restricts that
  grid.

- fun:

  The mapping function to use. Options are:

  - "avg": Average of nearby voxels (default)

  - "nn": Nearest neighbor

  - "mode": Most frequent value among nearby voxels

- knn:

  Number of cached nearest voxel candidates per surface sample.

- sigma:

  Legacy Gaussian-KNN bandwidth.

- dthresh:

  Maximum cached candidate distance in volume world-coordinate units.

- fill:

  Value used when no valid sample is available. For strict linear
  interpolation this includes an out-of-volume, NA, or masked corner;
  with `na_rm = TRUE`, remaining corner weights are renormalized
  instead.

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

- sampler:

  Optional surface sampler object created by
  [`surface_sampler()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_sampler.md).
  When provided, the sampler is reused and other sampling-related
  arguments are ignored.

- interpolation:

  Voxel interpolation contract. `"legacy"` (default) preserves the
  historical midpoint Gaussian-KNN path and the historical
  nearest-sample path for multi-depth sampling. `"nearest"` samples the
  full voxel grid, including zero-valued voxels. `"linear"` performs
  trilinear interpolation of the scalar field at each sample point.

- aggregate:

  Explicit aggregation across cortical-depth samples for non-legacy
  interpolation: `"mean"`, categorical `"mode"`, or `"closest"` (the
  valid sample nearest the ribbon midpoint). If NULL, it is inferred
  from `fun` for compatibility.

- na_rm:

  For trilinear interpolation, whether a sample may renormalize
  interpolation weights after NA, masked, or out-of-volume corners are
  removed. The default FALSE returns `fill` for that sample, preventing
  interpolation across a missing-data or mask boundary.

- surface_smooth_fwhm:

  Tangential surface smoothing in mm. Zero (default) disables smoothing.
  Positive values apply a topology-local Gaussian edge-weighted pass
  whose spatial weights use surface-coordinate millimetres; this is
  deliberately separate from voxel interpolation and cortical-depth
  aggregation.

## Value

A `NeuroSurface` object containing the mapped values.

## Examples

``` r
# \donttest{
# Load standard white and pial surfaces from the package
wm_surf_file <- system.file("extdata", "std.8_lh.white.asc", package = "neurosurf")
pial_surf_file <- system.file("extdata", "std.8_lh.pial.asc", package = "neurosurf")

surf_wm <- read_surf_geometry(wm_surf_file)
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_lh.white.asc
surf_pial <- read_surf_geometry(pial_surf_file)
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_lh.pial.asc

# Create a dummy volume for demonstration purposes
bb <- matrix(c(-80, 80, -120, 80, -60, 90), 3, 2, byrow = TRUE)
spacing <- c(1, 1, 1)
dims <- ceiling(abs(bb[,2] - bb[,1]) / spacing)
origin <- bb[,1]
sp <- neuroim2::NeuroSpace(dims, spacing, origin)
vol <- neuroim2::NeuroVol(rnorm(prod(dims)), sp)

# Map volume to surface using average mapping
mapped_surf <- vol_to_surf(surf_wm, surf_pial, vol, fun = "avg")
print(mapped_surf)
#> 
#>  NeuroSurface  
#> 
#>   Geometry & Data Mapping: 
#>   Hemisphere:         lh
#>   Total Vertices:   642
#>   Vertices w/ Data:642
#> 
#>   Data Summary: 
#>   Min:    -1.148
#>   Median:-0.01554
#>   Mean:  0.0004073
#>   Max:    1.126
#> 
# }
```
