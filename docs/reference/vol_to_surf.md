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
  sampler = NULL
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

  A mask defining the valid voxels in the image volume. If NULL, all
  non-zero voxels are considered valid.

- fun:

  The mapping function to use. Options are:

  - "avg": Average of nearby voxels (default)

  - "nn": Nearest neighbor

  - "mode": Most frequent value among nearby voxels

- knn:

  The number of nearest neighbors to consider for mapping (default: 6).

- sigma:

  The bandwidth of the smoothing kernel for the "avg" mapping function
  (default: 8).

- dthresh:

  The maximum distance threshold for valid mapping. A voxel is only
  considered if it is less than `dthresh` units away from the vertex
  (default: 2 \* sigma).

- fill:

  Value used when no nearby voxels are found (default: 0 to preserve
  previous behavior).

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
  [`surface_sampler()`](surface_sampler.md). When provided, the sampler
  is reused and other sampling-related arguments are ignored.

## Value

A `NeuroSurface` object containing the mapped values.

## Examples

``` r
# \donttest{
# Load standard white and pial surfaces from the package
wm_surf_file <- system.file("extdata", "std.8_lh.white.asc", package = "neurosurf")
pial_surf_file <- system.file("extdata", "std.8_lh.pial.asc", package = "neurosurf")

surf_wm <- read_surf_geometry(wm_surf_file)
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpQdM37p/temp_libpatha900461f6b09/neurosurf/extdata/std.8_lh.white.asc
surf_pial <- read_surf_geometry(pial_surf_file)
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpQdM37p/temp_libpatha900461f6b09/neurosurf/extdata/std.8_lh.pial.asc

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
#>   Min:    -1.066
#>   Median:0.008907
#>   Mean:  0.008469
#>   Max:    1.264
#> 
# }
```
