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
  fill = 0
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

## Value

A `NeuroSurface` object containing the mapped values.

## Examples

``` r
# \donttest{
# Load standard white and pial surfaces from the package
wm_surf_file <- system.file("extdata", "std.8.lh.white.asc", package = "neurosurf")
pial_surf_file <- system.file("extdata", "std.8.lh.pial.asc", package = "neurosurf")

surf_wm <- read_surf_geometry(wm_surf_file)
#> Error in SurfaceGeometrySource(surface_name): file.exists(surface_name) is not TRUE
surf_pial <- read_surf_geometry(pial_surf_file)
#> Error in SurfaceGeometrySource(surface_name): file.exists(surface_name) is not TRUE

# Load an example volume (replace with actual loading code later)
# vol <- neuroim2::read_vol("path/to/volume.nii")

# Example: Create a dummy volume for demonstration purposes
# This should be replaced with real volume data
library(neuroim2)
#> 
#> Attaching package: ‘neuroim2’
#> The following object is masked from ‘package:base’:
#> 
#>     scale
# Assume the surfaces are in a space roughly covered by this bounding box
# Adjust dimensions and origin based on your actual data alignment
bb <- matrix(c(-80, 80, -120, 80, -60, 90), 3, 2, byrow = TRUE)
spacing <- c(1, 1, 1)
dims <- ceiling(abs(bb[,2] - bb[,1]) / spacing)
origin <- bb[,1]
sp <- NeuroSpace(dims, spacing, origin)
vol <- NeuroVol(rnorm(prod(dims)), sp)

# Map volume to surface using average mapping
mapped_surf <- vol_to_surf(surf_wm, surf_pial, vol, fun = "avg")
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'vertices': object 'surf_wm' not found
print(summary(series(mapped_surf)))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'print': error in evaluating the argument 'object' in selecting a method for function 'summary': error in evaluating the argument 'x' in selecting a method for function 'series': object 'mapped_surf' not found

# Map volume to surface using nearest neighbor mapping
mapped_surf_nn <- vol_to_surf(surf_wm, surf_pial, vol, fun = "nn")
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'vertices': object 'surf_wm' not found
print(summary(series(mapped_surf_nn)))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'print': error in evaluating the argument 'object' in selecting a method for function 'summary': error in evaluating the argument 'x' in selecting a method for function 'series': object 'mapped_surf_nn' not found
# }
```
