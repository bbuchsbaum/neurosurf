# Read Surface Data from a File

This function reads surface data from a file in one of the supported
formats.

## Usage

``` r
read_surf(
  surface_name,
  surface_data_name = NULL,
  colind = NULL,
  nodeind = NULL
)
```

## Arguments

- surface_name:

  the name of the file containing the surface geometry.

- surface_data_name:

  the name of the file containing the values to be mapped to the surface
  (optional).

- colind:

  the columns/samples to load (optional), only if `surface_data_name` is
  not `NULL`

- nodeind:

  the subset of node indices to load

## Value

an instance of the class: [`SurfaceGeometry`](SurfaceGeometry-class.md)
or [`NeuroSurface`](NeuroSurface-class.md) or
[`NeuroSurfaceVector`](NeuroSurfaceVector-class.md)

## Details

The function supports reading surface data from various formats
including:

- Freesurfer ASCII (.asc)

- Freesurfer binary

- GIFTI (.gii)

- NIML Surface Dataset (.niml.dset)

The format is determined automatically from the file extension.

## Examples

``` r
# \donttest{
# Find the path to the example surface file in the package
surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")

# Check if the file exists
if (file.exists(surf_file)) {
  # Read the surface geometry (returns SurfaceGeometry for geometry-only files)
  surf <- read_surf(surf_file)

  # Display basic information about the surface
  print(surf)

  # Get vertex coordinates
  head(coords(surf))
}
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpFhvBlN/temp_libpathff9947c5f204/neurosurf/extdata/std.8_lh.smoothwm.asc
#> 
#>  SurfaceGeometry 
#> 
#>     /\     
#>    /  \    
#>   /____\   
#>  /      \  
#> /        \   
#> 
#>   Basic Information: 
#>   Hemisphere: left
#>   Vertices:   642
#>   Faces:      1,280
#>   Edges:      1,920
#> 
#>  Geometry Metrics: 
#>   Euler Characteristic: 2
#>   Genus:               0
#>   Surface Area:        36956
#>   Avg Edge Length:     10.31
#> 
#>           [,1]       [,2]       [,3]
#> [1,] -32.43124  -5.756156 -26.852390
#> [2,] -38.78497  10.942583  52.546612
#> [3,] -26.23813 -35.324768  58.413246
#> [4,] -29.04933 -73.917900 -10.296215
#> [5,] -39.65442  20.463083   9.974921
#> [6,] -44.23693 -70.911591  28.850304
# }
```
