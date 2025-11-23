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
  # Read the surface data
  surf <- read_surf(surf_file)

  # Display basic information about the surface
  print(surf)

  # Get summary statistics of the surface data
  summary(surf@data)

  # Visualize the surface if rgl is available
  if (requireNamespace("rgl", quietly = TRUE)) {
    # Plot the surface mesh
    rgl::open3d()
    rgl::shade3d(surf@geometry@mesh, col = "lightblue")
    rgl::title3d(main = "Example Surface")

    # If the surface has data values, color the mesh by these values
    if (length(surf@data) > 0) {
      # Normalize data to [0,1] for coloring
      norm_data <- (surf@data - min(surf@data)) / (max(surf@data) - min(surf@data))

      # Create a color palette
      colors <- grDevices::colorRampPalette(c("blue", "cyan", "green",
                                             "yellow", "red"))(100)

      # Map data values to colors
      col_idx <- ceiling(norm_data * 99) + 1
      vertex_colors <- colors[col_idx]

      # Plot colored mesh
      rgl::open3d()
      rgl::shade3d(surf@geometry@mesh, col = vertex_colors)
      rgl::title3d(main = "Surface Colored by Data Values")
    }
  }
} else {
  message("Example surface file not found. This may occur if the package ",
          "was installed without the example data.")
}
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpwYGE4M/temp_libpathbf0a5b9dbd99/neurosurf/extdata/std.8_lh.smoothwm.asc
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
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'object' in selecting a method for function 'summary': no slot of name "data" for this object of class "SurfaceGeometry"
# }
```
