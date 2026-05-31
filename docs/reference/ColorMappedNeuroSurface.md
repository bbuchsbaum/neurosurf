# ColorMappedNeuroSurface

This function creates a ColorMappedNeuroSurface object, which represents
a single set of data values associated with nodes on a surface geometry,
with pre-defined color mapping parameters.

## Usage

``` r
ColorMappedNeuroSurface(geometry, indices, data, cmap, irange, thresh)
```

## Arguments

- geometry:

  A `SurfaceGeometry` object representing the underlying surface
  structure.

- indices:

  An integer vector specifying the indices of valid surface nodes.

- data:

  A numeric vector of data values corresponding to the surface nodes.

- cmap:

  A character string specifying the colormap to use for mapping the data
  values.

- irange:

  A numeric vector of length 2 specifying the range of values to map.

- thresh:

  A numeric value specifying the threshold for the colormap.

## Value

An instance of class `ColorMappedNeuroSurface`.

## Details

This object bundles the surface geometry, data, and specific color
mapping parameters ('cmap', 'irange', 'thresh'). This is useful for
ensuring consistent visualization across different plots or for saving a
predefined view. The actual application of the color map happens during
rendering (e.g., when using 'plot()”).

## See also

[`SurfaceGeometry`](SurfaceGeometry.md),
[`NeuroSurface`](NeuroSurface.md)

## Examples

``` r
# Load a sample surface geometry
surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
surf_geom <- read_surf_geometry(surf_file)
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpzRF1jx/temp_libpathf6cf495866b/neurosurf/extdata/std.8_lh.inflated.asc

# Get vertex count and generate some random data
n_verts <- nrow(coords(surf_geom))
set.seed(123)
vertex_data <- rnorm(n_verts)

# Define indices (all vertices in this case)
vertex_indices <- 1:n_verts

# Define color mapping parameters
my_cmap <- colorRampPalette(c("blue", "white", "red"))(256) # Blue-white-red colormap
my_irange <- c(-2, 2) # Map data values from -2 to 2 onto the colormap
my_thresh <- c(-1, 1) # Define thresholds (e.g., for transparency later)

# Create the ColorMappedNeuroSurface object
mapped_surf <- ColorMappedNeuroSurface(geometry = surf_geom,
                                       indices = vertex_indices,
                                       data = vertex_data,
                                       cmap = my_cmap,
                                       irange = my_irange,
                                       thresh = my_thresh)

# Print the object summary
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
#>   Min:    -2.81
#>   Median:-0.02167
#>   Mean:  0.004774
#>   Max:    3.241
#> 

# The object can now be plotted, and the plotting function will use
# the stored cmap, irange, and thresh parameters by default.
# plot(mapped_surf) # Requires rgl package
```
