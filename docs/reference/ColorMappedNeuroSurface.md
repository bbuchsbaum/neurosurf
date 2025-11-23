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
surf_file <- system.file("extdata", "std.8.lh.inflated.asc", package = "neurosurf")
surf_geom <- read_surf_geometry(surf_file)
#> Error in SurfaceGeometrySource(surface_name): file.exists(surface_name) is not TRUE

# Get vertex count and generate some random data
n_verts <- nrow(coords(surf_geom))
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'coords': object 'surf_geom' not found
set.seed(123)
vertex_data <- rnorm(n_verts)
#> Error: object 'n_verts' not found

# Define indices (all vertices in this case)
vertex_indices <- 1:n_verts
#> Error: object 'n_verts' not found

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
#> Error: object 'surf_geom' not found

# Print the object summary
print(mapped_surf)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'print': object 'mapped_surf' not found

# The object can now be plotted, and the plotting function will use
# the stored cmap, irange, and thresh parameters by default.
# plot(mapped_surf) # Requires rgl package
```
