# Introduction to NeuroSurf Data Structures

This vignette introduces the fundamental data structures used in the
`neurosurf` package for representing and working with brain surface
geometries and associated data.

For visualization, see the companion vignettes:

- [`vignette("displaying-surfaces")`](../articles/displaying-surfaces.md)
  for interactive 3D plots based on
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
  [`view_surface()`](../reference/view_surface.md).
- [`vignette("interactive-surfaces")`](../articles/interactive-surfaces.md)
  for HTML-widget based exploration.
- [`vignette("surface-figures")`](../articles/surface-figures.md) for
  multi-view, publication-quality layouts built with
  [`surface_plot()`](../reference/surface_plot.md) and friends.

## Setup

First, let’s load the necessary libraries and set up the environment.

## `SurfaceGeometry`: Representing the Mesh

The core building block for any surface analysis is the geometry itself.
In `neurosurf`, this is represented by the `SurfaceGeometry` class.

An object of this class encapsulates:

1.  **`mesh`**: An
    [`rgl::mesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html)
    object containing the raw vertex coordinates and the triangular
    faces that define the surface shape.
2.  **`graph`**: An `igraph` object representing the connectivity of the
    mesh vertices. Edges connect adjacent vertices along the surface.
3.  **`hemi`**: A character string indicating the hemisphere, typically
    “lh” (left) or “rh” (right).

### Loading a `SurfaceGeometry`

You can load a surface geometry from various file formats (Freesurfer
binary, Freesurfer ASCII `.asc`, GIFTI `.gii`) using the
[`read_surf_geometry()`](../reference/read_surf_geometry.md) function.

``` r
# Load the example left hemisphere white matter surface (.asc format)
lh_geom <- read_surf_geometry(white_lh_asc)

# Display summary information about the loaded geometry
show(lh_geom)
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
```

### Accessing Geometry Properties

Several methods allow you to access information within the
`SurfaceGeometry` object:

``` r
# Total number of vertices (nodes) in the geometry
length(nodes(lh_geom))
#> [1] 642

# Vertex coordinates as an N x 3 matrix; its dimensions and first 3 rows
vertex_coords <- coords(lh_geom)
dim(vertex_coords)
#> [1] 642   3
head(vertex_coords, 3)
#>           [,1]       [,2]      [,3]
#> [1,] -32.43124  -5.756156 -26.85239
#> [2,] -38.78497  10.942583  52.54661
#> [3,] -26.23813 -35.324768  58.41325

# The underlying igraph connectivity object
neurosurf::graph(lh_geom)
#> IGRAPH 1c48116 U--- 642 1920 -- 
#> + attr: x (v/n), y (v/n), z (v/n), dist (e/n)
#> + edges from 1c48116:
#>  [1]  1-- 13  1-- 27  1-- 55  1-- 90  1--125  2--188  2--195  2--223  2--258
#> [10]  2--293  3--264  3--271  3--300  3--356  3--391  4-- 96  4--103  4--132
#> [19]  4--517  4--545  5-- 19  5-- 26  5-- 62  5--201  5--334  6--397  6--404
#> [28]  6--426  6--482  6--551  7-- 20  7-- 33  7--166  7--194  7--230  8--362
#> [37]  8--369  8--398  8--510  8--523  9--229  9--236  9--265  9--363  9--600
#> [46] 10-- 61 10-- 68 10-- 97 10--460 10--488 11--299 11--306 11--328 11--432
#> [55] 11--454 12--131 12--138 12--160 12--516 12--594 13-- 14 13-- 27 13-- 34
#> [64] 13-- 55 13-- 69 14-- 15 14-- 34 14-- 40 14-- 69 14-- 70 15-- 16 15-- 40
#> + ... omitted several edges

# The underlying rgl mesh3d object, and the hemisphere label
class(lh_geom@mesh)
#> [1] "mesh3d"  "shape3d"
lh_geom@hemi
#> [1] "lh"
```

## `NeuroSurface`: Mapping Data to Geometry

Often, we want to associate data values (like cortical thickness, fMRI
activation, etc.) with each vertex on the surface. The `NeuroSurface`
class links a *single vector* of data to a `SurfaceGeometry`.

It contains:

1.  **`geometry`**: The associated `SurfaceGeometry` object.
2.  **`indices`**: An integer vector specifying *which* vertices in the
    geometry have corresponding data values. This allows for
    representing data defined only on a subset of the surface.
3.  **`data`**: A numeric vector containing the data values. Its length
    must match the length of `indices`.

### Creating a `NeuroSurface`

You typically create a `NeuroSurface` using its constructor, providing
the geometry, indices, and data.

``` r
# Generate some example data (e.g., based on x-coordinate)
# Use all vertices from lh_geom
vertex_indices <- nodes(lh_geom)
example_data <- coords(lh_geom)[, 1] # Use x-coordinate as data

# Create the NeuroSurface object
lh_surf_data <- NeuroSurface(geometry = lh_geom, 
                               indices = vertex_indices, 
                               data = example_data)

# Display summary
show(lh_surf_data)
#> 
#>  NeuroSurface  
#> 
#>   Geometry & Data Mapping: 
#>   Hemisphere:         lh
#>   Total Vertices:   642
#>   Vertices w/ Data:642
#> 
#>   Data Summary: 
#>   Min:    -65.39
#>   Median:-29.95
#>   Mean:  -29.35
#>   Max:    1.209
```

### Loading Data with `read_surf` (Optional Data File)

If your data is stored in a separate file (e.g., AFNI `.1D.dset`, NIML
`.niml.dset`), you can load both the geometry and the data using
`read_surf`. Here we create a temporary `.1D.dset` file for
demonstration.

``` r
# 1. Prepare sample data for a subset of nodes
sample_nodes_indices_R <- sample(nodes(lh_geom), size = 500) # R indices (1-based)
sample_nodes_indices_0based <- sample_nodes_indices_R - 1     # 0-based for .1D file
sample_data <- rnorm(500) 

# 2. Create a temporary file
temp_dset_file <- tempfile(fileext = ".1D.dset")

# 3. Write data in AFNI .1D format (node_index value)
write.table(cbind(sample_nodes_indices_0based, sample_data), 
            file = temp_dset_file, 
            row.names = FALSE, 
            col.names = FALSE, 
            sep = " ")

# 4. Load geometry and the data from the temporary file
#    read_surf will match nodes in the file to the geometry
lh_surf_loaded <- read_surf(surface_name = white_lh_asc, 
                                surface_data_name = temp_dset_file)

# Display summary of the loaded NeuroSurface
show(lh_surf_loaded)
#> 
#>  NeuroSurface  
#> 
#>   Geometry & Data Mapping: 
#>   Hemisphere:         lh
#>   Total Vertices:   642
#>   Vertices w/ Data:500
#> 
#>   Data Summary: 
#>   Min:    -2.82
#>   Median:-0.01911
#>   Mean:  -0.03667
#>   Max:    2.689

# Number of data points loaded, and how many are non-zero (should match size = 500)
length(lh_surf_loaded@data)
#> [1] 500
sum(lh_surf_loaded@data != 0)
#> [1] 500
```

### Accessing `NeuroSurface` Properties

``` r
# The geometry can be retrieved and matches the original
identical(geometry(lh_surf_data), lh_geom)
#> [1] TRUE

# The data vector, via direct slot access (lh_surf_data@data) or as.vector();
# its length and first few values
data_vec <- lh_surf_data@data
length(data_vec)
#> [1] 642
head(data_vec, 5)
#> [1] -32.43124 -38.78497 -26.23813 -29.04933 -39.65442

# The associated vertex indices
index_vec <- indices(lh_surf_data)
length(index_vec)
#> [1] 642
head(index_vec, 5)
#> [1] 1 2 3 4 5
```

## `NeuroSurfaceVector`: Mapping Multiple Data Vectors

When you have multiple measurements per vertex (e.g., time series data,
multiple features), the `NeuroSurfaceVector` class is used. It links a
*matrix* of data to a `SurfaceGeometry`.

It contains:

1.  **`geometry`**: The associated `SurfaceGeometry` object.
2.  **`indices`**: An integer vector specifying *which* vertices have
    data (same as `NeuroSurface`).
3.  **`data`**: A `Matrix` (from the `Matrix` package, often sparse)
    where *rows* correspond to vertices (matching `indices`) and
    *columns* represent different measurements or time points.

### Creating a `NeuroSurfaceVector`

``` r
# Create example matrix data (e.g., x, y, z coordinates as 3 'features')
# Use all vertices
num_vertices <- length(nodes(lh_geom))
vertex_indices_vec <- nodes(lh_geom)

# Create a dense matrix first
example_matrix_data <- coords(lh_geom) 

# Convert to a Matrix object (can be sparse or dense)
example_matrix <- Matrix(example_matrix_data)

# Create the NeuroSurfaceVector
lh_surf_vec <- NeuroSurfaceVector(geometry = lh_geom,
                                    indices = vertex_indices_vec,
                                    mat = example_matrix)

# Display summary
show(lh_surf_vec)
#> 
#>  NeuroSurfaceVector  
#> 
#>   Geometry & Data Mapping: 
#>   Hemisphere:         lh
#>   Total Vertices:   642
#>   Vertices w/ Data:642
#> 
#>   Data Matrix Information: 
#>   Number of Vectors:3
#>   Data Dimensions:  [642 rows x 3 cols]
#>   Matrix Class:     dgeMatrix
```

### Accessing `NeuroSurfaceVector` Properties

``` r
# The geometry matches the original
identical(geometry(lh_surf_vec), lh_geom)
#> [1] TRUE

# The data matrix (sparse Matrix slot, or as.matrix()); its dimensions
data_mat <- lh_surf_vec@data
dim(data_mat)
#> [1] 642   3

# Standard matrix indexing: one vertex (row), or one column across vertices
data_mat[10, ]
#> [1] -48.594086 -23.064110  -8.115485
head(data_mat[, 2])
#> [1]  -5.756156  10.942583 -35.324768 -73.917900  20.463083 -70.911591

# The associated vertex indices
length(indices(lh_surf_vec))
#> [1] 642
```

## Specialized NeuroSurface Classes

`neurosurf` also provides specialized classes that inherit from
`NeuroSurface` and add features primarily for visualization:

- **`LabeledNeuroSurface`**: Associates categorical labels and
  corresponding colors with vertices (useful for atlases or
  parcellations). Contains `labels` and `cols` slots.
- **`ColorMappedNeuroSurface`**: Stores data along with a specific
  colormap (`cmap`), data range (`irange`), and thresholds (`thresh`) to
  pre-define how the data should be visualized.
- **`VertexColoredNeuroSurface`**: Stores specific hex color codes
  directly for each vertex (`colors` slot), bypassing data mapping.

These classes facilitate plotting with pre-set visual parameters using
the [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method
discussed in
[`vignette("displaying-surfaces")`](../articles/displaying-surfaces.md).

## Next Steps

Now that you understand the core data structures, explore the
visualization vignettes:

- [`vignette("displaying-surfaces")`](../articles/displaying-surfaces.md)
  — render surfaces with RGL, overlay data, and snapshot to PNG.
- [`vignette("interactive-surfaces")`](../articles/interactive-surfaces.md)
  — create interactive HTML widgets with
  [`surfwidget()`](../reference/surfwidget-methods.md).
- [`vignette("surface-figures")`](../articles/surface-figures.md) —
  build publication-quality multi-view figures with colourbars and atlas
  outlines.
