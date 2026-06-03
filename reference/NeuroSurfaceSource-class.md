# NeuroSurfaceSource Class

The \`NeuroSurfaceSource\` class serves as a factory for creating
[`NeuroSurface`](https://bbuchsbaum.github.io/neurosurf/reference/NeuroSurface-class.md)
instances. It encapsulates all necessary information to construct a
neuroimaging surface, including geometry, metadata, and indexing
information.

## Usage

``` r
NeuroSurfaceSource(
  surface_geom,
  surface_data_name,
  colind = NULL,
  nodeind = NULL
)
```

## Arguments

- surface_geom:

  the name of the file containing the surface geometry or a
  `SurfaceGeometry` instance

- surface_data_name:

  the name of the file containing the data values to be mapped to the
  surface.

- colind:

  the subset of column indices to load from surface data matrix (if
  provided)

- nodeind:

  the subset of node indices to load from surface data matrix (if
  provided)

## Value

An object of class `NeuroSurfaceSource` or
[`NeuroSurfaceVectorSource`](https://bbuchsbaum.github.io/neurosurf/reference/NeuroSurfaceVectorSource-class.md)
that contains information about the surface geometry and associated
data. If the data has multiple columns (colind \> 1), a
`NeuroSurfaceVectorSource` is returned; otherwise, a
`NeuroSurfaceSource` is returned. These objects can be used to load and
map neuroimaging data onto brain surfaces.

## Details

This class is designed to facilitate the creation of
[`NeuroSurface`](https://bbuchsbaum.github.io/neurosurf/reference/NeuroSurface-class.md)
objects by providing a standardized way to store and access all required
components. It combines geometric information, metadata, and indexing
details necessary for constructing a complete neuroimaging surface
representation.

## Slots

- `geometry`:

  An object of class
  [`SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry-class.md)
  representing the underlying surface structure.

- `data_meta_info`:

  An object of class
  [`SurfaceDataMetaInfo`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceDataMetaInfo-class.md)
  containing metadata about the surface data.

- `colind`:

  An integer specifying the column index of the surface map to be
  loaded.

- `nodeind`:

  An integer vector specifying the node indices of the surface map to be
  loaded.

## See also

[`NeuroSurface`](https://bbuchsbaum.github.io/neurosurf/reference/NeuroSurface-class.md),
[`SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry-class.md),
[`SurfaceDataMetaInfo`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceDataMetaInfo-class.md)

## Examples

``` r
# \donttest{
# Create a simple mesh for the example
vertices <- c(
  0, 0, 0,
  1, 0, 0,
  0, 1, 0,
  0, 0, 1
)
triangles <- c(
  1, 2, 3,
  1, 2, 4,
  1, 3, 4,
  2, 3, 4
)

# Create mesh3d object
mesh <- rgl::mesh3d(vertices = vertices, triangles = triangles)

# Create a graph representation
edges <- rbind(
  c(1,2), c(1,3), c(1,4),
  c(2,3), c(2,4),
  c(3,4)
)
graph <- igraph::graph_from_edgelist(edges)

# Create a SurfaceGeometry object
geometry <- new("SurfaceGeometry",
               mesh = mesh,
               graph = graph,
               hemi = "left")

# Create a SurfaceDataMetaInfo object
data_meta_info <- new("SurfaceDataMetaInfo",
                      header_file = "data_meta.txt",
                      data_file = "surface_data.1D",
                      file_descriptor = new("FileFormat"),
                      node_count = 4L,
                      nels = 1L,
                      label = "thickness")

# Create a NeuroSurfaceSource object
neuro_source <- new("NeuroSurfaceSource",
                    geometry = geometry,
                    data_meta_info = data_meta_info,
                    colind = 1L,
                    nodeind = 1:4)
# }
```
