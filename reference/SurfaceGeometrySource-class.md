# SurfaceGeometrySource Class

The \`SurfaceGeometrySource\` class serves as a factory for creating
[`SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry-class.md)
instances. It encapsulates the meta information required to construct a
surface geometry.

## Value

An object of class `SurfaceGeometrySource`.

## Details

This class is designed to facilitate the creation of
[`SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry-class.md)
objects by providing a standardized way to store and access the required
metadata. It acts as an intermediate step in the process of loading and
constructing surface geometries from various file formats and sources.

## Slots

- `meta_info`:

  An object of class
  [`SurfaceGeometryMetaInfo`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometryMetaInfo-class.md)
  containing the metadata necessary for creating a surface geometry.

## See also

[`SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry-class.md),
[`SurfaceGeometryMetaInfo`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometryMetaInfo-class.md)

## Examples

``` r
# \donttest{
# Create a SurfaceGeometryMetaInfo object
meta_info <- new("SurfaceGeometryMetaInfo",
                 header_file = "surface_meta.txt",
                 data_file = "surface_data.gii",
                 file_descriptor = new("FileFormat"),
                 vertices = 40000L,
                 faces = 79998L,
                 embed_dimension = 3L,
                 label = "white",
                 hemi = "lh")

# Create a SurfaceGeometrySource object
geom_source <- new("SurfaceGeometrySource", meta_info = meta_info)

# Use geom_source to create a SurfaceGeometry object (hypothetical function)
# surface_geometry <- createSurfaceGeometry(geom_source)
# }
```
