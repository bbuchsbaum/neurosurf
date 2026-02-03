# SurfaceGeometryMetaInfo Class

The \`SurfaceGeometryMetaInfo\` class encapsulates meta information for
brain surface geometry. It stores details about the file locations,
surface properties, and spatial characteristics.

## Value

An object of class `SurfaceGeometryMetaInfo`.

## Details

This class is crucial for maintaining metadata about brain surface
geometries. It provides a structured way to store information about file
locations, surface properties, and spatial characteristics, which is
essential for proper handling and processing of brain surface data in
neuroimaging analyses.

## Slots

- `header_file`:

  A character string specifying the name of the file containing meta
  information.

- `data_file`:

  A character string specifying the name of the file containing the
  actual surface data.

- `file_descriptor`:

  An object of class \`FileFormat\` describing the image file format.

- `vertices`:

  An integer indicating the number of surface vertices.

- `faces`:

  An integer indicating the number of faces in the surface mesh.

- `embed_dimension`:

  An integer specifying the dimensionality of the embedding (typically 3
  for 3D surfaces).

- `label`:

  A character string indicating the type of surface (e.g., "white",
  "pial", "inflated", "flat", "spherical").

- `hemi`:

  A character string indicating the hemisphere ("lh" for left, "rh" for
  right, or "unknown").

## Examples

``` r
# \donttest{
meta_info <- new("SurfaceGeometryMetaInfo",
                 header_file = "surface_meta.txt",
                 data_file = "surface_data.gii",
                 file_descriptor = new("FileFormat"),
                 vertices = 40000L,
                 faces = 79998L,
                 embed_dimension = 3L,
                 label = "white",
                 hemi = "lh")
# }
```
