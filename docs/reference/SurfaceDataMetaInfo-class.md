# SurfaceDataMetaInfo

This class contains meta information for surface-based data (the values
that map to a surface geometry)

## Slots

- `header_file`:

  name of the file containing meta information

- `data_file`:

  name of the file containing data

- `file_descriptor`:

  descriptor of image file format

- `node_count`:

  the number of nodes for which surface data exists

- `nels`:

  the number of data vectors (typically the number of columns in the
  surface data matrix; nels = 1 for a single surface data set)

- `label`:

  a label indicating the type of surface (e.g. white, pial, inflated,
  flat, spherical)

## Examples

``` r
# \donttest{
# Create a SurfaceDataMetaInfo object
meta_info <- new("SurfaceDataMetaInfo",
                 header_file = "data_header.txt",
                 data_file = "surface_data.1D",
                 file_descriptor = new("FileFormat"),
                 node_count = 10000L,
                 nels = 1L,
                 label = "thickness")
# }
```
