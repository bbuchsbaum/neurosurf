# NIMLSurfaceDataMetaInfo

This class contains meta information for surface-based data for the NIML
data format

## Slots

- `data`:

  the numeric data matrix of surface values (rows = nodes,
  columns=surface vectors)

- `node_indices`:

  the indices of the nodes for mapping to associated surface geometry.

## Examples

``` r
# \donttest{
# Create a SurfaceDataMetaInfo parent object
meta_info <- new("SurfaceDataMetaInfo",
                 header_file = "data_header.txt",
                 data_file = "surface_data.niml.dset",
                 file_descriptor = new("FileFormat"),
                 node_count = 10000L,
                 nels = 2L,
                 label = "thickness")

# Create sample data matrix (10000 nodes, 2 data vectors)
surface_data <- matrix(rnorm(20000), nrow = 10000, ncol = 2)

# Create node indices
node_indices <- 1:10000

# Create NIMLSurfaceDataMetaInfo object
niml_meta <- new("NIMLSurfaceDataMetaInfo",
                header_file = meta_info@header_file,
                data_file = meta_info@data_file,
                file_descriptor = meta_info@file_descriptor,
                node_count = meta_info@node_count,
                nels = meta_info@nels,
                label = meta_info@label,
                data = surface_data,
                node_indices = node_indices)
# }
```
