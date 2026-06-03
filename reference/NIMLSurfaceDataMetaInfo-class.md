# NIMLSurfaceDataMetaInfo

This class contains meta information for surface-based data for the NIML
data format

## Value

An object of class `NIMLSurfaceDataMetaInfo`.

## Slots

- `data`:

  the numeric data matrix of surface values (rows = nodes,
  columns=surface vectors)

- `node_indices`:

  the indices of the nodes for mapping to associated surface geometry.

## Examples

``` r
# \donttest{
meta_info <- new("SurfaceDataMetaInfo",
                 header_file = "data_header.txt",
                 data_file = "surface_data.niml.dset",
                 file_descriptor = new("FileFormat"),
                 node_count = length(nodes(geometry)),
                 nels = 2L,
                 label = "thickness")

surface_data <- matrix(rnorm(meta_info@node_count * meta_info@nels),
                       nrow = meta_info@node_count, ncol = meta_info@nels)

niml_meta <- new("NIMLSurfaceDataMetaInfo",
                header_file = meta_info@header_file,
                data_file = meta_info@data_file,
                file_descriptor = meta_info@file_descriptor,
                node_count = meta_info@node_count,
                nels = meta_info@nels,
                label = meta_info@label,
                data = surface_data,
                node_indices = seq_len(meta_info@node_count))
# }
```
