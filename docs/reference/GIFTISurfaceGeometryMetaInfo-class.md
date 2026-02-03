# GIFTISurfaceGeometryMetaInfo

This class contains meta information for surface-based geometry for the
GIFTI data format

## Value

An object of class `GIFTISurfaceGeometryMetaInfo`.

## Slots

- `info`:

  the underlying `gifti` object returned by
  [`readgii`](https://rdrr.io/pkg/gifti/man/readgii.html)

## Examples

``` r
# \donttest{
# First create a SurfaceGeometryMetaInfo parent object
meta_info <- new("SurfaceGeometryMetaInfo",
                 header_file = "rscan01_lh.gii",
                 data_file = "rscan01_lh.gii",
                 file_descriptor = new("FileFormat"),
                 vertices = 40000L,
                 faces = 79998L,
                 embed_dimension = 3L,
                 label = "white",
                 hemi = "lh")

# Use the example file included in the package
example_gii_file <- system.file("extdata", "rscan01_lh.gii", package = "neurostyle")

if (file.exists(example_gii_file) && requireNamespace("gifti", quietly = TRUE)) {
  # Load the actual GIFTI file
  gifti_obj <- gifti::readgii(example_gii_file)

  # Create GIFTISurfaceGeometryMetaInfo object with the real file
  gifti_meta <- new("GIFTISurfaceGeometryMetaInfo",
                   header_file = meta_info@header_file,
                   data_file = meta_info@data_file,
                   file_descriptor = meta_info@file_descriptor,
                   vertices = meta_info@vertices,
                   faces = meta_info@faces,
                   embed_dimension = meta_info@embed_dimension,
                   label = meta_info@label,
                   hemi = meta_info@hemi,
                   info = gifti_obj)
}
# }
```
