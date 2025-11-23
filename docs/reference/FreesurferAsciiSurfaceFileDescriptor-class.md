# FresurferAsciiSurfaceFileDescriptor

This class supports the Freesurfer Ascii file format for surface
geometry

## Examples

``` r
# \donttest{
# Create a FreesurferAsciiSurfaceFileDescriptor object
fs_ascii_descriptor <- new("FreesurferAsciiSurfaceFileDescriptor")

# This descriptor would typically be used when loading Freesurfer ASCII surfaces
# Example of a path to a Freesurfer ASCII surface file:
# fs_ascii_file <- "/path/to/subject/surf/lh.pial.asc"

# In practice, this descriptor would be used in code like:
# surface_geo <- loadSurfaceGeometry(fs_ascii_file, descriptor = fs_ascii_descriptor)
# }
```
