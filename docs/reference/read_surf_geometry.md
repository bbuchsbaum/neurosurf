# Read Surface Geometry from File

This function loads a supported surface geometry from a file and returns
a SurfaceGeometry object.

## Usage

``` r
read_surf_geometry(surface_name)
```

## Arguments

- surface_name:

  A character string specifying the name of the file containing the
  surface geometry.

## Value

A `SurfaceGeometry` object representing the loaded surface.

## Details

This function supports loading surface geometries from the following
file formats:

- Freesurfer ASCII (.asc)

- Freesurfer binary

- GIFTI (.gii)

The appropriate loader is automatically selected based on the file
extension or content.

## Examples

``` r
# \donttest{
asc_path <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
lh_surface <- read_surf_geometry(asc_path)
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpzRF1jx/temp_libpathf6cf495866b/neurosurf/extdata/std.8_lh.inflated.asc
# }

```
