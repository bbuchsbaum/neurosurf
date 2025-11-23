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
if (FALSE) { # \dontrun{
# Load a FreeSurfer surface
lh_surface <- read_surf_geometry("lh.pial")

# Load a GIFTI surface
rh_surface <- read_surf_geometry("rh.white.gii")
} # }

```
