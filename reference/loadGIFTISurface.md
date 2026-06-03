# Load GIFTI surface geometry

Loads a GIFTI (`.surf.gii`) surface into a
[`SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry-class.md).
The usual public entry point for reading a surface from disk is
[`read_surf_geometry`](https://bbuchsbaum.github.io/neurosurf/reference/read_surf_geometry.md)
/
[`read_surf`](https://bbuchsbaum.github.io/neurosurf/reference/read_surf.md);
this function is the GIFTI-specific loader it dispatches to. For
convenience it also accepts a file path directly, in which case the
header is read internally.

## Usage

``` r
loadGIFTISurface(meta_info)
```

## Arguments

- meta_info:

  either a `GIFTISurfaceGeometryMetaInfo` instance, or a length-one
  character path to a `.surf.gii` / `.gii` file.

## Value

a class of type `SurfaceGeometry`

## Details

requires rgl library

## Examples

``` r
# \donttest{
# Either pass a path directly ...
# geom <- loadGIFTISurface("lh.midthickness.surf.gii")
# ... or go through the meta-info object:
# meta <- read_meta_info(neurosurf:::GIFTI_SURFACE_DSET, "lh.midthickness.surf.gii")
# geom <- loadGIFTISurface(meta)
# }
```
