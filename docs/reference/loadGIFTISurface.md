# Load GIFTI surface geometry

Load GIFTI surface geometry

## Usage

``` r
loadGIFTISurface(meta_info)
```

## Arguments

- meta_info:

  instance of type `GIFTISurfaceGeometryMetaInfo`

## Value

a class of type `SurfaceGeometry`

## Details

requires rgl library

## Examples

``` r
# \donttest{
# Requires GIFTI surface file
# meta <- read_meta_info(GIFTISurfaceFileDescriptor(), "surface.gii")
# geom <- loadGIFTISurface(meta)
# }
```
