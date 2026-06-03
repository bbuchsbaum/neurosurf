# load Freesurfer ascii surface

load Freesurfer ascii surface

## Usage

``` r
loadFSSurface(meta_info, surf_to_world = diag(4))
```

## Arguments

- meta_info:

  instance of type `FreesurferSurfaceGeometryMetaInfo`

- surf_to_world:

  Optional 4x4 affine transformation matrix from surface (tkr-RAS)
  coordinates to world (scanner RAS) coordinates. Defaults to identity.
  For proper alignment with volumes, this transform can be computed from
  FreeSurfer's vox2ras-tkr matrix (available in orig.mgz or via mri_info
  –vox2ras-tkr).

## Value

a class of type `SurfaceGeometry`

## Details

requires rgl library

## Examples

``` r
# \donttest{
# Requires FreeSurfer surface file
# meta <- read_meta_info(FreesurferAsciiSurfaceFileDescriptor(), "lh.pial.asc")
# geom <- loadFSSurface(meta)
# }
```
