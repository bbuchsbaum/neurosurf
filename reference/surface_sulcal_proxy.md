# Approximate sulcal depth from white and inflated surfaces

Estimates a FreeSurfer-style sulcal depth map as the signed displacement
between a white (or pial) surface and its inflated counterpart, measured
along the inflated surface normals after matching the two meshes'
centres and RMS radii. Positive values mark gyral crowns and negative
values sulcal fundi. Both geometries must share topology.

## Usage

``` r
surface_sulcal_proxy(white, inflated, iterations = 4L)
```

## Arguments

- white:

  Folded \`SurfaceGeometry\` (white or pial).

- inflated:

  Inflated \`SurfaceGeometry\` with the same vertices and faces.

- iterations:

  Laplacian smoothing iterations applied to the result.

## Value

A numeric vector with one value per vertex.
