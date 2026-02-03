# SurfaceGeometry Class

The \`SurfaceGeometry\` class represents a three-dimensional surface
consisting of a set of triangle vertices. It encapsulates the mesh
structure, graph representation, and hemisphere information of a brain
surface.

## Value

An object of class `SurfaceGeometry`.

## Details

This class is fundamental for representing brain surface geometries in
neuroimaging analyses. The mesh slot contains the vertex and face
information, while the graph slot provides a network representation of
the surface topology. The hemi slot specifies which hemisphere the
surface represents.

The `surf_to_world` transform enables proper projection between surface
and volume spaces: `world_pos = surf_to_world %*% c(vertex_pos, 1)`,
then `voxel_ijk = world_to_vox %*% world_pos`.

## Slots

- `mesh`:

  An object of class `mesh3d` representing the underlying 3D mesh
  structure.

- `graph`:

  An object of class `igraph` representing the underlying graph
  structure of the surface.

- `hemi`:

  A character string indicating the hemisphere of the surface ("left",
  "right", or "both").

- `label`:

  A character string describing the surface type (e.g., "pial", "white",
  "inflated", "sphere").

- `surf_to_world`:

  A 4x4 numeric matrix representing the affine transformation from
  surface coordinates to world (scanner RAS) coordinates. This handles
  coordinate system conventions (e.g., RAS vs LPI), reference space
  alignment (e.g., MNI305 vs MNI152), and any embedded transforms from
  file formats (e.g., GIFTI CoordinateSystemTransformMatrix). Defaults
  to the identity matrix (surface coordinates are assumed to be in world
  space).

## See also

[`mesh3d`](https://dmurdoch.github.io/rgl/dev/reference/mesh3d.html),
[`igraph`](https://r.igraph.org/reference/aaa-igraph-package.html),
[`surf_to_world`](surf_to_world-methods.md)

## Examples

``` r
# \donttest{
geometry <- example_surface_geometry()
# }
```
