# SurfaceSearchlight

Creates an iterator that systematically samples searchlight regions on a
surface mesh. Regions are defined by `distance_type`, which defaults to
Euclidean distance between vertex coordinates; pass
`distance_type = "geodesic"` for distance along the mesh.

## Usage

``` r
SurfaceSearchlight(
  surfgeom,
  radius = 8,
  nodeset = NULL,
  distance_type = c("euclidean", "geodesic", "spherical"),
  as_deflist = FALSE
)
```

## Arguments

- surfgeom:

  A
  [`SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry-class.md)
  object representing the surface mesh.

- radius:

  Numeric, radius of the searchlight in mm, measured with the metric
  selected by `distance_type` (Euclidean by default). Must be a
  positive, length-one value.

- nodeset:

  Integer vector, optional subset of surface node indices to use. If
  provided, the vector must contain at least one node index.

- distance_type:

  Character, the distance metric to use: `"euclidean"` (the default),
  `"geodesic"`, or `"spherical"`. Note that the default differs from
  [`neighbor_graph()`](https://bbuchsbaum.github.io/neurosurf/reference/neighbor_graph-methods.md)
  and
  [`RandomSurfaceSearchlight()`](https://bbuchsbaum.github.io/neurosurf/reference/RandomSurfaceSearchlight.md),
  which use geodesic distance.

- as_deflist:

  Logical, whether to return a deflist object.

## Value

An iterator object of class "Searchlight".

## Details

Create a systematic Searchlight iterator for a surface mesh.

This function creates a systematic searchlight iterator, which visits
each node of the surface mesh in order. The searchlight region for each
node is defined by the specified radius and distance metric. The
Euclidean default is retained for compatibility with existing analyses.
Pass `distance_type` explicitly to this function and
[`RandomSurfaceSearchlight()`](https://bbuchsbaum.github.io/neurosurf/reference/RandomSurfaceSearchlight.md)
to use the same metric for both sampling strategies. Geodesic distance
follows shortest paths along mesh edges; Euclidean distance can include
nearby vertices on opposite sides of a fold. Use spherical distance only
for a spherical surface.

## Examples

``` r
# \donttest{
file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
geom <- read_surf(file)
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_lh.smoothwm.asc
searchlight <- SurfaceSearchlight(geom, 12, distance_type = "geodesic")
nodes <- searchlight$nextElem()
# }
```
