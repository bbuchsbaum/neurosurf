# Create a Random Searchlight iterator for surface mesh

Creates an iterator that randomly samples searchlight regions on a
surface mesh using the selected distance metric (geodesic by default) to
define regions.

## Usage

``` r
RandomSurfaceSearchlight(
  surfgeom,
  radius = 8,
  nodeset = NULL,
  as_deflist = FALSE,
  distance_type = c("geodesic", "euclidean", "spherical")
)
```

## Arguments

- surfgeom:

  A
  [`SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry-class.md)
  object representing the surface mesh.

- radius:

  Numeric, radius of the searchlight in mm, measured with the metric
  selected by `distance_type` (geodesic by default). Must be a positive
  numeric scalar.

- nodeset:

  Integer vector, optional subset of surface node indices to use. If
  supplied, must contain more than one index.

- as_deflist:

  Logical, whether to return a deflist object.

- distance_type:

  Character, the distance metric to use: `"geodesic"` (the default),
  `"euclidean"`, or `"spherical"`. The default is retained for
  compatibility and differs from
  [`SurfaceSearchlight()`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceSearchlight.md),
  which defaults to Euclidean distance.

## Value

An iterator object of class "RandomSurfaceSearchlight".

## Details

On each call to `nextElem`, a set of surface nodes is returned. These
nodes index into the vertices of the `igraph` instance. When
`as_deflist=TRUE`, the random ordering of centers is fixed when the
object is created. Use `set.seed` before construction for reproducible
sequences.

Pass `distance_type` explicitly to both searchlight constructors when
comparing systematic and random sampling. Geodesic distance follows
shortest paths along mesh edges; Euclidean distance can include nearby
vertices on opposite sides of a fold. Use spherical distance only for a
spherical surface.

## Examples

``` r
# \donttest{
file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
geom <- read_surf(file)
#> loading /home/runner/work/_temp/Library/neurosurf/extdata/std.8_lh.smoothwm.asc
searchlight <- RandomSurfaceSearchlight(geom, 12)
set.seed(42)
dl <- RandomSurfaceSearchlight(geom, 12, as_deflist=TRUE)
attr(dl[[1]], "center")
#> [1] 561
nodes <- searchlight$nextElem()
length(nodes) > 1
#> [1] TRUE
# }
# Match the distance metric across both sampling strategies.
geom <- example_surface_geometry()
standard <- SurfaceSearchlight(geom, 1.5, distance_type = "euclidean")
set.seed(42)
random <- RandomSurfaceSearchlight(geom, 1.5, distance_type = "euclidean")
random$nextElem()
#> [1] 1 2 3 4
#> attr(,"center")
#> [1] 1
#> attr(,"center.index")
#> [1] 1
#> attr(,"length")
#> [1] 4
```
