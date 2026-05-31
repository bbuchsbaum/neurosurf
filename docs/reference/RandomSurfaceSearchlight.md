# Create a Random Searchlight iterator for surface mesh

Creates an iterator that randomly samples searchlight regions on a
surface mesh using geodesic distance to define regions.

## Usage

``` r
RandomSurfaceSearchlight(
  surfgeom,
  radius = 8,
  nodeset = NULL,
  as_deflist = FALSE
)
```

## Arguments

- surfgeom:

  A [`SurfaceGeometry`](SurfaceGeometry-class.md) object representing
  the surface mesh.

- radius:

  Numeric, radius of the searchlight as a geodesic distance in mm. Must
  be a positive numeric scalar.

- nodeset:

  Integer vector, optional subset of surface node indices to use. If
  supplied, must contain more than one index.

- as_deflist:

  Logical, whether to return a deflist object.

## Value

An iterator object of class "RandomSurfaceSearchlight".

## Details

On each call to `nextElem`, a set of surface nodes is returned. These
nodes index into the vertices of the `igraph` instance. When
`as_deflist=TRUE`, the random ordering of centers is fixed when the
object is created. Use `set.seed` before construction for reproducible
sequences.

## Examples

``` r
# \donttest{
file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
geom <- read_surf(file)
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpzRF1jx/temp_libpathf6cf495866b/neurosurf/extdata/std.8_lh.smoothwm.asc
searchlight <- RandomSurfaceSearchlight(geom, 12)
set.seed(42)
dl <- RandomSurfaceSearchlight(geom, 12, as_deflist=TRUE)
attr(dl[[1]], "center")
#> [1] 561
nodes <- searchlight$nextElem()
length(nodes) > 1
#> [1] TRUE
# }
```
