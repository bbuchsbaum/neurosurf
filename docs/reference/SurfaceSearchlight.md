# SurfaceSearchlight

Creates an iterator that systematically samples searchlight regions on a
surface mesh using geodesic distance to define regions.

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

  A [`SurfaceGeometry`](SurfaceGeometry-class.md) object representing
  the surface mesh.

- radius:

  Numeric, radius of the searchlight as a geodesic distance in mm. Must
  be a positive, length-one value.

- nodeset:

  Integer vector, optional subset of surface node indices to use. If
  provided, the vector must contain at least one node index.

- distance_type:

  Character, the distance metric to use: "euclidean", "geodesic", or
  "spherical".

- as_deflist:

  Logical, whether to return a deflist object.

## Value

An iterator object of class "Searchlight".

## Details

Create a Searchlight iterator for surface mesh using geodesic distance
to define regions.

This function creates a systematic searchlight iterator, which visits
each node of the surface mesh in order. The searchlight region for each
node is defined by the specified radius and distance metric.

## Examples

``` r
# \donttest{
file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
geom <- read_surf(file)
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/RtmpQdM37p/temp_libpatha900461f6b09/neurosurf/extdata/std.8_lh.smoothwm.asc
searchlight <- SurfaceSearchlight(geom, 12, distance_type = "geodesic")
nodes <- searchlight$nextElem()
# }
```
