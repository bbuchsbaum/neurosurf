# Create an instance of class [`ROISurfaceVector`](ROISurfaceVector-class.md)

Create an instance of class
[`ROISurfaceVector`](ROISurfaceVector-class.md)

## Usage

``` r
ROISurfaceVector(geometry, indices, data)
```

## Arguments

- geometry:

  the parent geometry: an instance of class `SurfaceGeometry`

- indices:

  the parent surface indices

- data:

  the data values, a `matrix`

## Value

an instance of class `ROISurfaceVector`

## Examples

``` r
# \donttest{
verts <- matrix(c(0,0,0,
                  1,0,0,
                  0,1,0), ncol=3, byrow=TRUE)
faces <- matrix(c(1,2,3), ncol=3, byrow=TRUE)
geom <- SurfaceGeometry(verts, faces, "lh")
#> Error in igraph::graph_from_edgelist(edges, directed = FALSE): graph_from_edgelist expects a matrix with two columns.

vec <- matrix(c(0.5, 1.5), nrow=1)
ROISurfaceVector(geom, c(1L,2L), vec)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'nodes': object 'geom' not found

try(ROISurfaceVector(geom, c(1L,4L), vec))   # out of range
#> Error in h(simpleError(msg, call)) : 
#>   error in evaluating the argument 'x' in selecting a method for function 'nodes': object 'geom' not found
try(ROISurfaceVector(geom, c(1,2.5), vec))   # non-integer
#> Error in h(simpleError(msg, call)) : 
#>   error in evaluating the argument 'x' in selecting a method for function 'nodes': object 'geom' not found
# }
```
