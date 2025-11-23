# Create an instance of class [`ROISurface`](ROISurface-class.md)

Create an instance of class [`ROISurface`](ROISurface-class.md)

## Usage

``` r
ROISurface(geometry, indices, data)
```

## Arguments

- geometry:

  the parent geometry: an instance of class `SurfaceGeometry`

- indices:

  the parent surface indices

- data:

  the data values, numeric `vector`

## Value

an instance of class `ROISurface`

## Examples

``` r
# \donttest{
verts <- matrix(c(0,0,0,
                  1,0,0,
                  0,1,0), ncol=3, byrow=TRUE)
faces <- matrix(c(1,2,3), ncol=3, byrow=TRUE)
geom <- SurfaceGeometry(verts, faces, "lh")
#> Error in igraph::graph_from_edgelist(edges, directed = FALSE): graph_from_edgelist expects a matrix with two columns.

ROISurface(geom, 1L, 1)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'nodes': object 'geom' not found

try(ROISurface(geom, 4L, 1))      # out of range
#> Error in h(simpleError(msg, call)) : 
#>   error in evaluating the argument 'x' in selecting a method for function 'nodes': object 'geom' not found
try(ROISurface(geom, 1.5, 1))     # non-integer
#> Error in h(simpleError(msg, call)) : 
#>   error in evaluating the argument 'x' in selecting a method for function 'nodes': object 'geom' not found
# }
```
