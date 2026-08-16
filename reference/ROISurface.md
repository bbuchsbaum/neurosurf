# Create an instance of class [`ROISurface`](https://bbuchsbaum.github.io/neurosurf/reference/ROISurface-class.md)

Create an instance of class
[`ROISurface`](https://bbuchsbaum.github.io/neurosurf/reference/ROISurface-class.md)

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
faces <- matrix(c(0L,1L,2L), ncol=3, byrow=TRUE)
geom <- SurfaceGeometry(verts, faces, "lh")

ROISurface(geom, 1L, 1)
#> 
#> 
#>  ROISurface 
#>  size:  1 
#>  data type: vector 
#>  data dim: 1 
#>  vertex center of mass:  0 0 0 

try(ROISurface(geom, 4L, 1))      # out of range
#> Error in ROISurface(geom, 4L, 1) : 
#>   'indices' are out of bounds for provided 'geometry'
try(ROISurface(geom, 1.5, 1))     # non-integer
#> Error : 'indices' must contain integer-valued indices
# }
```
