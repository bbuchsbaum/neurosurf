# Create an instance of class [`ROISurfaceVector`](https://bbuchsbaum.github.io/neurosurf/reference/ROISurfaceVector-class.md)

Create an instance of class
[`ROISurfaceVector`](https://bbuchsbaum.github.io/neurosurf/reference/ROISurfaceVector-class.md)

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
faces <- matrix(c(0L,1L,2L), ncol=3, byrow=TRUE)
geom <- SurfaceGeometry(verts, faces, "lh")

vec <- matrix(c(0.5, 1.5), nrow=1)
ROISurfaceVector(geom, c(1L,2L), vec)
#> An object of class "ROISurfaceVector"
#> Slot "geometry":
#> 
#>  SurfaceGeometry 
#> 
#>     /\     
#>    /  \    
#>   /____\   
#>  /      \  
#> /        \   
#> 
#>   Basic Information: 
#>   Hemisphere: left
#>   Vertices:   3
#>   Faces:      1
#>   Edges:      3
#> 
#>  Geometry Metrics: 
#>   Euler Characteristic: 1
#>   Genus:               0.5
#>   Surface Area:        0
#>   Avg Edge Length:     1.138
#> 
#> 
#> Slot "data":
#>      [,1] [,2]
#> [1,]  0.5  1.5
#> 
#> Slot "coords":
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]    1    0    0
#> 
#> Slot "indices":
#> [1] 1 2
#> 

try(ROISurfaceVector(geom, c(1L,4L), vec))   # out of range
#> Error in ROISurfaceVector(geom, c(1L, 4L), vec) : 
#>   'indices' are out of bounds for provided 'geometry'
try(ROISurfaceVector(geom, c(1,2.5), vec))   # non-integer
#> Error : 'indices' must contain integer-valued indices
# }
```
