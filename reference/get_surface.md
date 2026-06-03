# Retrieve a geometry from a SurfaceSet

Retrieve a geometry from a SurfaceSet

## Usage

``` r
get_surface(x, label = NULL)
```

## Arguments

- x:

  SurfaceSet

- label:

  Label of the desired surface variant. Defaults to the set's default.

## Value

A \`SurfaceGeometry\` object.

## Examples

``` r
geom <- example_surface_geometry()
ss <- surface_set(inflated = geom)
get_surface(ss, "inflated")
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
#>   Vertices:   4
#>   Faces:      4
#>   Edges:      6
#> 
#>  Geometry Metrics: 
#>   Euler Characteristic: 2
#>   Genus:               0
#>   Surface Area:        0.5
#>   Avg Edge Length:     1.207
#> 
```
