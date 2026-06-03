# List available surface labels

List available surface labels

## Usage

``` r
surface_labels(x)
```

## Arguments

- x:

  SurfaceSet

## Value

Character vector of labels

## Examples

``` r
geom <- example_surface_geometry()
ss <- surface_set(inflated = geom, pial = geom)
surface_labels(ss)
#> [1] "inflated" "pial"    
```
