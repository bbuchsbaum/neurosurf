# load_data

Loads surface geometry data from a source object.

## Usage

``` r
# S4 method for class 'NeuroSurfaceVectorSource'
load_data(x)

# S4 method for class 'NeuroSurfaceSource'
load_data(x)

# S4 method for class 'FreesurferSurfaceGeometryMetaInfo'
load_data(x)

# S4 method for class 'GIFTISurfaceGeometryMetaInfo'
load_data(x)

# S4 method for class 'SurfaceGeometrySource'
load_data(x)
```

## Arguments

- x:

  the object to load data from

## Value

A `SurfaceGeometry` object containing the loaded mesh data
