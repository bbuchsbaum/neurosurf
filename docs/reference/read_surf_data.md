# load surface data and link to [`SurfaceGeometry`](SurfaceGeometry-class.md)

load surface data and link to
[`SurfaceGeometry`](SurfaceGeometry-class.md)

## Usage

``` r
read_surf_data(geometry, surface_data_name, colind = NULL, nodeind = NULL)
```

## Arguments

- geometry:

  a [`SurfaceGeometry`](SurfaceGeometry-class.md) instance

- surface_data_name:

  the name of the file containing the values to be mapped to the
  surface.

- colind:

  the subset column indices of surface dataset to load (optional)

- nodeind:

  the subset node indices of surface dataset to include (optional)

## Value

an instance of the class [`NeuroSurface`](NeuroSurface-class.md) or
[`NeuroSurfaceVector`](NeuroSurfaceVector-class.md)
