# load surface data and link to [`SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry-class.md)

load surface data and link to
[`SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry-class.md)

## Usage

``` r
read_surf_data(geometry, surface_data_name, colind = NULL, nodeind = NULL)
```

## Arguments

- geometry:

  a
  [`SurfaceGeometry`](https://bbuchsbaum.github.io/neurosurf/reference/SurfaceGeometry-class.md)
  instance

- surface_data_name:

  the name of the file containing the values to be mapped to the
  surface.

- colind:

  the subset column indices of surface dataset to load (optional)

- nodeind:

  the subset node indices of surface dataset to include (optional)

## Value

an instance of the class
[`NeuroSurface`](https://bbuchsbaum.github.io/neurosurf/reference/NeuroSurface-class.md)
or
[`NeuroSurfaceVector`](https://bbuchsbaum.github.io/neurosurf/reference/NeuroSurfaceVector-class.md)

## Examples

``` r
# \donttest{
# Load geometry and surface data file
# geom <- read_surf_geometry("lh.white")
# surf_data <- read_surf_data(geom, "lh.thickness")
# }
```
