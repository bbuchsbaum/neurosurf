# Write Surface Data to File

This function writes surface data from a NeuroSurface or
NeuroSurfaceVector object to a .1D.dset file.

## Usage

``` r
write_surf_data(surf, outstem, hemi = "")
```

## Arguments

- surf:

  An object of class `NeuroSurface` or `NeuroSurfaceVector` containing
  the surface data to be written.

- outstem:

  A character string specifying the base name for the output file
  (without extension).

- hemi:

  A character string specifying the hemisphere ("lh" for left, "rh" for
  right). Default is an empty string.

## Value

This function does not return a value. It writes the data to a .1D.dset
file as a side effect.

## Details

The function writes the surface data to a .1D.dset file, which is a
tabular data format. The output file contains node indices in the first
column, followed by data values in subsequent columns. The file name is
constructed by combining `outstem`, `hemi` (if provided), and the
extension ".1D.dset". For NeuroSurfaceVector objects, all columns of
data are written. For NeuroSurface objects, only the single data vector
is written. The data is written without row names, column names, or
quotes.

## Examples

``` r
# \donttest{
sg <- example_surface_geometry()
nv <- nrow(vertices(sg))
ns <- NeuroSurface(geometry = sg, indices = seq_len(nv), data = rnorm(nv))
write_surf_data(ns, file.path(tempdir(), "output_data"), "lh")
# }
```
