# Read Freesurfer Annotation File

Reads a Freesurfer annotation file and creates a LabeledNeuroSurface
object.

## Usage

``` r
read_freesurfer_annot(file_name, geometry)
```

## Arguments

- file_name:

  Character string; path to the '.annot' file

- geometry:

  A SurfaceGeometry object representing the surface structure

## Value

A LabeledNeuroSurface object containing:

- indices:

  Integer vector of vertex indices

- data:

  Numeric vector of label codes

- labels:

  Character vector of label names

- cols:

  Character vector of label colors in hex format

## Details

This function reads binary data from a FreeSurfer annotation file, which
includes vertex labels, color information, and label names. It then
constructs a LabeledNeuroSurface object using this information along
with the provided surface geometry.

## Examples

``` r
# \donttest{
geom <- readSurfaceGeometry("path/to/surface.gii")
#> Error in readSurfaceGeometry("path/to/surface.gii"): could not find function "readSurfaceGeometry"
labeled_surface <- read_freesurfer_annot("path/to/labels.annot", geom)
#> Warning: cannot open file 'path/to/labels.annot': No such file or directory
#> Error in file(file_name, "rb"): cannot open the connection
# }
```
