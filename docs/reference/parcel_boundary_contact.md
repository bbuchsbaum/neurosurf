# Parcel boundary contact matrix

Parcel boundary contact matrix

## Usage

``` r
parcel_boundary_contact(
  labeled_surface,
  component_policy = c("error", "largest", "each", "merge"),
  counts = FALSE
)
```

## Arguments

- labeled_surface:

  A `LabeledNeuroSurface`.

- component_policy:

  Fragment handling policy.

- counts:

  If TRUE, return edge counts; otherwise logical contact matrix.

## Value

Matrix (logical or integer) indicating parcel-unit boundary contacts.

## Examples

``` r
# \donttest{
# Requires a LabeledNeuroSurface
# lsurf <- read_freesurfer_annot("lh.aparc.annot", geom)
# contact <- parcel_boundary_contact(lsurf)
# }
```
