# Add a vector field overlay

Add a vector field overlay

## Usage

``` r
add_vector_layer(
  x,
  vectors,
  vertices = NULL,
  scale = NULL,
  color = "red",
  alpha = 0.8,
  lwd = 1.5,
  hemi = c("both", "left", "right")
)
```

## Arguments

- x:

  A `"neurosurf_plot"` object.

- vectors:

  Matrix (n x 3) of XYZ vectors or a list with `left`/`right` matrices.
  When supplying a single matrix for both hemispheres, rows are split
  left-to-right to match the vertex ordering.

- vertices:

  Optional vector or list of vertex ids matching the rows of `vectors`
  when defined on a subset of vertices. For both hemispheres, supply a
  list to avoid ambiguity.

- scale:

  Optional numeric scalar. If `NULL`, a heuristic scale is derived from
  the mesh extent and vector magnitudes.

- color:

  Colour for the vectors (single value or vector).

- alpha:

  Numeric in \\\[0,1\]\\ for vector opacity.

- lwd:

  Numeric line width for the glyphs.

- hemi:

  One of `"both"`, `"left"`, or `"right"` when a single `vectors` matrix
  is provided.

## Value

A modified `"neurosurf_plot"` object.

## Examples

``` r
# \donttest{
# Requires FreeSurfer-like surface files
# sp <- surface_plot(left = "lh.pial", right = "rh.pial")
# vectors <- matrix(rnorm(163842 * 3), ncol = 3)
# sp <- add_vector_layer(sp, vectors = vectors)
# }
```
