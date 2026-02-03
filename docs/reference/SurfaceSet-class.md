# SurfaceSet: bundle multiple surface variants for one hemisphere

A \`SurfaceSet\` keeps several \`SurfaceGeometry\` variants (e.g., pial,
white, inflated, sphere) for the same hemisphere together with a default
choice.

## Usage

``` r
# S4 method for class 'SurfaceSet'
geometry(x)

# S4 method for class 'SurfaceSet'
vertices(x, ...)

# S4 method for class 'SurfaceSet'
faces(x, ...)

# S4 method for class 'SurfaceSet'
nodes(x)

# S4 method for class 'SurfaceSet'
graph(x, ...)

# S4 method for class 'SurfaceSet'
curvature(x, ...)
```

## Arguments

- x:

  a \`SurfaceSet\`

- ...:

  additional arguments forwarded to the underlying geometry method

## Value

An S4 object of class `SurfaceSet`. Use [`surface_set`](surface_set.md)
to construct instances.

## Slots

- `hemi`:

  Character string for hemisphere (e.g., "lh" or "rh").

- `surfaces`:

  Named list of \`SurfaceGeometry\` objects keyed by their label.

- `default_label`:

  Character string giving the label to use by default.
