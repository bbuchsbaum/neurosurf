# VertexData

A set of arbitary vertices associated with a data table

## Details

The VertexData class provides a flexible way to associate arbitrary data
with specific vertices in a brain surface. Unlike ROISurface and similar
classes, VertexData does not require or maintain a reference to surface
geometry, making it lighter and more versatile for storing and
manipulating vertex-specific information.

This class is particularly useful for:

- Storing heterogeneous data (different data types) associated with
  vertices

- Maintaining analysis results like statistical outcomes for specific
  vertices

- Tracking vertex-specific annotations or classifications

- Creating lookup tables for vertex properties that can be joined with
  other data

The `data` slot contains a data frame where each row corresponds to a
vertex in the same order as the `indices` slot. This structure allows
storing multiple attributes of different types (numeric, character,
logical) for each vertex, and facilitates standard data frame operations
like filtering, sorting, and joining.

## Slots

- `indices`:

  the node indices

- `data`:

  the associated table with `nrow(data) == length(indices)`

## Examples

``` r
# \donttest{
# Create a set of vertex indices
vertex_indices <- c(10L, 25L, 50L, 100L, 200L)

# Create a data frame with information for each vertex
# Each row corresponds to one vertex in the same order as indices
vertex_data <- data.frame(
  value = c(0.5, 1.2, 0.8, 1.5, 0.3),
  label = c("A", "B", "A", "C", "B"),
  significant = c(TRUE, FALSE, TRUE, TRUE, FALSE)
)

# Create the VertexData object
vd <- new("VertexData",
         indices = vertex_indices,
         data = vertex_data)

# Access the data
# vd@data$value
# vd@data$label[vd@data$significant]
# vd@indices[vd@data$label == "A"]
# }
```
