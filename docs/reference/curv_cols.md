# Convert Curvature Values to Binary Colors for Visualization

This function maps a vector of surface curvature values (e.g., mean
curvature) to a binary color scheme, typically used to distinguish gyri
(outward folds) from sulci (inward folds) on a brain surface
visualization.

## Usage

``` r
curv_cols(vals, incol = "#B3B3B3", outcol = "#404040")
```

## Arguments

- vals:

  A numeric vector containing curvature values for each vertex on the
  surface.

- incol:

  A character string specifying the hex color code to represent vertices
  with curvature values \*greater than\* the median curvature. Default
  is "#B3B3B3" (light gray).

- outcol:

  A character string specifying the hex color code to represent vertices
  with curvature values \*less than or equal to\* the median curvature.
  Default is "#404040" (dark gray).

## Value

A character vector of the same length as \`vals\`, containing hex color
codes based on the binary classification of curvature values relative to
the median.

## Details

Surface curvature provides information about the local shape of the
surface. Mean curvature is often used, where positive values typically
indicate outward curvature (gyri) and negative values indicate inward
curvature (sulci). This function simplifies the curvature map into two
colors based on whether the value is above or below the median
curvature, providing a quick visual distinction between these features.
Note the default coloring assigns \`incol\` to values \*above\* the
median and \`outcol\` to values \*at or below\* the median. You might
need to adjust \`incol\` and \`outcol\` depending on the specific
interpretation of curvature values in your data (e.g., if positive
values represent sulci).

## See also

[`curvature`](curvature-methods.md), [`view_surface`](view_surface.md)

## Examples

``` r
# Generate some example curvature values
set.seed(123)
curvature_values <- rnorm(100, mean = 0, sd = 0.1)

# Get binary colors using default light/dark gray
gray_colors <- curv_cols(curvature_values)
table(gray_colors)
#> gray_colors
#> #404040 #B3B3B3 
#>      50      50 

# Use different colors (e.g., red for above median, blue for below)
red_blue_colors <- curv_cols(curvature_values, incol = "#FF0000", outcol = "#0000FF")
table(red_blue_colors)
#> red_blue_colors
#> #0000FF #FF0000 
#>      50      50 
```
