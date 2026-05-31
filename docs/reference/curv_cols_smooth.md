# Convert Curvature Values to Smooth Gradient Colors

Maps surface curvature values to a continuous grayscale gradient,
producing a FreeSurfer-style sulcal shading that is visually smoother
than the binary mapping of [`curv_cols`](curv_cols.md).

## Usage

``` r
curv_cols_smooth(
  vals,
  light = "#C8C8C8",
  dark = "#4D4D4D",
  quantiles = c(0.05, 0.95),
  sharpness = 6
)
```

## Arguments

- vals:

  A numeric vector of curvature values for each vertex.

- light:

  Hex color for the lightest shade (gyral crowns). Default `"#D4D4D4"`.

- dark:

  Hex color for the darkest shade (sulcal fundi). Default `"#3A3A3A"`.

- quantiles:

  Length-2 numeric vector of lower and upper quantiles used to clamp
  extreme values before rescaling. Default `c(0.05, 0.95)`.

- sharpness:

  Non-negative number controlling how crisp the gyral/sulcal split is.
  Larger values push vertices toward the `light`/`dark` extremes (a more
  binary, FreeSurfer-like contrast); smaller values give a softer
  gradient. `0` reproduces a plain linear ramp. Default `6`.

## Value

A character vector of hex color codes the same length as `vals`.

## Details

Values are clamped to the range defined by `quantiles` to prevent
outliers from washing out the colour map. The clamped values are then
centred on their median (the gyral/sulcal boundary) and passed through a
logistic contrast curve governed by `sharpness` before being
interpolated between `dark` (sulcal fundi) and `light` (gyral crowns).

A plain linear ramp leaves most vertices near mid-grey, so the fold
pattern tends to disappear once surface lighting is applied. Pushing
values toward the two extremes with a logistic curve keeps the sulci
clearly dark and the gyri clearly light, which reads as anatomical
folding under lighting much the way FreeSurfer / Connectome Workbench
curvature overlays do.

## See also

[`curv_cols`](curv_cols.md), [`curvature`](curvature-methods.md),
[`view_surface`](view_surface.md)

## Examples

``` r
set.seed(1)
curv <- rnorm(500, sd = 0.1)
cols <- curv_cols_smooth(curv)
head(cols)
#> [1] "#5B5B5B" "#A1A1A1" "#545454" "#C8C8C8" "#ADADAD" "#545454"

# Softer, more continuous gradient
cols_soft <- curv_cols_smooth(curv, sharpness = 2)
```
