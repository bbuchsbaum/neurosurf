# Convert Curvature Values to Smooth Gradient Colors

Maps surface curvature values to a continuous grayscale gradient,
producing a FreeSurfer-style sulcal shading that is visually smoother
than the binary mapping of [`curv_cols`](curv_cols.md).

## Usage

``` r
curv_cols_smooth(
  vals,
  light = "#D4D4D4",
  dark = "#3A3A3A",
  quantiles = c(0.05, 0.95)
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

## Value

A character vector of hex color codes the same length as `vals`.

## Details

Values are clamped to the range defined by `quantiles` to prevent
outliers from washing out the colour map. The clamped values are
linearly interpolated between `dark` (most negative / sulcal) and
`light` (most positive / gyral).

## See also

[`curv_cols`](curv_cols.md), [`curvature`](curvature-methods.md),
[`view_surface`](view_surface.md)

## Examples

``` r
set.seed(1)
curv <- rnorm(500, sd = 0.1)
cols <- curv_cols_smooth(curv)
head(cols)
#> [1] "#686868" "#8E8E8E" "#5E5E5E" "#D0D0D0" "#959595" "#5F5F5F"
```
