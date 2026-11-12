# Write a surface figure to PNG

Draws a
[`surface_figure()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_figure.md)
on a PNG device sized to its panels. Uses the `ragg` device when
available, otherwise
[`grDevices::png()`](https://rdrr.io/r/grDevices/png.html).

## Usage

``` r
write_surface_figure(x, file, scale = 1)
```

## Arguments

- x:

  A `surface_figure` object.

- file:

  Output PNG path.

- scale:

  Multiplier applied to the figure's pixel dimensions.

## Value

The normalized path, invisibly.

## See also

[`surface_figure()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_figure.md),
[`write_surface_rgba()`](https://bbuchsbaum.github.io/neurosurf/reference/write_surface_rgba.md)
for single panels.
