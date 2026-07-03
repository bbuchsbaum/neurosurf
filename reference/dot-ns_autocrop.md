# Auto-crop a raster image (removes uniform background)

Auto-crop a raster image (removes uniform background)

## Usage

``` r
.ns_autocrop(img, border = 0, bg = NULL, fuzz = 0.02, margin = NULL)
```

## Arguments

- img:

  A raster image array

- border:

  Number of pixels to preserve as border. Ignored when `margin` is
  supplied.

- bg:

  Optional background colour. When given (and the image has no alpha
  channel), pixels within `fuzz` of this colour are treated as
  background and cropped away, so any solid background works, not just
  white. When `NULL`, near-white is treated as background.

- fuzz:

  Numeric tolerance in \\\[0,1\]\\ for matching the background colour
  per RGB channel.

- margin:

  Optional fraction of the content extent to retain as a proportional
  border. Overrides `border` when supplied.

## Value

The cropped image array with background removed
