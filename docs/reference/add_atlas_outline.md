# Add an atlas outline layer to a surface plot

This helper adds an outline-only layer to an existing `"neurosurf_plot"`
object using ROI labels. It configures sensible defaults for boundary
aesthetics (including a light halo and a small depth offset) so that
atlas outlines remain legible over filled statistical maps.

## Usage

``` r
add_atlas_outline(
  x,
  labels,
  rois = NULL,
  label = "atlas",
  outline_col = "black",
  outline_lwd = 1.5,
  outline_offset = 0.5,
  outline_halo = TRUE,
  outline_halo_col = "white",
  outline_halo_lwd = NULL,
  outline_lty = c("solid", "dashed"),
  ...
)
```

## Arguments

- x:

  A `"neurosurf_plot"` object created by
  [`surface_plot`](surface_plot.md).

- labels:

  Numeric vector or list of vectors containing ROI labels for each
  vertex. If a single vector is supplied, it is split across hemispheres
  based on vertex counts.

- rois:

  Optional numeric vector of ROI ids to outline. If `NULL`, all ROIs
  present in `labels` are outlined.

- label:

  Optional character label for this outline layer.

- outline_col:

  Colour to use for ROI boundaries. Defaults to `"black"`.

- outline_lwd:

  Numeric line width for boundaries. Defaults to `1.5`.

- outline_offset:

  Numeric depth offset along surface normals to avoid z-fighting.
  Defaults to `0.5`.

- outline_halo:

  Logical; if `TRUE`, draws a thicker halo under the main line for
  better visibility. Defaults to `TRUE`.

- outline_halo_col:

  Colour for the halo. Defaults to `"white"`.

- outline_halo_lwd:

  Numeric line width for the halo. If `NULL`, slightly larger than
  `outline_lwd`.

- outline_lty:

  Line type for boundaries: `"solid"` or `"dashed"`.

- ...:

  Additional arguments passed through to
  [`add_surface_layer`](add_surface_layer.md), allowing fine control
  over line colour, width, offset, and halo appearance.

## Value

A modified `"neurosurf_plot"` object.

## Examples

``` r
# \donttest{
fs <- load_fsaverage_std8("inflated")
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/Rtmp0Nbniq/temp_libpath80e31f94425b/neurosurf/extdata/std.8_lh.inflated.asc
#> loading /private/var/folders/9h/nkjq6vss7mqdl4ck7q1hd8ph0000gp/T/Rtmp0Nbniq/temp_libpath80e31f94425b/neurosurf/extdata/std.8_rh.inflated.asc
p  <- surface_plot(fs$lh, fs$rh)
# roi_labels <- ... # per-vertex ROI ids
# p <- add_atlas_outline(p, roi_labels)
# }
```
