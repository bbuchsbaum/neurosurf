# Displaying Surfaces with RGL

This vignette demonstrates how to display 3D brain surface meshes using
the `rgl` plotting tools provided by the `neurosurf` package, primarily
through the [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
method which utilizes the
[`view_surface()`](https://bbuchsbaum.github.io/neurosurf/reference/view_surface.md)
function internally.

For interactive HTML widgets, see
[`vignette("interactive-surfaces")`](https://bbuchsbaum.github.io/neurosurf/articles/interactive-surfaces.md).
For high-level, multi-view layouts with colourbars and atlas outlines,
see
[`vignette("surface-figures")`](https://bbuchsbaum.github.io/neurosurf/articles/surface-figures.md).

## Setup and Loading Data

First, we set up `knitr` options to embed `rgl` plots directly into the
HTML output using WebGL and prevent standalone `rgl` windows from
popping up during knitting. We then load example left and right
hemisphere white matter surfaces included with the package and prepare
some data (smoothed geometry, curvature, random values) for the
examples.

## Basic Surface Plotting

The simplest way to display a `SurfaceGeometry` object is using the
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method. By
default, it renders the surface with a light gray background. We can
specify a `viewpoint`.

``` r

# Plot the smoothed left hemisphere from a lateral viewpoint
render_surface(white_lh_display, viewpoint = "lateral", lit = TRUE)
```

![](displaying-surfaces_files/figure-html/basic-plot-1.-render-1.png)

## Coloring Based on Curvature

Surface curvature helps distinguish gyri (outward folds) from sulci
(inward folds). The
[`curvature()`](https://bbuchsbaum.github.io/neurosurf/reference/curvature-methods.md)
function calculates this, and
[`curv_cols_smooth()`](https://bbuchsbaum.github.io/neurosurf/reference/curv_cols_smooth.md)
maps the values to a continuous grayscale gradient (dark in sulci, light
on gyri) for natural-looking shading. For a simpler binary split, see
[`curv_cols()`](https://bbuchsbaum.github.io/neurosurf/reference/curv_cols.md).
Either way, pass the resulting colors to the `bgcol` argument of
[`plot()`](https://rdrr.io/r/graphics/plot.default.html).

``` r

# Calculate curvature colors
curv_colors <- curv_cols_smooth(curv_lh_display)

# Plot with curvature background from a medial viewpoint
render_surface(white_lh_display, bgcol = curv_colors, viewpoint = "medial", specular = "black")
```

![](displaying-surfaces_files/figure-html/curvature-plot-1.-render-2.png)

## Overlaying Data Values

Often, we want to visualize data mapped onto the surface vertices (e.g.,
activation values, thickness). We can pass a vector of values to the
`vals` argument. The `cmap` argument specifies the color map, and
`irange` defines the data range to map onto the colormap. Values outside
`irange` are clamped to the minimum or maximum color.

``` r

# Overlay random data using a rainbow colormap
# Map data range from -2 to 2 onto the colormap
render_surface(white_lh_display, vals = random_vals_display_smooth, cmap = rainbow(256),
               irange = c(-2, 2), thresh = NULL, viewpoint = "lateral", specular = "gray")
```

![](displaying-surfaces_files/figure-html/data-overlay-1.-render-3.png)

## Thresholding Data Visualization

The `thresh` argument (a vector of two values, `c(lower, upper)`) can be
used with `vals` to make parts of the surface transparent. Vertices
where the corresponding value in `vals` is *inside* this range (between
`lower` and `upper`) are rendered transparently; values outside remain
opaque. This is useful for masking out a band of values.

``` r

# Same data overlay as above, but make values between -1 and 1 transparent
render_surface(white_lh_display, vals = random_vals_display_smooth, cmap = rainbow(256),
               irange = c(-2, 2), thresh = c(-1, 1), viewpoint = "lateral", lit = TRUE)
```

![](displaying-surfaces_files/figure-html/threshold-plot-1.-render-4.png)

## Direct Vertex Coloring

Instead of mapping data values to a colormap, you can provide a vector
of specific hex color codes directly to the `vert_clrs` argument. This
overrides `vals` and `cmap`. The vector length must match the number of
vertices.

``` r

# Color vertices based on their x-coordinate (e.g., red for positive x, blue for negative)
x_coords <- coords(white_lh_display)[, 1]
vertex_colors <- ifelse(x_coords > median(x_coords), "#FF0000", "#0000FF") # Red/Blue

render_surface(white_lh_display, vert_clrs = vertex_colors, viewpoint = "ventral", lit = TRUE)
```

![](displaying-surfaces_files/figure-html/vertex-color-plot-1.-render-5.png)

## Controlling Transparency

The `alpha` argument controls the overall transparency of the surface,
ranging from 0 (fully transparent) to 1 (fully opaque).

``` r

# Plot the surface with 60% opacity (40% transparent)
render_surface(white_lh_display, vals = random_vals_display_smooth, cmap = heat.colors(256),
               irange = c(-2, 2), alpha = 0.6, viewpoint = "posterior")
```

![](displaying-surfaces_files/figure-html/alpha-plot-1.-render-6.png)

## Adjusting Lighting and Material

The appearance of the surface is affected by lighting. The `specular`
argument controls the color of specular highlights (shininess). Setting
it to `"black"` creates a matte appearance.

``` r

# Plot with a matte finish (no specular highlights)
render_surface(white_lh_display, vals = random_vals_display_smooth, cmap = topo.colors(256),
               irange = c(-2, 2), specular = "black", viewpoint = "lateral", lit = TRUE)
```

![](displaying-surfaces_files/figure-html/lighting-plot-1.-render-7.png)

## Snapshotting to an image (for knitr/CI)

Use
[`snapshot_surface()`](https://bbuchsbaum.github.io/neurosurf/reference/snapshot_surface.md)
to render an off-screen PNG and include it directly:

``` r

.render_counter$n <- .render_counter$n + 1
snapshot_file <- knitr::fig_path(paste0("-snapshot-", .render_counter$n, ".png"))
dir.create(dirname(snapshot_file), recursive = TRUE, showWarnings = FALSE)

img_path <- try(snapshot_surface(white_lh_display,
                                 file = snapshot_file,
                                 vals = random_vals_display_smooth,
                                 cmap = viridis::viridis(256),
                                 viewpoint = "lateral",
                                 specular = "black",
                                 width = 1200, height = 900),
                silent = TRUE)

if (!inherits(img_path, "try-error") && snapshot_is_usable(img_path)) {
  knitr::include_graphics(img_path)
} else {
  rgl::open3d()
  view_surface(white_lh_display,
               vals = random_vals_display_smooth,
               cmap = viridis::viridis(256),
               viewpoint = "lateral",
               specular = "black",
               new_window = FALSE)
  widget <- rgl::rglwidget()
  rgl::close3d()
  widget
}
```

![](displaying-surfaces_files/figure-html/snapshot-example-1.-snapshot-8.png)

## Changing Viewpoints

The `viewpoint` argument can be set to common anatomical views like
`"lateral"`, `"medial"`, `"ventral"`, or `"posterior"`. The function
automatically selects the correct left/right version based on the
surface’s hemisphere information (`surf@hemi`).

``` r

# Display multiple viewpoints with curvature shading
render_multi_view(white_lh_display,
                  viewpoints = c("lateral", "medial", "ventral", "posterior"),
                  bgcol = curv_cols_smooth(curv_lh_display), specular = "black")
```

![](displaying-surfaces_files/figure-html/viewpoints-plot-1.-multiview-9.png)![](displaying-surfaces_files/figure-html/viewpoints-plot-1.-multiview-10.png)![](displaying-surfaces_files/figure-html/viewpoints-plot-1.-multiview-11.png)![](displaying-surfaces_files/figure-html/viewpoints-plot-1.-multiview-12.png)

## Displaying Two Hemispheres

For lateral views, each hemisphere is best rendered separately since the
camera can only face one direction. We render the left and right lateral
views side by side.

``` r

# Render both hemispheres as a single figure so they always appear together
# (two side-by-side images, or one combined widget when snapshots are
# unavailable). Leave some extra margin so the static PNGs do not feel cramped.
render_hemi_pair(
  white_lh_display,
  white_rh_display,
  bgcol_lh = curv_cols_smooth(curv_lh_display, quantiles = c(0.02, 0.98)),
  bgcol_rh = curv_cols_smooth(curv_rh_display, quantiles = c(0.02, 0.98)),
  viewpoint = "lateral",
  specular = "black",
  zoom = 0.92,
  width = 900,
  height = 700
)
```

![](displaying-surfaces_files/figure-html/two-hemispheres-plot-1.-hemipair-13.png)![](displaying-surfaces_files/figure-html/two-hemispheres-plot-1.-hemipair-14.png)

## Adding Spheres to the Surface

The `spheres` argument allows you to draw spherical markers at specified
coordinates. It requires a data frame with columns `x`, `y`, `z`, and
`radius`. An optional `color` column can specify colors for each sphere.

``` r

# Define coordinates for some spherical markers
# Sample some vertex indices safely from available vertices
n_vertices <- nrow(coords(white_lh_display))
sample_indices <- sample(1:n_vertices, size = min(3, n_vertices))

peak_coords <- data.frame(
  x = coords(white_lh_display)[sample_indices, 1],
  y = coords(white_lh_display)[sample_indices, 2],
  z = coords(white_lh_display)[sample_indices, 3],
  radius = c(3, 4, 2.5)[1:length(sample_indices)],
  color = c("yellow", "cyan", "magenta")[1:length(sample_indices)]
)

# Plot the surface with curvature shading and add the spheres
render_surface(white_lh_display, bgcol = curv_cols_smooth(curv_lh_display),
               viewpoint = "lateral", specular = "black", spheres = peak_coords)
```

![](displaying-surfaces_files/figure-html/spheres-plot-1.-render-15.png)

## Plotting Other NeuroSurface Objects

The [`plot()`](https://rdrr.io/r/graphics/plot.default.html) method also
works for other classes like `NeuroSurface`, `LabeledNeuroSurface`, and
`ColorMappedNeuroSurface`. These objects already contain data and
potentially color mapping information. The `plot` method extracts this
information and passes the appropriate arguments (like `vals`, `cmap`,
`irange`, `thresh`, `vert_clrs`) to the underlying `view_surface`
function.

``` r

# Create a NeuroSurface object with the random data
nsurf <- NeuroSurface(white_lh_display, indices = 1:length(random_vals_display), data = random_vals_display)

# Plot the NeuroSurface - uses data stored within the object
# We can still override or add parameters like cmap, irange, thresh, alpha etc.
render_surface(geometry(nsurf), vals = values(nsurf), cmap = heat.colors(128),
               irange = c(-2.5, 2.5), viewpoint = "lateral")
```

![](displaying-surfaces_files/figure-html/neurosurface-plot-1.-render-16.png)

## Showing an activation map overlaid on a surface mesh

We will plot surface in a row of 3. We generate a set of random values
and then smooth those values along the surface to approximate a
realistic activation pattern.

In the first column we display all the values in the map. Next we make
values between (-0.2, 0.2) transparent. In the last panel we
additionally add a cluster size threshold of 30 nodes.

[`surface_montage()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_montage.md)
handles the per-panel rendering and layout for us: it captures each
panel as a static image and tiles them into one figure (falling back to
a single interactive widget when static snapshots are unavailable). Each
panel is either a surface or a `list(surface, ...overrides)`, and
arguments shared by every panel (here `cmap` and `irange`) are passed
once.

``` r

vals <- rnorm(length(nodes(white_lh_base)))
ssurf <- smooth(NeuroSurface(white_lh_base, indices = seq_along(vals), data = vals))
csurf <- cluster_threshold(ssurf, size = 30, threshold = c(-0.2, 0.2))

surface_montage(
  list(
    ssurf,                                # all values
    list(ssurf, thresh = c(-0.2, 0.2)),   # band around zero made transparent
    list(csurf, thresh = c(-0.2, 0.2))    # + cluster-size threshold (>= 30 nodes)
  ),
  cmap = rainbow(100), irange = c(-2, 2), ncol = 3
)
```

![](displaying-surfaces_files/figure-html/activation-map-1.-montage-1.png)

## Showing two hemispheres in same scene

For views where the left-right axis maps to the screen (posterior,
anterior, dorsal), both hemispheres can share a single scene since their
coordinates naturally separate (LH at x \< 0, RH at x \> 0).

``` r

# Two hemispheres shown from posterior viewpoint
.render_counter$n <- .render_counter$n + 1
posterior_file <- knitr::fig_path(paste0("-posterior-", .render_counter$n, ".png"))
dir.create(dirname(posterior_file), recursive = TRUE, showWarnings = FALSE)

img_path <- try({
  file <- posterior_file
  rgl::open3d()
  rgl::par3d(windowRect = c(0, 0, 1200, 600))
  rgl::bg3d(color = "white")

  # LH and RH sit naturally at x<0 and x>0; small offset adds breathing room
  view_surface(white_lh_display, bgcol = curv_cols_smooth(curv_lh_display),
               viewpoint = "posterior", new_window = FALSE, offset = c(-5, 0, 0))
  view_surface(white_rh_display, bgcol = curv_cols_smooth(curv_rh_display),
               viewpoint = "posterior", new_window = FALSE, offset = c(5, 0, 0))
  rgl::view3d(fov = 0, zoom = 0.55,
              userMatrix = rbind(c(1,0,0,0), c(0,0,1,0), c(0,-1,0,0), c(0,0,0,1)))
  snapshot_current_scene(file)
}, silent = TRUE)
try(rgl::close3d(), silent = TRUE)

if (!inherits(img_path, "try-error") && snapshot_is_usable(img_path)) {
  knitr::include_graphics(img_path)
} else {
  # Fallback to rglwidget
  rgl::open3d()
  view_surface(white_lh_display, bgcol = curv_cols_smooth(curv_lh_display),
               viewpoint = "posterior", new_window = FALSE, offset = c(-5, 0, 0))
  view_surface(white_rh_display, bgcol = curv_cols_smooth(curv_rh_display),
               viewpoint = "posterior", new_window = FALSE, offset = c(5, 0, 0))
  rgl::view3d(fov = 0, zoom = 0.55,
              userMatrix = rbind(c(1,0,0,0), c(0,0,1,0), c(0,-1,0,0), c(0,0,0,1)))
  rgl::rglwidget()
}
```

![](displaying-surfaces_files/figure-html/two-hemi-posterior-1.-posterior-17.png)

## Next Steps

For **interactive 3D visualization** with
[`surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md),
see
[`vignette("interactive-surfaces")`](https://bbuchsbaum.github.io/neurosurf/articles/interactive-surfaces.md).

For **publication-quality multi-view figures**, see
[`vignette("surface-figures")`](https://bbuchsbaum.github.io/neurosurf/articles/surface-figures.md).
