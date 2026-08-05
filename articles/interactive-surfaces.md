# Interactive Surface Visualization with surfwidget

This vignette focuses on interactive 3D visualization using
[`surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md).
For RGL-based 3D plots, see
[`vignette("displaying-surfaces")`](https://bbuchsbaum.github.io/neurosurf/articles/displaying-surfaces.md).
For multi-view, publication-quality figures with colourbars and atlas
outlines, see
[`vignette("surface-figures")`](https://bbuchsbaum.github.io/neurosurf/articles/surface-figures.md).

## Introduction

The
[`surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
function provides interactive 3D visualization of brain surfaces using
HTML widgets. This creates web-based viewers with real-time rotation,
zooming, and interactive exploration capabilities.

## Quick Start

Create a small widget to confirm things are working, then jump to richer
examples below.

``` r

surfwidget(
  white_lh_smooth,
  width  = "100%",
  height = "420px",
  curvature = curv_vals,
  config = list(
    ambientLightColor = "#404040",
    initialZoom = 2,
    showControls = FALSE   # keep the panel hidden for the quick preview
  )
)
```

Interactive left-hemisphere surface; map: data.

## Basic Surface Display

The simplest usage is to pass a `SurfaceGeometry` object directly:

``` r

# Create basic interactive surface
basic_widget <- surfwidget(
  white_lh_smooth,
  width  = "100%",
  height = "400px",
  curvature = curv_vals,
  config = list(
    ambientLightColor = "#404040",
    initialZoom = 2
  )
)
basic_widget
```

Interactive left-hemisphere surface; map: data.

## Data Visualization

Display data mapped onto surface vertices using a `NeuroSurface` object:

``` r

# Create NeuroSurface with data
nsurf <- NeuroSurface(white_lh_smooth, indices=1:length(random_vals), data=random_vals)

# Create widget with data overlay
data_widget <- surfwidget(nsurf, 
                         cmap = heat.colors(256), 
                         irange = c(-2, 2),
                         width = "100%", 
                         height = "400px",
                         curvature = curv_vals,
                         config = list(
                           ambientLightColor = "#404040",
                           initialZoom = 2
                         ))

data_widget
```

Interactive left-hemisphere surface; map: data.

## Advanced Configuration

Fine-tune the appearance with custom configuration:

``` r

# Create ColorMappedNeuroSurface with threshold
color_mapped_surf <- ColorMappedNeuroSurface(
  white_lh_smooth, 
  indices = 1:length(random_vals), 
  data = random_vals,
  cmap = rainbow(256),
  irange = c(-2.5, 2.5),
  thresh = c(-1, 1)
)

# Advanced configuration
advanced_config <- list(
  shininess = 60,
  specularColor = "#ffffff",
  ambientLightColor = "#404040",
  directionalLightColor = "#ffffff",
  directionalLightIntensity = 0.7,
  flatShading = FALSE
)

# Curvature is the anatomical underlay; the ColorMappedNeuroSurface supplies
# the selectable data layer without copying it into a legacy layer list.
advanced_widget <- surfwidget(color_mapped_surf,
                             curvature = curv_vals,
                             config = c(advanced_config, list(initialZoom = 2)), 
                             alpha = 0.9,
                             width = "100%", 
                             height = "450px")

advanced_widget
```

Interactive left-hemisphere surface; map: data.

## Vertex-Colored Surfaces

Create surfaces with direct vertex coloring:

``` r

# Create vertex colors based on coordinates
x_coords <- coords(white_lh_smooth)[, 1]
vertex_colors <- ifelse(x_coords > median(x_coords), "#FF6B6B", "#4ECDC4")

# Create VertexColoredNeuroSurface
vertex_surf <- VertexColoredNeuroSurface(
  geometry = white_lh_smooth, 
  indices = 1:length(vertex_colors), 
  colors = vertex_colors
)

# Custom material settings
material_config <- list(
  shininess = 20,
  specularColor = "#333333",
  ambientLightColor = "#404040"
)

vertex_widget <- surfwidget(vertex_surf, 
                           curvature = curv_vals,
                           config = c(material_config, list(initialZoom = 2)), 
                           alpha = 0.8,
                           width = "100%", 
                           height = "400px")

vertex_widget
```

Interactive left-hemisphere surface; map: data.

## Interactive Controls

Report widgets include a deliberately small set of native controls:

- **Rotation**: Click and drag to rotate the surface
- **Zoom**: Mouse wheel or pinch gestures to zoom in/out
- **Pan**: Right-click and drag to pan the view
- **Map**: Select one of the named layers authored in the scene
- **Viewpoint**: Switch among anatomical views and reset the camera
- **Fullscreen**: Expand the report figure when the browser permits it
- **PNG**: Export the current view using the active figure preset

`paper-light` names the default visual preset. It combines a light
background, lighting, material, and export defaults suited to papers and
reports. It does not enable controls. The independent `SurfaceScene`
`mode` controls behavior: `mode = "report"` adds the curated toolbar,
while `mode = "viewer"` leaves a bare rotatable viewer.

Tweakpane is deprecated for
[`surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
reports. Existing `config = list(showControls = ...)` and `controlType`
calls warn during the migration; no Tweakpane code or CDN dependency is
loaded.

For a portable, explicitly authored report, construct a scene with
required fallback and alternative text:

``` r

scene <- surface_scene(
  left = white_lh_smooth,
  layers = surface_layer("effect", random_vals, limits = c(-2, 2)),
  fallback = "Left cortical surface showing the effect map from -2 to 2.",
  alt_text = "Lateral view of the left cortex colored by effect size.",
  preset = "paper-light",
  mode = "report"
)

surfwidget(scene)
write_surface_scene(scene, "surface-report", self_contained = FALSE)
```

## Troubleshooting

**Testing in Interactive R:**

All
[`surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
examples should work perfectly when run in an interactive R console:

``` r

# This should work in interactive R:
library(neurosurf)
white_lh_asc <- system.file("extdata", "std.8_lh.smoothwm.asc", package="neurosurf")
white_lh <- read_surf(white_lh_asc)
white_lh_smooth <- smooth(white_lh, type="HCLaplace", delta=.2, iteration=5)

# Basic widget
surfwidget(white_lh_smooth, width = "100%", height = "400px")

# With data
random_vals <- rnorm(length(nodes(white_lh_smooth)))
nsurf <- NeuroSurface(white_lh_smooth, indices=1:length(random_vals), data=random_vals)
surfwidget(nsurf, cmap = heat.colors(256), irange = c(-2, 2))
```

**Alternative: Save to File**

For guaranteed display, save widgets to standalone HTML files:

``` r

widget <- surfwidget(white_lh_smooth, width = "100%", height = "400px")
htmlwidgets::saveWidget(widget, "interactive_surface.html", selfcontained = TRUE)
# Open the resulting HTML file in your browser
```

## Advantages over Static Plots

Interactive widgets offer several advantages:

- **Real-time interaction**: Rotate, zoom, and pan without re-rendering
- **Dynamic controls**: Adjust visualization parameters interactively
- **Web-based**: Works in HTML documents, Shiny apps, and R Markdown
- **Touch support**: Works on tablets and mobile devices
- **High-quality rendering**: Uses WebGL for smooth,
  hardware-accelerated graphics
- **Data exploration**: Click on vertices to see coordinate and data
  values
- **Multiple viewpoints**: Easily switch between anatomical orientations

These interactive capabilities make
[`surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
ideal for exploratory data analysis and interactive presentations.

## Next Steps

- [`vignette("displaying-surfaces")`](https://bbuchsbaum.github.io/neurosurf/articles/displaying-surfaces.md)
  — RGL-based 3D rendering with curvature shading, data overlays, and
  PNG snapshots.
- [`vignette("surface-figures")`](https://bbuchsbaum.github.io/neurosurf/articles/surface-figures.md)
  — publication-quality multi-view layouts with colourbars and atlas
  outlines.
- [`vignette("introduction-to-neurosurf")`](https://bbuchsbaum.github.io/neurosurf/articles/introduction-to-neurosurf.md)
  — the data structures (`SurfaceGeometry`, `NeuroSurface`,
  `NeuroSurfaceVector`) behind these visualizations.
