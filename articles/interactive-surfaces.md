# Build bilateral interactive surface reports

An HTML report often needs one interactive figure for both hemispheres
and several result maps. Repeating a widget for every map wastes space
and sends the same mesh to the browser many times.
[`surface_scene()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_scene.md)
describes the geometries, maps, labels, and fallback once;
[`surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
displays that scene in R Markdown or Quarto; and
[`write_surface_scene()`](https://bbuchsbaum.github.io/neurosurf/reference/write_surface_scene.md)
writes the same scene as an ordinary web page.

This article builds one bilateral viewer with two selectable maps. The
example uses package data, runs without a network connection, and keeps
the scientific transformation in R.

## What does a scene contain?

A `SurfaceScene` has one `SurfaceGeometry` per hemisphere and one or
more named scalar layers. Each bilateral layer supplies a left and right
value vector whose lengths match the corresponding geometry. A scene
also records the initial map, legend metadata, provenance, alternative
text, and fallback text.

The example surfaces are small meshes shipped with neurosurf:

``` r

left_geometry <- read_surf(system.file(
  "extdata", "std.8_lh.smoothwm.asc", package = "neurosurf"
))
right_geometry <- read_surf(system.file(
  "extdata", "std.8_rh.smoothwm.asc", package = "neurosurf"
))
```

The browser renders the values that R supplies. Perform thresholding,
capping, atlas projection, and other scientific transformations before
constructing a layer. Here we create a deterministic coordinate-based
demonstration map and replace sub-threshold values with `NA` in R:

``` r

scaled_height <- function(geometry) {
  value <- coords(geometry)[, 3]
  3 * (value - mean(value)) / max(abs(value - mean(value)))
}

effect <- list(
  left = scaled_height(left_geometry),
  right = scaled_height(right_geometry)
)
effect <- lapply(effect, function(x) replace(x, abs(x) < 1.25, NA_real_))
```

The second map is a simple two-region label used to demonstrate map
switching:

``` r

region <- list(
  left = as.numeric(coords(left_geometry)[, 2] > 0),
  right = as.numeric(coords(right_geometry)[, 2] > 0)
)
```

## How do you build one bilateral, multi-map viewer?

Create each named map with
[`surface_layer()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_layer.md),
then put both geometries and all layers in one scene. `selected_layer`
chooses the map shown when the viewer opens.

``` r

scene <- surface_scene(
  left = left_geometry,
  right = right_geometry,
  layers = list(
    surface_layer(
      "effect", effect,
      colormap = c("#2166ac", "#f7f7f7", "#b2182b"),
      limits = c(-3, 3), units = "z",
      legend = list(title = "Thresholded effect")
    ),
    surface_layer(
      "region", region,
      colormap = c("#d9d9d9", "#238b45"),
      limits = c(0, 1), units = "class",
      legend = list(title = "Demonstration region")
    )
  ),
  selected_layer = "effect",
  metadata = list(subject = "example"),
  provenance = list(source = "neurosurf package data"),
  fallback = "Static bilateral surface figure with effect and region maps.",
  alt_text = paste(
    "Bilateral cortical surfaces with selectable thresholded effect",
    "and demonstration region maps."
  ),
  preset = "paper-light",
  mode = "report"
)
```

Pass the scene to
[`surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
once. The map selector changes the active values and legend without
constructing another widget or another WebGL context.

``` r

surfwidget(scene, width = "100%", height = "520px")
```

Static bilateral surface figure with effect and region maps.

The report toolbar provides the authored map selector, coordinated
anatomical views, reset, and PNG export. Drag to rotate, use the wheel
or a pinch gesture to zoom, and right-drag to pan.

## Which option controls appearance and behavior?

`paper-light` and `report` name different contracts:

| Setting | Controls | What it changes |
|----|----|----|
| `preset = "paper-light"` | Appearance | Light background, restrained lighting and material values, labels, and PNG defaults. |
| `mode = "report"` | Behavior | Lazy mounting, the compact report toolbar, deterministic initial framing, and fallback/print behavior. |
| `mode = "viewer"` | Behavior | A bare rotatable viewer without the report toolbar. |

Tweakpane is deprecated. The report runtime does not load it or contact
a CDN. Legacy `showControls` and `controlType` configuration entries
warn during the migration; use the scene `mode` instead.

## How is geometry shared across maps?

[`surface_scene_manifest()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_scene_manifest.md)
converts numeric arrays to typed binary assets and addresses each asset
by its SHA-256 digest. The scene refers to each hemisphere geometry
once. Each additional full bilateral map contributes two value arrays,
not another copy of the vertices or faces.

``` r

manifest <- surface_scene_manifest(scene, asset_mode = "inline")
roles <- vapply(manifest$assets, `[[`, character(1), "role")
bytes <- vapply(manifest$assets, `[[`, numeric(1), "byteLength")

data.frame(
  content = c("geometry", "map values"),
  assets = c(sum(roles %in% c("vertices", "faces", "curvature")),
             sum(roles == "values")),
  bytes = c(sum(bytes[roles %in% c("vertices", "faces", "curvature")]),
            sum(bytes[roles == "values"]))
)
#>      content assets bytes
#> 1   geometry      3 30768
#> 2 map values      4 10272
```

Missing map values remain IEEE `NaN` values in the binary payload. The
browser verifies every adjacent asset’s byte length and checksum before
constructing the scene.

## How do you write an offline report?

Use
[`write_surface_scene()`](https://bbuchsbaum.github.io/neurosurf/reference/write_surface_scene.md)
when the viewer must live outside an R Markdown or Quarto document. Both
output modes bundle the compatible surfviewjs runtime locally and make
no network requests.

``` r

# index.html, the runtime, and content-addressed .bin files
write_surface_scene(scene, "surface-report", self_contained = FALSE)

# one index.html with the runtime and typed arrays inlined
write_surface_scene(scene, "surface-report-inline", self_contained = TRUE)
```

Choose adjacent assets for a site or a report directory: the browser can
cache the runtime and SHA-addressed arrays, and repeated assets are
deduplicated. Choose a self-contained file when a single portable HTML
file matters more than file size or cross-page caching.

## How do you mount the scene on a plain web page?

The HTML writer produces a minimal page that uses the public
`surfview.mountSurfView()` API. You can customize its generated
`index.html` or use the same lifecycle in another page. In this example,
`manifest` is a `surfview.scene.v1` object produced by
[`surface_scene_manifest()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_scene_manifest.md):

``` html
<div id="surface-report"></div>
<script src="./surfview.embed.iife.js"></script>
<script>
  const handle = surfview.mountSurfView(
    document.getElementById("surface-report"),
    manifest,
    {
      lazy: true,
      preset: "paper-light",
      controls: true,
      baseUrl: document.baseURI,
      bilateralGroup: {
        id: "bilateral",
        leftSurfaceId: "left",
        rightSurfaceId: "right"
      }
    }
  );

  handle.ready.then(() => handle.setView("lateral"));
  window.addEventListener("pagehide", () => handle.dispose(), { once: true });
</script>
```

A manifest with both hemispheres must name a `bilateralGroup` that pairs
the left and right surface ids; without it the report runtime refuses to
mount.
[`surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
and
[`write_surface_scene()`](https://bbuchsbaum.github.io/neurosurf/reference/write_surface_scene.md)
supply this option automatically.

The returned handle also provides `selectLayer()`, `resize()`,
`exportPNG()`, and `dispose()`. Call `dispose()` when an application
removes or replaces the viewer so it can release observers, listeners,
animation frames, the WebGL context, and GPU resources.

## What happens when interaction is unavailable?

`fallback` is required plain text. The widget and standalone writer
expose it when JavaScript is disabled or WebGL initialization fails, and
print CSS shows it instead of a blank canvas. `alt_text` labels the
interactive figure for assistive technology.

The fallback text is not a bitmap renderer. If a report needs a visual
print, PDF, or archival representation, build a static figure from the
same R values with
[`surface_figure()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_figure.md);
see
[`vignette("surface-figures")`](https://bbuchsbaum.github.io/neurosurf/articles/surface-figures.md).

## When should you use the legacy shortcuts?

[`surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
still accepts `SurfaceGeometry`, `NeuroSurface`,
`ColorMappedNeuroSurface`, and `VertexColoredNeuroSurface`. Those
methods adapt a single surface to a `SurfaceScene` and are useful for
quick exploration:

``` r

values <- scaled_height(left_geometry)
surface <- NeuroSurface(left_geometry, nodes(left_geometry), values)
surfwidget(surface, irange = c(-3, 3))
```

Use an explicit `SurfaceScene` for bilateral figures, multiple named
maps, portable assets, authored fallback text, provenance, or report
behavior.

## What should you read next?

- [`vignette("surface-figures")`](https://bbuchsbaum.github.io/neurosurf/articles/surface-figures.md)
  builds static, publication-quality multi-view figures with
  [`surface_figure()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_figure.md).
- [`vignette("displaying-surfaces")`](https://bbuchsbaum.github.io/neurosurf/articles/displaying-surfaces.md)
  covers RGL-based 3D rendering, curvature shading, and PNG snapshots.
- [`vignette("introduction-to-neurosurf")`](https://bbuchsbaum.github.io/neurosurf/articles/introduction-to-neurosurf.md)
  introduces `SurfaceGeometry`, `NeuroSurface`, and the package’s other
  core data structures.
