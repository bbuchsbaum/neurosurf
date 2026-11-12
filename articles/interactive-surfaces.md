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

The example uses the fsaverage5 template bundled with neurosurf:
inflated surfaces for display, white surfaces to place the example
clusters, sulcal depth for the grey anatomy underneath the maps, and the
cortex mask that keeps colour off the medial wall in the static figure.

``` r

inflated <- load_fsaverage("fsaverage5", "inflated")
white <- load_fsaverage("fsaverage5", "white")
sulc <- load_fsaverage_sulc("fsaverage5")
cortex <- load_fsaverage_cortex("fsaverage5")

# Display anatomy, lighter on gyri. FreeSurfer sulc is positive in
# sulci, so it is negated. The report preset's grey ramp saturates at
# |0.1|; scaling to that range keeps it continuous and quiet.
anatomy <- list(
  left = 0.2 * normalize_surface_anatomy(-sulc$lh, "continuous"),
  right = 0.2 * normalize_surface_anatomy(-sulc$rh, "continuous")
)
```

The browser renders the values that R supplies. Perform thresholding,
capping, atlas projection, and other scientific transformations before
constructing a layer. Here two synthetic *z* maps are built from smooth
clusters placed at white-surface coordinates and falling off with
distance over the inflated surface:

``` r

cluster <- function(hemi, centre, peak, width = 12) {
  seed <- which.min(colSums((t(coords(white[[hemi]])) - centre)^2))
  xyz <- coords(inflated[[hemi]])
  peak * exp(-colSums((t(xyz) - xyz[seed, ])^2) / (2 * width^2))
}
contrast <- function(centres, lh_peaks, rh_peaks) {
  one_side <- function(hemi, side, peaks) {
    Reduce(`+`, Map(function(centre, peak) {
      cluster(hemi, centre * c(side, 1, 1), peak)
    }, centres, peaks))
  }
  list(left = one_side("lh", -1, lh_peaks),
       right = one_side("rh", 1, rh_peaks))
}

memory <- contrast(
  centres = list(c(45, -60, 20), c(40, 20, 30), c(55, -20, 0)),
  lh_peaks = c(4, 3, -3.5), rh_peaks = c(3.5, 2.5, -2.8)
)
# A right-hand movement: left motor cortex dominates
motor <- contrast(
  centres = list(c(38, -22, 56), c(10, -60, 50)),
  lh_peaks = c(4.5, -3), rh_peaks = c(-2.2, -3)
)
```

A binary region of interest, used below as a reference layer: every
vertex within 18 mm of a point in the intraparietal sulcus, again
measured over the inflated surface.

``` r

roi <- function(hemi, centre, radius = 18) {
  seed <- which.min(colSums((t(coords(white[[hemi]])) - centre)^2))
  xyz <- coords(inflated[[hemi]])
  as.numeric(colSums((t(xyz) - xyz[seed, ])^2) < radius^2)
}
region <- list(left = roi("lh", c(-30, -55, 45)),
               right = roi("rh", c(30, -55, 45)))
```

## How do you build one bilateral, multi-map viewer?

Create each named map with
[`surface_layer()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_layer.md),
then put both geometries and all layers in one scene. `selected_layer`
chooses the map shown when the viewer opens.

For thresholded two-sided maps, use a palette whose tails start at
clearly visible colours and never pass through the light anatomy grey:
blue for negative, red for positive.
[`surface_palette()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_palette.md)
with the same limits and threshold returns exactly the colours
[`surface_figure()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_figure.md)
would use, so the interactive and static figures agree. The layer’s
`threshold` hides \|*z*\| \< 2 and marks the gap in the colour key;
`magnitude_opacity` fades colour in between \|*z*\| = 2 and 2.6, so
cluster edges are smooth instead of following the mesh’s triangles.
Unthresholded values are passed in, and `NA` is kept for genuinely
missing data.

``` r

z_colors <- surface_palette("two_sided", limits = c(-5, 5),
                            threshold = 2)
z_layer <- function(name, values, title) {
  surface_layer(name, values, colormap = z_colors, limits = c(-5, 5),
                threshold = c(-2, 2), units = "z",
                magnitude_opacity = list(low = 2, high = 2.6),
                legend = list(title = title))
}

scene <- surface_scene(
  left = inflated$lh,
  right = inflated$rh,
  curvature = anatomy,
  layers = list(
    z_layer("memory", memory, "Memory contrast"),
    z_layer("motor", motor, "Motor contrast")
  ),
  selected_layer = "memory",
  metadata = list(template = "fsaverage5"),
  provenance = list(source = "synthetic clusters on package data"),
  fallback = "Inflated cortical surfaces with two thresholded z maps.",
  alt_text = paste(
    "Inflated left and right cortical surfaces showing a selectable",
    "thresholded z map, blue negative and red positive, |z| above 2."
  ),
  preset = "report",
  mode = "report"
)
```

Pass the scene to
[`surfwidget()`](https://bbuchsbaum.github.io/neurosurf/reference/surfwidget-methods.md)
once. The map selector changes the active values and legend without
constructing another widget or another WebGL context.

``` r

surfwidget(scene, width = "100%", height = "420px")
```

Inflated cortical surfaces with two thresholded z maps.

The report toolbar provides the authored map selector, coordinated
anatomical views, reset, and PNG export. Drag to rotate, use the wheel
or a pinch gesture to zoom, and right-drag to pan.

## How do you keep a reference region beneath the selected effect map?

Give one binary layer `role = "reference"` and an explicit color. It has
its own checkbox, opacity slider and categorical swatch. It stays
beneath either quantitative map and cannot become the selected map or
enter effect readouts. Reset restores its authored visibility and
opacity.

``` r

reference_scene <- surface_scene(
  left = inflated$lh, right = inflated$rh,
  layers = c(scene@layers, list(surface_layer(
    "reference", region, role = "reference", color = "#238b45",
    opacity = 0.4, legend = list(title = "Intraparietal ROI"),
    provenance = list(source = "synthetic 18 mm disc")
  ))),
  curvature = anatomy,
  preset = "report",
  fallback = "Bilateral z maps over a faint intraparietal ROI.",
  alt_text = "Thresholded z maps over a green intraparietal region."
)
stopifnot(identical(reference_scene@selected_layer, "memory"))
```

``` r

surfwidget(reference_scene, width = "100%", height = "360px")
```

In the viewer the green region sits beneath whichever map is selected
and keeps its own swatch, checkbox and opacity slider; it never enters
the colour key. (The widget is not embedded here to keep this article
small; the static figure below shows the same composition.) References
require a full 0/1 mask per hemisphere, with no limits, units, indices
or thresholds.

The CPU renderer draws the same membership beneath a static map, in the
order anatomy, reference, effect:

``` r

image <- render_surface_rgba(
  inflated$lh, memory$left, anatomy_metric = sulc$lh,
  cortex_mask = cortex$lh,
  threshold = 2, limits = c(-5, 5), alpha_ramp = 0.6,
  reference = list(
    id = "reference", label = "Intraparietal ROI",
    mask = region$left, color = "#238b45", opacity = 0.4
  ),
  width = 680, height = 420, antialias = 2
)
surface_panels(image, labels = NULL, legend_title = "z",
               references = c("Intraparietal ROI" = "#238b45"))
```

![Left hemisphere, lateral view: the intraparietal ROI (green, 40%
opacity) beneath the memory \*z\*
map.](interactive-surfaces_files/figure-html/static-reference-1.png)

Left hemisphere, lateral view: the intraparietal ROI (green, 40%
opacity) beneath the memory *z* map.

Both renderers classify membership at 0.5 after linear interpolation,
and when a cortex mask is supplied, triangles outside it never receive
reference colour. A static figure should label the reference with its
own swatch, outside the effect colour scale, as
[`surface_panels()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_panels.md)
does above.

Supply already projected binary membership. For a volume label, use
explicit categorical/nearest-voxel sampling and record the source and
mesh identity, depth aggregation and projection parameters in
`provenance`. This API does not perform projection or inference.

## Which option controls appearance and behavior?

`preset` sets appearance and `mode` sets behaviour; the scene above uses
`preset = "report"` with `mode = "report"`.

| Setting | Controls | What it changes |
|----|----|----|
| `preset = "report"` | Appearance | Quiet sulcal greys on a neutral grey panel, lit for on-screen reading; made for report widgets. Its grey ramp turns two-tone unless the anatomy is scaled, as above. |
| `preset = "paper-light"` | Appearance | White background, stronger folding contrast, and print-oriented PNG defaults. |
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
#>      content assets  bytes
#> 1   geometry      5 573504
#> 2 map values      4 163872
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
write_surface_scene(scene, "surface-report-inline",
                    self_contained = TRUE)
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
  window.addEventListener("pagehide", () => handle.dispose(),
                          { once: true });
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

surface <- NeuroSurface(inflated$lh, nodes(inflated$lh), memory$left)
surfwidget(surface, irange = c(-5, 5))
```

Use an explicit `SurfaceScene` for bilateral figures, multiple named
maps, portable assets, authored fallback text, provenance, or report
behavior.

## What should you read next?

- [`vignette("surface-figures")`](https://bbuchsbaum.github.io/neurosurf/articles/surface-figures.md)
  builds static, publication-quality multi-view figures with
  [`surface_figure()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_figure.md).
- the [Displaying surfaces with
  RGL](https://bbuchsbaum.github.io/neurosurf/articles/displaying-surfaces.html)
  article covers RGL-based 3D rendering, curvature shading, and PNG
  snapshots.
- [`vignette("introduction-to-neurosurf")`](https://bbuchsbaum.github.io/neurosurf/articles/introduction-to-neurosurf.md)
  introduces `SurfaceGeometry`, `NeuroSurface`, and the package’s other
  core data structures.
