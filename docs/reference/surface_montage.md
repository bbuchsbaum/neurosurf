# Arrange multiple surface views into a single montage figure

Renders several surface views as one figure: a row (the default) or grid
of panels. It is designed for R Markdown / pkgdown documents, where it
replaces the repetitive "snapshot each panel, check it, otherwise fall
back to an interactive widget" boilerplate with a single call.

When the session can produce static snapshots (an interactive \`rgl\`
device, or a headless build with webshot2 installed) the panels are
captured and tiled into one PNG. When knitting this is returned via
[`include_graphics`](https://rdrr.io/pkg/knitr/man/include_graphics.html);
otherwise the composed image is written to `file`. If snapshots are
unavailable, the panels are drawn into a single
[`mfrow3d`](https://dmurdoch.github.io/rgl/dev/reference/mfrow3d.html)
scene and returned as one
[`rglwidget`](https://dmurdoch.github.io/rgl/dev/reference/rglwidget.html)
(a single combined widget, never one widget per panel).

## Usage

``` r
surface_montage(
  panels,
  ncol = NULL,
  nrow = NULL,
  width = 600,
  height = 450,
  file = NULL,
  ...
)
```

## Arguments

- panels:

  A list of panels. Each element is either a surface object
  ([`SurfaceGeometry`](SurfaceGeometry.md),
  [`NeuroSurface`](NeuroSurface.md) and its subclasses, or a
  `SurfaceSet`), a file path readable by
  [`read_surf_geometry`](read_surf_geometry.md), or a list whose first
  *unnamed* element is such an object and whose remaining *named*
  elements are per-panel display arguments (for example `thresh`,
  `vals`, `cmap`, `irange`, or `viewpoint`). Per-panel arguments
  override the shared arguments passed through `...`.

- ncol, nrow:

  Integer layout controls. If both are `NULL` (default), all panels are
  placed in a single row. If only one is supplied the other is derived
  from the number of panels.

- width, height:

  Pixel dimensions used when snapshotting each panel.

- file:

  Optional output path for the composed PNG. When knitting, a figure
  path is generated automatically and this is ignored. When called
  interactively, supplying `file` forces composition to that path
  instead of opening an interactive window.

- ...:

  Shared display arguments forwarded to every panel (e.g. `cmap`,
  `irange`, `specular`, `viewpoint`). These are passed to
  [`view_surface`](view_surface.md) (for `SurfaceGeometry` panels) or to
  the object's `plot` method (for data surfaces).

## Value

When knitting, a `knitr` image object (static path) or a single
`rglwidget` (fallback). When called interactively, the composed file
path (if `file` is given) or `invisible(NULL)` after drawing an
interactive `mfrow3d` window.

## Details

Each static panel is rendered in its own fresh `rgl` scene, so lighting
is computed independently and panels never share or clobber one
another's lights. Panels are centred on equally sized white cells so
differing silhouettes stay aligned.

## See also

[`view_surface`](view_surface.md),
[`snapshot_surface`](snapshot_surface.md),
[`surface_plot`](surface_plot.md)

## Examples

``` r
# \donttest{
geom <- example_surface_geometry()
set.seed(1)
vals <- rnorm(nrow(coords(geom)))
ns <- smooth(NeuroSurface(geom, indices = seq_along(vals), data = vals))

out <- tempfile(fileext = ".png")
if (interactive()) {
  surface_montage(
    list(
      ns,
      list(ns, thresh = c(-0.5, 0.5)),
      list(ns, thresh = c(-1, 1))
    ),
    cmap = grDevices::rainbow(100), irange = c(-2, 2),
    ncol = 3, file = out
  )
}
# }
```
