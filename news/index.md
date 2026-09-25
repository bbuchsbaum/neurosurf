# Changelog

## neurosurf (development version)

### Interactive report scenes

- The bundled surfview runtime is repinned to surfviewjs 2d9f786. Report
  scenes gain an anatomical whole-brain layout with a data-driven
  overview pose, a `report` preset (sulcal two-tone underlay,
  camera-attached lighting, anti-aliased threshold isolines),
  overlay-aware camera framing, and fixes for self-overlap artefacts on
  inflated surfaces, ignored curvature restyling, and hover picks that
  reported a hidden hemisphere. The runtime patch is now empty: every
  former patch hunk is upstream.

### Parcel maps

- New publication parcel-map renderer:
  [`prepare_surface_parcels()`](https://bbuchsbaum.github.io/neurosurf/reference/prepare_surface_parcels.md)
  and
  [`render_surface_parcels()`](https://bbuchsbaum.github.io/neurosurf/reference/render_surface_parcels.md)
  rasterize a labelled surface with deferred per-pixel shading. Parcel
  boundaries follow smoothed label memberships, so they are smooth and
  antialiased rather than stair-stepped along triangle edges; the
  underlay combines a sulcal-depth proxy with soft Lambert lighting;
  filled parcels get a thin outline and the medial wall is drawn flat.
  [`surface_parcel_style()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_parcel_style.md)
  collects the settings, and
  [`surface_sulcal_proxy()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_sulcal_proxy.md)
  estimates sulcal depth from white and inflated meshes.

### Static surface figures

[`render_surface_rgba()`](https://bbuchsbaum.github.io/neurosurf/reference/render_surface_rgba.md)
and
[`surface_figure()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_figure.md)
produce substantially better figures by default:

- Surfaces are lit with interpolated normals (two-light Blinn-Phong with
  hemispheric ambient), so cortical form is visible. `lighting = FALSE`
  restores flat rendering; a named list tunes it. Overlay shading is
  normalized so camera-facing overlay matches the colour bar.
- Coarse meshes are subdivided for display (`subdivide = "auto"`) with
  an interpolating butterfly scheme. Original vertex values are
  preserved and new values are clamped to their edge’s endpoints, so no
  suprathreshold region appears that linear interpolation would not
  show.
- Palettes span only the suprathreshold range, so the weakest displayed
  value is clearly coloured. New defaults suit each `tail`; the positive
  palette runs light to dark.
- The anatomy underlay is lighter, with sulci darker than gyri.
  `anatomy_invert = "auto"` detects metrics that are larger in sulci
  (FreeSurfer `sulc`, `curv`) and inverts them.
- The silhouette contour is anti-aliased, the default background is
  white, and the default supersampling is 3.
- Medial views are now true camera views (the left hemisphere’s anterior
  pole is on the right). Previously they were mirror images.
- [`surface_figure()`](https://bbuchsbaum.github.io/neurosurf/reference/surface_figure.md)
  shows dorsal and ventral views of both hemispheres in one bilateral
  panel beside the lateral/medial rows, adds hemisphere and view headers
  (`labels`), and draws a colour bar with only the suprathreshold
  segments, labelled at limits and thresholds.
- Overlay colour stays within a few percent of its colour-bar colour at
  any viewing angle; lighting conveys form, the palette conveys value.
- Bilateral dorsal and ventral panels carry anterior/posterior markers.

### Templates and I/O

- The fsaverage5 template (inflated and white surfaces, sulcal depth) is
  bundled: `load_fsaverage("fsaverage5")` and
  [`load_fsaverage_sulc()`](https://bbuchsbaum.github.io/neurosurf/reference/load_fsaverage_sulc.md).
  The README and figure vignette use it instead of the legacy std.8
  mesh.
- [`read_surf_geometry()`](https://bbuchsbaum.github.io/neurosurf/reference/read_surf_geometry.md)
  reads gzipped GIFTI (`.gii.gz`) surfaces; it previously failed on
  them.

## neurosurf 0.1.0

- Initial CRAN submission.
