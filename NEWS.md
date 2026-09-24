# neurosurf (development version)

## Parcel maps

* New publication parcel-map renderer: `prepare_surface_parcels()` and
  `render_surface_parcels()` rasterize a labelled surface with deferred
  per-pixel shading. Parcel boundaries follow smoothed label memberships, so
  they are smooth and antialiased rather than stair-stepped along triangle
  edges; the underlay combines a sulcal-depth proxy with soft Lambert
  lighting; filled parcels get a thin outline and the medial wall is drawn
  flat. `surface_parcel_style()` collects the settings, and
  `surface_sulcal_proxy()` estimates sulcal depth from white and inflated
  meshes.

## Static surface figures

`render_surface_rgba()` and `surface_figure()` produce substantially better
figures by default:

* Surfaces are lit with interpolated normals (two-light Blinn-Phong with
  hemispheric ambient), so cortical form is visible. `lighting = FALSE`
  restores flat rendering; a named list tunes it. Overlay shading is
  normalized so camera-facing overlay matches the colour bar.
* Coarse meshes are subdivided for display (`subdivide = "auto"`) with an
  interpolating butterfly scheme. Original vertex values are preserved and
  new values are clamped to their edge's endpoints, so no suprathreshold
  region appears that linear interpolation would not show.
* Palettes span only the suprathreshold range, so the weakest displayed value
  is clearly coloured. New defaults suit each `tail`; the positive palette
  runs light to dark.
* The anatomy underlay is lighter, with sulci darker than gyri.
  `anatomy_invert = "auto"` detects metrics that are larger in sulci
  (FreeSurfer `sulc`, `curv`) and inverts them.
* The silhouette contour is anti-aliased, the default background is white,
  and the default supersampling is 3.
* Medial views are now true camera views (the left hemisphere's anterior pole
  is on the right). Previously they were mirror images.
* `surface_figure()` shows dorsal and ventral views of both hemispheres in one
  bilateral panel beside the lateral/medial rows, adds hemisphere and view
  headers (`labels`), and draws a colour bar with only the suprathreshold
  segments, labelled at limits and thresholds.

# neurosurf 0.1.0

* Initial CRAN submission.
