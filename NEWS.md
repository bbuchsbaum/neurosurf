# neurosurf (development version)

* New publication parcel-map renderer: `prepare_surface_parcels()` and
  `render_surface_parcels()` rasterize a labelled surface with deferred
  per-pixel shading. Parcel boundaries follow smoothed label memberships, so
  they are smooth and antialiased rather than stair-stepped along triangle
  edges; the underlay combines a sulcal-depth proxy with soft Lambert
  lighting; filled parcels get a thin outline and the medial wall is drawn
  flat. `surface_parcel_style()` collects the settings, and
  `surface_sulcal_proxy()` estimates sulcal depth from white and inflated
  meshes.

# neurosurf 0.1.0

* Initial CRAN submission.
