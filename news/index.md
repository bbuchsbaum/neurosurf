# Changelog

## neurosurf (development version)

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

## neurosurf 0.1.0

- Initial CRAN submission.
