# PySurfer feature notes vs neurosurf

- **Time-series + exports:** PySurfer handles time-indexed overlays, includes a TraitsUI TimeViewer (slider for time and color-scale controls), and can save image sequences, montages, rotations, and movies (`surfer/viz.py` methods `add_data` with time, `save_image_sequence`, `save_montage`, `save_movie`, `animate`). Neurosurf currently renders static frames only (`R/surface_plot.R`, `R/snapshot_surface.R`).
- **Sparse/vector data:** PySurfer can take undersampled vertex data (builds smoothing matrices) and 3D vectors (quiver glyphs). Neurosurf requires full-length scalars and has no vector glyph support.
- **Signed overlays with dual colorbars:** PySurfer splits pos/neg overlays into separate surfaces/colorbars. Neurosurf maps a single colormap per layer; no paired pos/neg bars.
- **Contour and morphometry overlays:** PySurfer offers contour isolines and convenience loaders for FreeSurfer morph metrics (area/curv/sulc/thickness). Neurosurf has no contour helper and expects morph data to be provided by the caller.
- **Foci mapping:** PySurfer can map MNI coordinates through a surface to vertex space before plotting spheres. Neurosurf only plots spheres at provided coordinates (no snap-to-surface).
- **Interactive colormap scaling:** PySurfer exposes runtime fmin/fmid/fmax/opacity adjustments (and UI). Neurosurf fixes color scales at layer creation with no interactive adjustment.
