# Surface renderer decision

Date: 2026-08-05

Scope: bounded evaluation for neuromosaic epic
`bd-01KZA1VTK5GQN1C21T99D5JV4X`.

## Existing rgl path

`surface_plot()` and `view_surface()` provide per-vertex colors, smooth mesh
normals, orthographic cameras (`fov = 0`), an OpenGL z-buffer, and
supersampled panel capture. They remain the supported interactive renderer.

The static publication gate fails for two independent reasons:

1. Headless capture is not self-contained. `render_surface_plot()` requires a
   working real OpenGL context or `webshot2` and a browser. If neither capture
   works, its tracked behavior is to warn and emit a blank panel. This is not a
   deterministic CI, Slurm, or Quarto contract.
2. `.ns_compute_vertex_colors()` thresholds and maps scalar values to vertex
   RGBA before `rgl::shade3d(meshColor = "vertices")`. The GPU then
   interpolates mapped colors. It does not barycentrically interpolate the
   scientific scalar and threshold that scalar per fragment, so threshold
   geometry and filled colors cannot meet the numeric oracle.

The source path was inspected on a host with `rgl`, `webshot2`, and `png`
installed. No browser was launched for this decision: browser availability is
itself outside the deterministic backend contract. Runtime and memory of rgl
are driver/browser qualified and are therefore not accepted as portable
baselines.

## Disposition

Implement a CPU backend in neurosurf. The backend rasterizes triangle coverage,
uses a per-sample z-buffer, barycentrically interpolates scalar and anatomy
values, applies cortex masks, thresholds and maps the interpolated scalar,
composes premultiplied alpha, and downsamples deterministic supersamples. rgl
is retained unchanged for interactive exploration.

Numeric buffer invariants, permutation tests, threshold monotonicity, mask
exclusion, and analytic threshold crossing are the primary correctness gates.
PNG composites are secondary artifacts because device/font composition is not
the deterministic core.

## fsaverage6 baseline

The mandatory bundled fsaverage6 fixture was rendered on 2026-08-05 with
40,962 vertices per hemisphere, 600 by 375 output pixels, and 2x
supersampling. The four panels completed in 0.743 to 1.399 seconds each. The
full R process, including package loading and white-surface curvature
calculation, reached 1,089,437,696 bytes maximum resident set size and
770,035,328 bytes peak memory footprint as reported by macOS `/usr/bin/time
-l`. These are host-qualified baselines, not CI limits.

The receipt in `cpu-renderer-fsaverage6.csv` records per-panel pixel coverage,
overlay area, PNG size, and exact xxHash64 values for the deterministic RGBA
buffers. The four PNGs were visually inspected: lateral and medial anatomy are
distinct, curvature remains visible beneath the synthetic continuous field,
and no parcel or occlusion contours are present. fsLR-32k is intentionally a
conditional fixture until its supported template asset is present in the local
cache; no network download is hidden inside the fast test suite.

## Verification cadence

The analytic triangle, signed marching-triangle, differential slow-reference,
z-buffer order, vertex/face permutation, shared-edge, threshold monotonicity,
mask exclusion, camera landmark, degenerate/NA/extreme, and projection tests are
fast pull-request gates. The bundled fsaverage6 four-view receipt is rerun for
renderer releases and when camera, raster coverage, scalar mapping, mask,
curvature, or antialiasing code changes. Full-size visual inspection and
runtime/memory receipts are release gates rather than exact per-PR image
comparisons. fsLR-32k runs on the same release cadence when its TemplateFlow
asset is already available in the supported cache; an absent external asset is
reported rather than downloaded implicitly or counted as a fast-suite failure.
