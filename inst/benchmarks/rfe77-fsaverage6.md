# RFE 77 fsaverage6 performance receipt

This receipt measures one report viewer containing the fsaverage6 left and
right inflated surfaces, curvature, and two complete bilateral Float32 maps.
The source geometry has 40,962 vertices and 81,920 faces per hemisphere.

The reference run used an Apple M3 Max host running Darwin 23.3.0 and the
project-controlled headless Chromium 141.0.7390.37. The first navigation was
cold; the following nine reused the browser cache. The median time to
interactive was 504.7 ms, below the 2,000 ms reference-host criterion. The
slowest measured run was 703.5 ms, below the 5,000 ms CI regression ceiling.

The raw vertex and face payload is 1,966,128 bytes. Curvature is 327,696
bytes. Each complete bilateral map is 327,696 bytes, exactly four bytes per
vertex and without repeated topology. The manifest is 5,827 bytes. The small
ordinary HTML shell is 7,898 bytes; its first run transferred 2,449,495 bytes
and reported 4,598,558 encoded body bytes including the vendored runtime and
all scene assets. Typed-array construction averaged 0.003 ms over 100 repeats.

Browser assertions proved the following:

- Three.js reports revision 185 and one WebGL context was created.
- Both hemispheres contain non-background pixels in a 900 by 600 PNG.
- Map switching preserved both geometry objects and the three deduplicated
  geometry requests.
- Browser value-asset SHA-256 values exactly matched the authoritative R
  manifest values.
- Lateral, medial, dorsal, ventral, and reset orientations were deterministic.
- Dragging changed both the camera quaternion and PNG pixel digest.
- Resize updated both CSS and drawing-buffer dimensions to 900 by 600.
- Disposal returned active animation frames to zero.
- Ordinary HTML, R Markdown, and Quarto made no remote requests and emitted no
  console errors, page errors, or failed resources.
- No-JavaScript, WebGL, load, checksum, print, and unsupported-fullscreen paths
  exposed authored fallback or disabled states.
- Browser ownership audits before and after the suite found no automated
  top-level browser processes.

The machine-readable receipt is `rfe77-fsaverage6.json` in this directory.

## Reproduce

Fetch the reference mesh with Nilearn, then generate the ordinary report:

```bash
uv run --with nilearn python -c \
  'from nilearn.datasets import fetch_surf_fsaverage; fetch_surf_fsaverage("fsaverage6", data_dir="/tmp/neurosurf-rfe77/nilearn-data")'

RGL_USE_NULL=TRUE Rscript tools/rfe77_fsaverage6_scene.R \
  /tmp/neurosurf-rfe77/nilearn-data/fsaverage6 \
  /tmp/neurosurf-rfe77/fsaverage6
```

Render the R Markdown and Quarto fixtures under `tests/fixtures`, serve
`/tmp/neurosurf-rfe77` at `http://127.0.0.1:8765`, and run:

```bash
cd ~/code/jscode/surfviewjs
NEUROSURF_RFE77_URL=http://127.0.0.1:8765 \
NEUROSURF_RFE77_RECEIPT=/tmp/neurosurf-rfe77/browser-metrics.json \
npx playwright test \
  --config=~/code/neurosurf/tests/browser/playwright.config.mjs
```
