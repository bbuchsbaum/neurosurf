# Smooth parcel partition: fsaverage5 qualification

The opt-in `boundary_method = "smooth"` was checked on the left hemisphere
of the Schaefer 2018 200-parcel / 7-network fsaverage5 atlas. This record is
local numerical and visual evidence, not cross-platform or cortex-studio
qualification. No general smoothness or speedup claim follows from it.

## Pinned inputs

- Annotation repository: `ThomasYeoLab/CBIG`, commit
  `35b5664bec8822e2f77da5e090e96f91d0095be6`.
- Annotation path:
  `stable_projects/brain_parcellation/Schaefer2018_LocalGlobal/Parcellations/FreeSurfer5.3/fsaverage5/label/lh.Schaefer2018_200Parcels_7Networks_order.annot`.
- Annotation SHA-256:
  `6c2d0e6e85841f9031f4193a9ceaca34023857c9aaf68538801d78b1ec367b5f`.
- Geometry: bundled `inst/extdata/fsaverage5/infl_left.gii.gz`, SHA-256
  `1a571c9d598a476a202c7295c235a709de63360047b92c0e7b75ed18b0f37e74`.
  Its source is nilearn commit `e5faab66dc2fe72d2cb50a8aaf2f44813aba4a51`.
- The annotation's explicit colour-table IDs are used without resampling;
  ID 0 is the medial wall. Its packed RGB code is nonzero and is not used
  directly as a parcel ID.

The mesh has 10,242 vertices and 20,480 faces, with 100 left-hemisphere
parcels and 870 medial-wall vertices. The smallest parcel has 44 vertices.

## Source identity and environment

The measured source files have these SHA-256 hashes:

| File | SHA-256 |
| --- | --- |
| `R/parcel_partition.R` | `4c953e405390cea6f03d56bf68afe9b85dfaebbd4ff3480eb86b05dd091cf941` |
| `R/render_surface_parcels.R` | `66fdfc4f152d33bdcfa827f40d8f7e755d5826e6b23fa9b6eb0c95934dd32071` |

R 4.5.1, aarch64-apple-darwin20; Darwin 23.3.0 arm64. Rendering used the
CPU parcel renderer, headless rgl, 480-by-360 output, 2x supersampling,
the lateral and medial canonical cameras, and `t_clamp = c(0.1, 0.9)`.

## Results

| Measurement | 0 iterations | 3 iterations |
| --- | ---: | ---: |
| Preparation elapsed seconds | 0.690 | 0.598 |
| Lateral rendering elapsed seconds | 2.276 | 0.836 |
| Medial rendering elapsed seconds | 1.098 | 0.900 |
| Prepared-object bytes | 10,781,712 | 10,781,712 |
| Crossings | 4,080 | 4,080 |
| Three-label junctions | 200 | 200 |
| Chained paths | 300 | 300 |
| Clamped crossings | 0 | 86 |
| Projected junctions | 0 | 43 |
| Face-corner label checks passed | 61,440 | 61,440 |
| Original label adjacency pairs preserved | 298 | 298 |
| Segments emitted exactly once | 4,380 | 4,380 |

`/usr/bin/time -l` reported maximum resident set size **1,718,239,232 bytes**
for the entire R process, including package loading, both preparations and
four renders; this is not the partition's standalone memory requirement.
The reported peak physical memory footprint was 714,772,480 bytes.
These are single-process, single-run measurements without a warm-up study;
an independent package check was running concurrently on the same machine.

The four generated PNGs were inspected. The three-iteration lateral and
medial outlines showed reduced mesh-scale zigzagging relative to zero
iterations, with a continuous medial-wall border and no visible gaps.
Pixel visibility of every small parcel is not guaranteed at this resolution;
vertex-label preservation was checked numerically.

The ordinary tests separately cover analytic tetrahedron failures, the
octahedron candidate seam, small parcels and thin necks, closed paths,
coincident disconnected components, input permutations, and exact-boundary
tie handling. Raster parity uses an independent polygon ray-crossing oracle
at the renderer's visible-face sample locations.

The rasterizer admits samples up to its existing edge tolerance outside an
original face. Smooth rendering clips negative barycentrics to zero and
renormalizes these samples for label selection only. A dedicated regression
checks this case; depth and shading still use the original raster weights.

## Reproduce

Download the annotation from the pinned commit and run from the package root:

```sh
RGL_USE_NULL=TRUE /usr/bin/time -l Rscript --vanilla \
  tools/check-parcel-partition.R /path/to/lh.annot /tmp/parcel-check
```

The script verifies input hashes and writes `receipt.json` plus
`lateral-0.png`, `lateral-3.png`, `medial-0.png`, and `medial-3.png`.
It records the current source hashes so later measurements can be compared
without attributing them to this source snapshot.

Full independent raster parity against cortex-studio remains unrun. Its
implementation must adopt the endpoint-margin, projection and tie rules
before agreement can be claimed.
