# SDF-Based vol→surf Alignment & Mapping — Working Notes

Source idea: keep the signed-distance field (SDF) target, align via a
small rigid transform, and re-use neurosurf/neuroim2 for geometry while
moving hot loops to Rcpp (optionally roptim).

## What’s already strong

- Geometry-first: align a surface SDF to the volume hull; avoids per-run
  ANTs-style registration.
- Cacheable: ship precomputed SDF (e.g., fsaverage) as `inst/extdata`
  NeuroVol; optional gradient vols.
- Sparse source: reduce volume to a boundary point cloud (~2k pts) for
  speed.
- Normal-aware sampling is conceptually right for thin cortex + voxel
  blocks.

## Refinements to make

1.  Use SDF gradients (currently Powell + distance-only).  
2.  Smarter masking: boundary voxels from a mask/Otsu, not all
    above-median voxels.  
3.  Lean on neuroim2 geometry: `trans()` / `inverse_trans()` for
    world↔︎grid.  
4.  Actually sample along normals in mapping step.  
5.  Default to rigid (±isotropic scale); affine/FFD only as an optional
    heavy mode.

## Data structures

- Volumes: `NeuroVol` + `NeuroSpace` (neuroim2).
- Surfaces: `SurfaceGeometry` (coords, faces, normals).
- SDF cache object: `list(sdf, grad_x, grad_y, grad_z, surf_space)`.

## Proposed R API sketch

``` r

fsavg_sdf <- load_fsaverage_sdf()
T_vol2surf <- vol2surf_transform(vol, fsavg_sdf$sdf, n_points = 2000, rigid = TRUE, use_roptim = TRUE)
surf_vals  <- vol2surf_sample(vol, fsavg_surf, T = T_vol2surf, delta = 2, n_steps = 5, summary = "gaussian")
```

## Pipeline pieces

### 1) Hull extraction (C++: `compute_hull_world_cpp`)

- Inputs: vol, optional mask, `grid2world = trans(space(vol))`,
  `n_points`, `thresh` (median non-zero if NULL).
- Steps: build inside mask (thresh + optional mask) → mark boundary
  voxels (6-neighbor) → convert (i,j,k)→world → deterministic downsample
  to ~`n_points`.

### 2) Rigid SDF alignment (C++: `estimate_sdf_rigid_cpp` with roptim)

- Params p = (tx,ty,tz,rx,ry,rz) in mm/rad; optional isotropic scale
  later.
- Loss: mean of squared SDF values after transforming hull pts:  
  `q = T(p) * hull_pt`, `g = world2grid_sdf * q`, `d = SDF(g)`,
  `L = mean(d^2)` (Huber/clipping optional).
- Gradient: cheap finite differences in C++ (6–7 params).
- Init: COM(diff) translation; rotations start at 0; PCA optional.
- Returns: optimized params, 4×4 `T` (vol→surf); also use `solve(T)` for
  surf→vol.

### 3) Mapping step (re-use vs. new)

- Quick path: transform template surfaces into volume space with
  `T_inv`; call existing
  [`vol_to_surf()`](https://bbuchsbaum.github.io/neurosurf/reference/vol_to_surf.md)
  (FNN knn + Gaussian weights).
- Faster/normal-aware path (new C++): per-vertex sampling along ±δ·n in
  volume space with Gaussian weights
  (`sample_volume_along_normals_cpp`), using
  `world2grid_vol = inverse_trans(space(vol))`.

### 4) Optional FFD mode

- Coarse 3D B-spline grid (e.g., 3³ or 5³ ctrl pts → 375 params); same
  SDF loss; multi-res if needed. Heavy/opt-in only.

## New R wrappers to add

- `compute_hull_world()` → thin wrapper over C++ hull extractor.
- `estimate_sdf_rigid()` → thin wrapper over roptim functor.
- `transform_surface_geometry()` → apply 4×4 to vertex coords.
- [`vol_to_surf_sdf()`](https://bbuchsbaum.github.io/neurosurf/reference/vol_to_surf_sdf.md)
  → high-level: hull → rigid align → transform surfaces → call existing
  [`vol_to_surf()`](https://bbuchsbaum.github.io/neurosurf/reference/vol_to_surf.md);
  no mapping rewrite required.

## Performance notes

- Precompute SDF once; 2 mm grid over fsaverage/MNI-sized box is fine.
- Hull pass over ~10⁶ voxels is trivial in C++; downsample to ~2k pts.
- Optimizer: 6–7 params, ~2k pts, finite-diff BFGS → tens of iterations
  (ms–s).
- For 4D, fit T on one volume (mean/first) and reuse for all timepoints.
- Cache reusable geometry: `T`, `T_inv`, `verts`, `normals`,
  `world2grid_vol`.

## Next implementation steps (if/when)

- Finalize
  [`compute_hull_world_cpp()`](https://bbuchsbaum.github.io/neurosurf/reference/compute_hull_world_cpp.md)
  with correct 0/1-based index convention for neuroim2.
- Add `estimate_sdf_rigid_cpp` (with trilinear sampler + roptim) to
  src/.
- Wire
  [`vol_to_surf_sdf()`](https://bbuchsbaum.github.io/neurosurf/reference/vol_to_surf_sdf.md)
  and expose a precomputed fsaverage SDF dataset.
- Optional: C++ normal-sampling mapper for a parallel
  `vol_to_surf_normals_sdf()`.
