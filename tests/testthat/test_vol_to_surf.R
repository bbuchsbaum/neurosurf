test_that("vol_to_surf respects mask when selecting voxels", {
  skip_if_not_installed("rgl")

  sp   <- neuroim2::NeuroSpace(c(4, 1, 1), c(1, 1, 1), c(0, 0, 0))
  vol  <- neuroim2::NeuroVol(c(1, 2, 3, 4), sp)
  mask <- array(c(FALSE, TRUE, TRUE, FALSE), dim = c(4, 1, 1))

  # Use voxel centers as surface vertices so distances are simple.
  verts <- neuroim2::index_to_coord(vol, 1:4)
  faces <- matrix(c(0L, 1L, 2L,
                    1L, 2L, 3L), nrow = 2, byrow = TRUE)

  surf_wm   <- SurfaceGeometry(verts, faces, hemi = "lh")
  surf_pial <- SurfaceGeometry(verts, faces, hemi = "lh")

  res <- vol_to_surf(surf_wm, surf_pial, vol,
                     mask = mask, knn = 1, dthresh = 2, fill = NA_real_)

  expect_equal(as.vector(res@data), c(2, 2, 3, 3))
})


test_that("vol_to_surf drops neighbors beyond dthresh and uses fill value", {
  skip_if_not_installed("rgl")

  sp  <- neuroim2::NeuroSpace(c(2, 1, 1), c(1, 1, 1), c(0, 0, 0))
  vol <- neuroim2::NeuroVol(c(1, 2), sp)

  # Place surface far from the only voxel so distance exceeds dthresh.
  verts_far <- matrix(rep(10, 9), ncol = 3, byrow = TRUE)  # three identical far vertices
  faces     <- matrix(c(0L, 1L, 2L,
                        0L, 2L, 1L), nrow = 2, byrow = TRUE)

  surf_wm   <- SurfaceGeometry(verts_far, faces, hemi = "lh")
  surf_pial <- SurfaceGeometry(verts_far, faces, hemi = "lh")

  res <- vol_to_surf(surf_wm, surf_pial, vol,
                     knn = 1, dthresh = 0.5, fill = NA_real_)

  expect_true(all(is.na(res@data)))
})

test_that("vol_to_surf supports thickness sampling", {
  skip_if_not_installed("rgl")

  sp <- neuroim2::NeuroSpace(c(3, 1, 1), c(1, 1, 1), c(0, 0, 0))
  vol <- neuroim2::NeuroVol(c(10, 20, 30), sp)

  verts_wm <- matrix(c(
    1, 0, 0,
    1, 1, 0,
    1, 0, 1
  ), ncol = 3, byrow = TRUE)
  verts_pial <- verts_wm + c(2, 0, 0)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3, byrow = TRUE)

  surf_wm <- SurfaceGeometry(verts_wm, faces, hemi = "lh")
  surf_pial <- SurfaceGeometry(verts_pial, faces, hemi = "lh")

  res <- vol_to_surf(surf_wm, surf_pial, vol,
                     sampling = "thickness",
                     depth = c(0, 1),
                     fun = "avg",
                     dthresh = 2,
                     sigma = 1)

  expect_length(res@data, nrow(verts_wm))
  expect_true(all(res@data >= 10 & res@data <= 30))
})

test_that("vol_to_surf supports normal_line sampling", {
  skip_if_not_installed("rgl")

  sp <- neuroim2::NeuroSpace(c(3, 3, 3), c(1, 1, 1), c(0, 0, 0))
  vol_arr <- array(5, dim = c(3, 3, 3))
  vol_arr[2, 2, 2] <- 9
  vol <- neuroim2::NeuroVol(vol_arr, sp)

  verts <- matrix(c(
    2, 2, 2,
    2, 3, 2,
    2, 2, 3
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3, byrow = TRUE)
  surf <- SurfaceGeometry(verts, faces, hemi = "lh")

  res <- vol_to_surf(surf, surf, vol,
                     sampling = "normal_line",
                     n_samples = 3,
                     radius = 1,
                     fun = "nn",
                     dthresh = 2)

  expect_length(res@data, nrow(verts))
  expect_true(all(!is.na(res@data)))
})

test_that("linear interpolation recovers an analytic 3-D field", {
  skip_if_not_installed("rgl")

  sp <- neuroim2::NeuroSpace(c(6, 6, 6), c(1, 1, 1), c(0, 0, 0))
  template <- neuroim2::NeuroVol(array(0, c(6, 6, 6)), sp)
  coord <- neuroim2::index_to_coord(template, seq_len(length(template)))
  field <- 2 * coord[, 1] - 3 * coord[, 2] + 0.5 * coord[, 3] + 7
  vol <- neuroim2::NeuroVol(array(field, c(6, 6, 6)), sp)

  verts <- matrix(c(
    1.25, 1.75, 2.20,
    2.40, 2.10, 1.60,
    3.15, 1.35, 3.40
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3)
  surf <- SurfaceGeometry(verts, faces, hemi = "lh")

  res <- vol_to_surf(
    surf, surf, vol,
    interpolation = "linear",
    sampling = "midpoint",
    aggregate = "mean",
    fill = NA_real_
  )
  expected <- 2 * verts[, 1] - 3 * verts[, 2] + 0.5 * verts[, 3] + 7
  expect_equal(as.vector(res@data), expected, tolerance = 1e-10)
})

test_that("explicit interpolation preserves zeros and mask boundaries", {
  skip_if_not_installed("rgl")

  sp <- neuroim2::NeuroSpace(c(3, 3, 3), c(1, 1, 1), c(0, 0, 0))
  vol <- neuroim2::NeuroVol(array(0, c(3, 3, 3)), sp)
  verts <- matrix(c(
    0.0, 0.0, 0.0,
    0.5, 0.0, 0.0,
    1.0, 0.0, 0.0
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3)
  surf <- SurfaceGeometry(verts, faces, hemi = "lh")

  nearest <- vol_to_surf(
    surf, surf, vol, interpolation = "nearest", fill = NA_real_
  )
  expect_equal(as.vector(nearest@data), c(0, 0, 0))

  mask <- array(TRUE, c(3, 3, 3))
  mask[2, 1, 1] <- FALSE
  strict <- vol_to_surf(
    surf, surf, vol, mask = mask, interpolation = "linear",
    fill = NA_real_, na_rm = FALSE
  )
  expect_equal(strict@data[[1]], 0)
  expect_true(is.na(strict@data[[2]]))
  expect_true(is.na(strict@data[[3]]))
})

test_that("categorical mode remains a nearest-grid operation", {
  skip_if_not_installed("rgl")
  sp <- neuroim2::NeuroSpace(c(3, 1, 1), c(1, 1, 1), c(0, 0, 0))
  vol <- neuroim2::NeuroVol(c(1, 2, 2), sp)
  verts <- matrix(c(0, 0, 0, 1, 0, 0, 2, 0, 0),
                  ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), nrow = 1)
  surf <- SurfaceGeometry(verts, faces, hemi = "lh")
  categorical <- vol_to_surf(
    surf, surf, vol, interpolation = "nearest", aggregate = "mode"
  )
  expect_equal(as.vector(categorical@data), c(1, 2, 2))
})

test_that("linear thickness projection has explicit aggregation semantics", {
  skip_if_not_installed("rgl")

  sp <- neuroim2::NeuroSpace(c(5, 3, 3), c(1, 1, 1), c(0, 0, 0))
  template <- neuroim2::NeuroVol(array(0, c(5, 3, 3)), sp)
  coord <- neuroim2::index_to_coord(template, seq_len(length(template)))
  vol <- neuroim2::NeuroVol(array(coord[, 1], c(5, 3, 3)), sp)
  wm_v <- matrix(c(0, 0, 0, 0, 1, 0, 0, 0, 1),
                 ncol = 3, byrow = TRUE)
  pial_v <- wm_v + matrix(rep(c(4, 0, 0), 3), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3)
  wm <- SurfaceGeometry(wm_v, faces, hemi = "lh")
  pial <- SurfaceGeometry(pial_v, faces, hemi = "lh")

  res <- vol_to_surf(
    wm, pial, vol,
    sampling = "thickness",
    interpolation = "linear",
    aggregate = "mean",
    fill = NA_real_
  )
  expect_equal(as.vector(res@data), rep(2, 3), tolerance = 1e-10)
  expect_error(
    vol_to_surf(wm, pial, vol, sampling = "thickness",
                interpolation = "linear", aggregate = "mode"),
    "invalid with linear"
  )
  smoothed <- vol_to_surf(
    wm, pial, vol, interpolation = "linear", sampling = "thickness",
    surface_smooth_fwhm = 2
  )
  expect_length(smoothed@data, 3)
})

test_that("explicit legacy interpolation is exactly backward compatible", {
  skip_if_not_installed("rgl")

  sp <- neuroim2::NeuroSpace(c(4, 2, 2), c(1, 1, 1), c(0, 0, 0))
  vol <- neuroim2::NeuroVol(array(seq_len(16), c(4, 2, 2)), sp)
  verts <- matrix(c(
    0.5, 0.5, 0.5,
    1.5, 0.5, 0.5,
    2.5, 0.5, 0.5
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3)
  surf <- SurfaceGeometry(verts, faces, hemi = "lh")

  old <- vol_to_surf(surf, surf, vol, fun = "avg", knn = 3, sigma = 2)
  explicit <- vol_to_surf(
    surf, surf, vol, fun = "avg", knn = 3, sigma = 2,
    interpolation = "legacy"
  )
  expect_identical(as.vector(explicit@data), as.vector(old@data))
})

test_that("tangential smoothing is separate, millimetre-scaled, and opt-in", {
  skip_if_not_installed("rgl")
  sp <- neuroim2::NeuroSpace(c(4, 2, 2), c(1, 1, 1), c(0, 0, 0))
  vol <- neuroim2::NeuroVol(array(c(0, 10, 0, 0, rep(0, 12)), c(4, 2, 2)), sp)
  verts <- matrix(c(0, 0, 0,
                    1, 0, 0,
                    2, 0, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3)
  surf <- SurfaceGeometry(verts, faces, hemi = "lh")
  raw <- vol_to_surf(surf, surf, vol, interpolation = "nearest",
                     surface_smooth_fwhm = 0)
  smooth <- vol_to_surf(surf, surf, vol, interpolation = "nearest",
                        surface_smooth_fwhm = 2)
  expect_equal(raw@data, c(0, 10, 0))
  expect_lt(max(smooth@data), max(raw@data))
  expect_gt(smooth@data[[1]], raw@data[[1]])
})

test_that("surface_sampler caches neighbor lookups", {
  skip_if_not_installed("rgl")

  sp <- neuroim2::NeuroSpace(c(3, 1, 1), c(1, 1, 1), c(0, 0, 0))
  vol1 <- neuroim2::NeuroVol(c(1, 2, 3), sp)
  vol2 <- neuroim2::NeuroVol(c(3, 2, 1), sp)

  verts_wm <- matrix(c(
    1, 0, 0,
    2, 0, 0,
    3, 0, 0
  ), ncol = 3, byrow = TRUE)
  verts_pial <- verts_wm
  faces <- matrix(c(0L, 1L, 2L), ncol = 3, byrow = TRUE)

  surf <- SurfaceGeometry(verts_wm, faces, hemi = "lh")

  sampler <- surface_sampler(surf, surf, vol1,
                             sampling = "midpoint",
                             knn = 2,
                             dthresh = 2)

  res1 <- apply_surface_sampler(sampler, vol1, fun = "nn", fill = NA_real_)
  res2 <- apply_surface_sampler(sampler, vol2, fun = "nn", fill = NA_real_)

  ref1 <- vol_to_surf(surf, surf, vol1, fun = "nn", knn = 2, dthresh = 2)
  ref2 <- vol_to_surf(surf, surf, vol2, fun = "nn", knn = 2, dthresh = 2)

  expect_equal(as.vector(res1@data), as.vector(ref1@data))
  expect_equal(as.vector(res2@data), as.vector(ref2@data))
})

test_that("sampler_to_triplets extracts valid triplets", {
  skip_if_not_installed("rgl")

  sp <- neuroim2::NeuroSpace(c(3, 1, 1), c(1, 1, 1), c(0, 0, 0))
  vol <- neuroim2::NeuroVol(c(1, 2, 3), sp)

  verts_wm <- matrix(c(
    1, 0, 0,
    2, 0, 0,
    3, 0, 0
  ), ncol = 3, byrow = TRUE)
  verts_pial <- verts_wm
  faces <- matrix(c(0L, 1L, 2L), ncol = 3, byrow = TRUE)

  surf <- SurfaceGeometry(verts_wm, faces, hemi = "lh")

  sampler <- surface_sampler(surf, surf, vol,
                             sampling = "midpoint",
                             knn = 2,
                             dthresh = 2)

  triplets <- sampler_to_triplets(sampler, sigma = 1)

  expect_s3_class(triplets, "vol2surf_triplets")
  expect_equal(triplets$n_vertices, 3)
  expect_equal(triplets$n_voxels, 3)
  expect_true(triplets$nnz > 0)
  expect_equal(length(triplets$i), triplets$nnz)
  expect_equal(length(triplets$j), triplets$nnz)
  expect_equal(length(triplets$x), triplets$nnz)
  expect_true(all(triplets$i >= 1 & triplets$i <= triplets$n_vertices))
  expect_true(all(triplets$j >= 1 & triplets$j <= triplets$n_voxels))
  expect_true(all(triplets$x > 0))
})

test_that("sampler_to_triplets weights sum to 1 when normalized", {
  skip_if_not_installed("rgl")

  sp <- neuroim2::NeuroSpace(c(5, 5, 5), c(1, 1, 1), c(0, 0, 0))
  vol_arr <- array(1, dim = c(5, 5, 5))
  vol <- neuroim2::NeuroVol(vol_arr, sp)

  verts <- matrix(c(
    2, 2, 2,
    3, 3, 3,
    2, 3, 2
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3, byrow = TRUE)

  surf <- SurfaceGeometry(verts, faces, hemi = "lh")

  sampler <- surface_sampler(surf, surf, vol,
                             sampling = "midpoint",
                             knn = 6,
                             dthresh = 4)

  triplets <- sampler_to_triplets(sampler, sigma = 2, normalize = TRUE)

  # Check weights sum to 1 for each vertex
  for (v in unique(triplets$i)) {
    wts <- triplets$x[triplets$i == v]
    expect_equal(sum(wts), 1, tolerance = 1e-10)
  }
})

test_that("sampler_to_triplets can build sparse matrix", {
  skip_if_not_installed("rgl")
  skip_if_not_installed("Matrix")

  sp <- neuroim2::NeuroSpace(c(3, 1, 1), c(1, 1, 1), c(0, 0, 0))
  vol <- neuroim2::NeuroVol(c(10, 20, 30), sp)

  verts <- matrix(c(
    1, 0, 0,
    2, 0, 0,
    3, 0, 0
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3, byrow = TRUE)

  surf <- SurfaceGeometry(verts, faces, hemi = "lh")

  sampler <- surface_sampler(surf, surf, vol,
                             sampling = "midpoint",
                             knn = 2,
                             dthresh = 2)

  triplets <- sampler_to_triplets(sampler, sigma = 1)

  # Build sparse matrix from triplets
  P <- Matrix::sparseMatrix(
    i = triplets$i,
    j = triplets$j,
    x = triplets$x,
    dims = triplets$dims
  )

  expect_s4_class(P, "dgCMatrix")
  expect_equal(dim(P), triplets$dims)

  # Apply projector to volume values
  vol_vals <- vol[triplets$voxel_indices]
  result <- as.vector(P %*% vol_vals)

  # Compare with apply_surface_sampler
  ref <- apply_surface_sampler(sampler, vol, fun = "avg", sigma = 1)

  expect_equal(result, as.vector(ref@data), tolerance = 1e-6)
})

test_that("print.vol2surf_triplets works", {
  skip_if_not_installed("rgl")

  sp <- neuroim2::NeuroSpace(c(3, 1, 1), c(1, 1, 1), c(0, 0, 0))
  vol <- neuroim2::NeuroVol(c(1, 2, 3), sp)

  verts <- matrix(c(1, 0, 0, 2, 0, 0, 3, 0, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3, byrow = TRUE)
  surf <- SurfaceGeometry(verts, faces, hemi = "lh")

  sampler <- surface_sampler(surf, surf, vol,
                             sampling = "midpoint",
                             knn = 2,
                             dthresh = 2)

  triplets <- sampler_to_triplets(sampler)

  expect_output(print(triplets), "vol2surf_triplets object")
  expect_output(print(triplets), "Vertices:")
  expect_output(print(triplets), "Non-zeros:")
})
