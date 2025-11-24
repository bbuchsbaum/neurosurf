test_that("compute_hull_world returns boundary points and downsamples", {
  sp <- neuroim2::NeuroSpace(c(5, 5, 5))
  arr <- array(0, dim = c(5, 5, 5))
  arr[2:4, 2:4, 2:4] <- 1
  vol <- neuroim2::NeuroVol(arr, sp)

  mask_arr <- array(TRUE, dim = c(5, 5, 5))
  mask_arr[3, 3, 3] <- FALSE
  mask_vol <- neuroim2::NeuroVol(mask_arr, sp)

  hull <- compute_hull_world(vol, mask = mask_vol, n_points = 30L, thresh = 0.5)

  expect_gt(nrow(hull), 0)
  expect_equal(ncol(hull), 3)
  expect_lte(nrow(hull), 30)
})

test_that("estimate_sdf_rigid recovers a simple translation", {
  dims <- c(20, 20, 20)
  sp <- neuroim2::NeuroSpace(dims)
  center <- c(10, 10, 10)
  radius <- 5

  grid <- expand.grid(x = seq_len(dims[1]),
                      y = seq_len(dims[2]),
                      z = seq_len(dims[3]))

  dist_center <- sqrt((grid$x - center[1])^2 +
                        (grid$y - center[2])^2 +
                        (grid$z - center[3])^2) - radius
  sdf_arr <- array(dist_center, dim = dims)
  sdf_vol <- neuroim2::NeuroVol(sdf_arr, sp)

  shift <- c(1.5, -2, 0.5)
  dist_shift <- sqrt((grid$x - (center[1] + shift[1]))^2 +
                       (grid$y - (center[2] + shift[2]))^2 +
                       (grid$z - (center[3] + shift[3]))^2) - radius
  vol_arr <- array(0, dim = dims)
  vol_arr[dist_shift <= 0] <- 1
  vol_shift <- neuroim2::NeuroVol(vol_arr, sp)

  hull <- compute_hull_world(vol_shift, n_points = 1500L, thresh = 0.5)
  fit <- estimate_sdf_rigid(hull,
                            sdf_vol,
                            init_par = rep(0, 6),
                            outside_penalty = 10,
                            method = "L-BFGS-B")

  expect_equal(fit$par[1:3], -shift, tolerance = 0.35)
  expect_lt(max(abs(fit$par[4:6])), 0.2)
})

test_that("vol_to_surf_sdf matches vol_to_surf when alignment is identity", {
  verts_wm <- matrix(c(
    1, 1, 1,
    2, 1, 1,
    2, 2, 1,
    1, 2, 1
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0, 1, 2,
                    0, 2, 3), ncol = 3, byrow = TRUE)

  surf_wm <- SurfaceGeometry(vert = verts_wm, faces = faces, hemi = "lh")
  surf_pial <- SurfaceGeometry(vert = verts_wm + c(0, 0, 0.2),
                               faces = faces, hemi = "lh")

  sp <- neuroim2::NeuroSpace(c(4, 4, 4))
  vol_arr <- array(0, dim = c(4, 4, 4))
  vol_arr[2:3, 2:3, 2:3] <- 5
  vol <- neuroim2::NeuroVol(vol_arr, sp)

  grid <- expand.grid(x = seq_len(4),
                      y = seq_len(4),
                      z = seq_len(4))
  center <- c(2, 2, 2)
  radius <- 1.5
  sdf_vals <- sqrt((grid$x - center[1])^2 +
                     (grid$y - center[2])^2 +
                     (grid$z - center[3])^2) - radius
  sdf_vol <- neuroim2::NeuroVol(array(sdf_vals, dim = c(4, 4, 4)), sp)

  mapped_direct <- vol_to_surf(surf_wm,
                               surf_pial,
                               vol,
                               fun = "nn",
                               knn = 1,
                               dthresh = 5,
                               fill = 0)

  mapped_aligned <- vol_to_surf_sdf(surf_wm,
                                    surf_pial,
                                    vol,
                                    sdf_vol,
                                    n_hull = 500L,
                                    thresh = 0.5,
                                    init_par = rep(0, 6),
                                    outside_penalty = 10,
                                    method = "L-BFGS-B",
                                    fun = "nn",
                                    knn = 1,
                                    dthresh = 5,
                                    fill = 0)

  expect_equal(mapped_direct@data, mapped_aligned@data)
})

test_that("compute_hull_world centroid respects identity grid", {
  sp <- neuroim2::NeuroSpace(c(4, 4, 4))
  arr <- array(0, dim = c(4, 4, 4))
  arr[2:3, 2:3, 2:3] <- 1
  vol <- neuroim2::NeuroVol(arr, sp)

  hull <- compute_hull_world(vol, n_points = 200L, thresh = 0.5)
  ctr <- colMeans(hull)
  expect_equal(ctr, c(2.5, 2.5, 2.5), tolerance = 1e-6)
})

test_that("compute_hull_world honors logical vector mask", {
  sp <- neuroim2::NeuroSpace(c(5, 5, 5))
  arr <- array(0, dim = c(5, 5, 5))
  arr[2:4, 2:4, 2:4] <- 1
  vol <- neuroim2::NeuroVol(arr, sp)

  mask_arr <- array(TRUE, dim = c(5, 5, 5))
  mask_arr[2:3, 2:4, 2:4] <- FALSE  # remove a slab through the cube
  mask_vec <- as.logical(mask_arr)

  hull_full <- compute_hull_world(vol, n_points = 500L, thresh = 0.5)
  hull_masked <- compute_hull_world(vol,
                                    mask = mask_vec,
                                    n_points = 500L,
                                    thresh = 0.5)
  expect_lt(nrow(hull_masked), nrow(hull_full))
})

test_that("estimate_sdf_rigid recovers small yaw rotation", {
  dims <- c(24, 24, 24)
  sp <- neuroim2::NeuroSpace(dims)
  center <- c(12, 12, 12)
  axes <- c(5, 3, 4)  # non-spherical
  theta <- 0.2        # ~11.5 degrees about z

  grid <- expand.grid(x = seq_len(dims[1]),
                      y = seq_len(dims[2]),
                      z = seq_len(dims[3]))

  ellipsoid_fn <- function(x, y, z) {
    ((x - center[1]) / axes[1])^2 +
      ((y - center[2]) / axes[2])^2 +
      ((z - center[3]) / axes[3])^2 - 1
  }

  # SDF of axis-aligned ellipsoid
  sdf_vals <- ellipsoid_fn(grid$x, grid$y, grid$z)
  sdf_vol <- neuroim2::NeuroVol(array(sdf_vals, dim = dims), sp)

  # Rotate coordinates by +theta to create rotated volume
  cos_t <- cos(theta); sin_t <- sin(theta)
  xr <- (grid$x - center[1]) * cos_t - (grid$y - center[2]) * sin_t + center[1]
  yr <- (grid$x - center[1]) * sin_t + (grid$y - center[2]) * cos_t + center[2]
  zr <- grid$z

  rot_inside <- ellipsoid_fn(xr, yr, zr)
  vol_arr <- array(0, dim = dims)
  vol_arr[rot_inside <= 0] <- 1
  vol_rot <- neuroim2::NeuroVol(vol_arr, sp)

  hull <- compute_hull_world(vol_rot, n_points = 2000L, thresh = 0.5)
  fit <- estimate_sdf_rigid(hull,
                            sdf_vol,
                            init_par = rep(0, 6),
                            outside_penalty = 10,
                            method = "L-BFGS-B")

  rz <- fit$par[6]
  expect_equal(rz, theta, tolerance = 0.12)
  expect_lt(max(abs(fit$par[1:2])), 0.6)
})
