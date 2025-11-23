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
