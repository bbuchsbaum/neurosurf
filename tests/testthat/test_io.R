test_that("write_surf_data writes expected rows/cols", {
  geom <- example_surface_geometry()
  idx <- c(1L, 3L)
  vals <- c(10, 20)
  surf <- NeuroSurface(geometry = geom, indices = idx, data = vals)

  outstem <- tempfile("surfdata")
  fname <- paste0(outstem, ".1D.dset")
  on.exit(unlink(fname), add = TRUE)

  write_surf_data(surf, outstem = outstem, hemi = "")
  tab <- read.table(fname, header = FALSE)
  expect_equal(nrow(tab), length(idx))
  expect_equal(tab[, 1], idx - 1)
  expect_equal(tab[, 2], vals)
})

test_that("write_surf_data handles NeuroSurfaceVector", {
  geom <- example_surface_geometry()
  idx <- 1:4
  mat <- matrix(1:8, nrow = 4, ncol = 2)
  surfv <- NeuroSurfaceVector(geometry = geom, indices = idx, mat = mat)

  outstem <- tempfile("surfvec")
  fname <- paste0(outstem, "_lh.1D.dset")
  on.exit(unlink(fname), add = TRUE)

  write_surf_data(surfv, outstem = outstem, hemi = "lh")
  tab <- read.table(fname, header = FALSE)
  expect_equal(nrow(tab), length(idx))
  expect_equal(tab[, 1], idx - 1)
  expect_equal(unname(as.matrix(tab[, -1])), unname(mat))
})
