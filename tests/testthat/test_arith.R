skip_if_not_installed("rgl")

test_that("Compare keeps logical and enforces geometry/indices", {
  verts <- matrix(c(
    0, 0, 0,
    1, 0, 0,
    0, 1, 0
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3, byrow = TRUE)

  g1 <- SurfaceGeometry(verts, faces, hemi = "lh")
  g2 <- SurfaceGeometry(verts, faces, hemi = "lh")

  s1 <- NeuroSurface(g1, indices = 1:3, data = c(1, 2, 3))
  s2 <- NeuroSurface(g2, indices = 1:3, data = c(1, 2, 0))

  res <- s1 > s2
  expect_true(is.numeric(res@data))
  expect_equal(res@indices, 1:3)

  s3 <- NeuroSurface(g1, indices = 1:2, data = c(1, 2))
  expect_error(s1 + s3, "indices differ")
})

test_that("Arith requires matching geometry and indices", {
  verts <- matrix(c(
    0, 0, 0,
    1, 0, 0,
    0, 1, 0
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3, byrow = TRUE)

  g1 <- SurfaceGeometry(verts, faces, hemi = "lh")
  g2 <- SurfaceGeometry(verts + 1, faces, hemi = "lh")  # different coords

  s1 <- NeuroSurface(g1, indices = 1:3, data = c(1, 2, 3))
  s2 <- NeuroSurface(g2, indices = 1:3, data = c(4, 5, 6))

  expect_error(s1 + s2, "geometries differ")

  v1 <- Matrix::Matrix(cbind(1:3, 4:6))
  v2 <- Matrix::Matrix(cbind(6:4, 3:1))
  sv1 <- NeuroSurfaceVector(g1, indices = 1:3, mat = v1)
  sv2 <- NeuroSurfaceVector(g1, indices = 1:3, mat = v2)

  res <- sv1 + sv2
  expect_equal(res@indices, 1:3)
  expect_equal(as.matrix(res@data), as.matrix(v1 + v2))
})
