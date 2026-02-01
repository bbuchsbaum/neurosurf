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

test_that("NeuroSurface arithmetic with numeric works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- c(1, 2, 3, 4)
  ns <- NeuroSurface(geom, seq_len(n_verts), data)

  # Addition
  res_add <- ns + 10
  expect_equal(res_add@data, data + 10)

  # Subtraction
  res_sub <- ns - 1
  expect_equal(res_sub@data, data - 1)

  # Multiplication
  res_mul <- ns * 2
  expect_equal(res_mul@data, data * 2)

  # Division
  res_div <- ns / 2
  expect_equal(res_div@data, data / 2)
})

test_that("numeric arithmetic with NeuroSurface works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- c(1, 2, 3, 4)
  ns <- NeuroSurface(geom, seq_len(n_verts), data)

  # numeric + NeuroSurface
  res_add <- 10 + ns
  expect_equal(res_add@data, 10 + data)

  # numeric - NeuroSurface
  res_sub <- 10 - ns
  expect_equal(res_sub@data, 10 - data)

  # numeric * NeuroSurface
  res_mul <- 2 * ns
  expect_equal(res_mul@data, 2 * data)

  # numeric / NeuroSurface
  res_div <- 12 / ns
  expect_equal(res_div@data, 12 / data)
})

test_that("NeuroSurface comparison with numeric works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- c(1, 2, 3, 4)
  ns <- NeuroSurface(geom, seq_len(n_verts), data)

  # Greater than
  res_gt <- ns > 2
  expect_equal(res_gt@data, as.numeric(data > 2))

  # Less than
  res_lt <- ns < 3
  expect_equal(res_lt@data, as.numeric(data < 3))

  # Equal
  res_eq <- ns == 2
  expect_equal(res_eq@data, as.numeric(data == 2))

  # Greater than or equal
  res_gte <- ns >= 2
  expect_equal(res_gte@data, as.numeric(data >= 2))

  # Less than or equal
  res_lte <- ns <= 3
  expect_equal(res_lte@data, as.numeric(data <= 3))
})

test_that("NeuroSurfaceVector arithmetic with numeric works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(1:8, nrow = n_verts, ncol = 2)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  # Addition
  res_add <- nsv + 10
  expect_equal(as.matrix(res_add@data), mat + 10)

  # Multiplication
  res_mul <- nsv * 2
  expect_equal(as.matrix(res_mul@data), mat * 2)
})

test_that("numeric arithmetic with NeuroSurfaceVector works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(1:8, nrow = n_verts, ncol = 2)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  # numeric + NeuroSurfaceVector
  res_add <- 10 + nsv
  expect_equal(as.matrix(res_add@data), 10 + mat)

  # numeric * NeuroSurfaceVector
  res_mul <- 2 * nsv
  expect_equal(as.matrix(res_mul@data), 2 * mat)
})

test_that("NeuroSurfaceVector comparison with numeric works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(1:8, nrow = n_verts, ncol = 2)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  # Greater than
  res_gt <- nsv > 4
  expect_true(inherits(res_gt, "NeuroSurfaceVector"))
  expected <- matrix(as.numeric(mat > 4), nrow = n_verts, ncol = 2)
  expect_equal(as.matrix(res_gt@data), expected)
})

test_that("numeric comparison with NeuroSurfaceVector works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(1:8, nrow = n_verts, ncol = 2)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  # numeric > NeuroSurfaceVector
  res_gt <- 4 > nsv
  expect_true(inherits(res_gt, "NeuroSurfaceVector"))
})

test_that("NeuroSurface + NeuroSurfaceVector works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- c(1, 2, 3, 4)
  mat <- matrix(1:8, nrow = n_verts, ncol = 2)

  ns <- NeuroSurface(geom, seq_len(n_verts), data)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  # NeuroSurface + NeuroSurfaceVector
  res <- ns + nsv
  expect_s4_class(res, "NeuroSurfaceVector")
})

test_that("NeuroSurfaceVector + NeuroSurface works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- c(1, 2, 3, 4)
  mat <- matrix(1:8, nrow = n_verts, ncol = 2)

  ns <- NeuroSurface(geom, seq_len(n_verts), data)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  # NeuroSurfaceVector + NeuroSurface
  res <- nsv + ns
  expect_s4_class(res, "NeuroSurfaceVector")
})

test_that("NeuroSurfaceVector comparison with NeuroSurfaceVector works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat1 <- matrix(1:8, nrow = n_verts, ncol = 2)
  mat2 <- matrix(c(2, 1, 4, 3, 6, 5, 8, 7), nrow = n_verts, ncol = 2)

  nsv1 <- NeuroSurfaceVector(geom, seq_len(n_verts), mat1)
  nsv2 <- NeuroSurfaceVector(geom, seq_len(n_verts), mat2)

  res <- nsv1 > nsv2
  expect_s4_class(res, "NeuroSurfaceVector")
})

test_that("NeuroSurface comparison with NeuroSurface works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data1 <- c(1, 2, 3, 4)
  data2 <- c(2, 1, 4, 3)

  ns1 <- NeuroSurface(geom, seq_len(n_verts), data1)
  ns2 <- NeuroSurface(geom, seq_len(n_verts), data2)

  res <- ns1 > ns2
  expect_s4_class(res, "NeuroSurface")
  expect_equal(res@data, as.numeric(data1 > data2))
})
