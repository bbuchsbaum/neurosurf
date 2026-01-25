# Test faces() API coverage across surface types

skip_if_not_installed("rgl")

# Helper to create a simple tetrahedron geometry
make_tetra_geometry <- function(hemi = "lh") {
  verts <- matrix(c(0, 0, 0,
                    1, 0, 0,
                    0, 1, 0,
                    0, 0, 1), ncol = 3, byrow = TRUE)
  # 0-based input as expected by SurfaceGeometry constructor

faces <- matrix(c(0L, 1L, 2L,
                    0L, 1L, 3L,
                    0L, 2L, 3L,
                    1L, 2L, 3L), ncol = 3, byrow = TRUE)
  SurfaceGeometry(verts, faces, hemi = hemi)
}

test_that("faces(SurfaceGeometry) returns matrix with 3 columns and 1-based indices", {
  geom <- make_tetra_geometry()
  f <- faces(geom)

  expect_true(is.matrix(f))
  expect_equal(ncol(f), 3)
  expect_equal(nrow(f), 4)  # tetrahedron has 4 faces

  # Indices must be 1-based (min >= 1)
  expect_true(min(f) >= 1)
  # Max index should not exceed vertex count
  expect_true(max(f) <= 4)
})

test_that("faces(NeuroSurface) delegates to geometry and matches faces(geometry)", {
  geom <- make_tetra_geometry()
  idx <- as.integer(1:4)
  ns <- NeuroSurface(geom, indices = idx, data = as.numeric(idx))

  f_ns <- faces(ns)
  f_geom <- faces(geom)

  expect_identical(f_ns, f_geom)
  expect_equal(ncol(f_ns), 3)
})

test_that("faces(NeuroSurfaceVector) delegates to geometry", {
  geom <- make_tetra_geometry()
  idx <- as.integer(1:4)
  mat <- matrix(1:8, nrow = 4, ncol = 2)
  nsv <- NeuroSurfaceVector(geom, indices = idx, mat = mat)

  f_nsv <- faces(nsv)
  f_geom <- faces(geom)

  expect_identical(f_nsv, f_geom)
  expect_equal(ncol(f_nsv), 3)
})

test_that("faces(SurfaceSet) dispatches to default geometry", {
  g1 <- make_tetra_geometry("lh")
  # Create a second geometry with same structure but slightly different vertices
  verts2 <- matrix(c(0.1, 0, 0,
                     1.1, 0, 0,
                     0.1, 1, 0,
                     0.1, 0, 1), ncol = 3, byrow = TRUE)
  faces_mat <- matrix(c(0L, 1L, 2L,
                        0L, 1L, 3L,
                        0L, 2L, 3L,
                        1L, 2L, 3L), ncol = 3, byrow = TRUE)
  g2 <- SurfaceGeometry(verts2, faces_mat, hemi = "lh", label = "inflated")

  ss <- surface_set(pial = g1, inflated = g2, default_label = "pial")

  f_ss <- faces(ss)
  f_default <- faces(get_surface(ss))

  expect_identical(f_ss, f_default)
  expect_equal(ncol(f_ss), 3)
})

test_that("faces() returns consistent 1-based indices across all surface types", {
  geom <- make_tetra_geometry()
  idx <- as.integer(1:4)

  ns <- NeuroSurface(geom, indices = idx, data = as.numeric(idx))
  nsv <- NeuroSurfaceVector(geom, indices = idx, mat = matrix(1:4, ncol = 1))

  # All should return identical face matrices
  expect_identical(faces(geom), faces(ns))
  expect_identical(faces(geom), faces(nsv))

  # All should be 1-based
  for (obj in list(geom, ns, nsv)) {
    f <- faces(obj)
    expect_true(min(f) >= 1, info = paste("1-based check for", class(obj)[1]))
  }
})
