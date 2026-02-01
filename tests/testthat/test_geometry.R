# Tests for geometry.R functions and methods

# Helper to create a simple test geometry
make_test_geom <- function() {
  verts <- matrix(c(
    0, 0, 0,
    1, 0, 0,
    0, 1, 0,
    0, 0, 1
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(
    0L, 1L, 2L,
    0L, 1L, 3L,
    0L, 2L, 3L,
    1L, 2L, 3L
  ), ncol = 3, byrow = TRUE)
  SurfaceGeometry(verts, faces, hemi = "lh")
}

test_that("SurfaceGeometry constructor works with valid inputs", {
  geom <- make_test_geom()
  expect_s4_class(geom, "SurfaceGeometry")
  expect_equal(geom@hemi, "lh")
})

test_that("SurfaceGeometry constructor accepts surf_to_world parameter", {
  verts <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3, byrow = TRUE)

  custom_xform <- diag(4)
  custom_xform[1, 4] <- 10  # translation in x

  geom <- SurfaceGeometry(verts, faces, hemi = "rh", surf_to_world = custom_xform)
  expect_equal(surf_to_world(geom), custom_xform)
})

test_that("coords() returns vertex coordinates", {
  geom <- make_test_geom()
  crds <- coords(geom)

  expect_true(is.matrix(crds))
  expect_equal(ncol(crds), 3)
  expect_equal(nrow(crds), 4)
  expect_equal(crds[1, ], c(0, 0, 0))
  expect_equal(crds[2, ], c(1, 0, 0))
})

test_that("vertices() returns subset of vertex coordinates", {
  geom <- make_test_geom()

  # Get vertices 1 and 3
  verts <- vertices(geom, c(1, 3))
  expect_true(is.matrix(verts))
  expect_equal(nrow(verts), 2)
  expect_equal(ncol(verts), 3)
  expect_equal(verts[1, ], c(0, 0, 0))
  expect_equal(verts[2, ], c(0, 1, 0))
})

test_that("faces() returns face indices", {
  geom <- make_test_geom()
  f <- faces(geom)

  expect_true(is.matrix(f))
  expect_equal(ncol(f), 3)
  expect_equal(nrow(f), 4)
  # faces() returns 1-based indices (transposed from mesh$it)
  expect_true(all(f >= 1))
})

test_that("nodes() returns sequence of node numbers", {
  geom <- make_test_geom()
  n <- nodes(geom)

  expect_equal(n, 1:4)
})

test_that("graph() returns igraph object", {
  geom <- make_test_geom()
  g <- graph(geom)

  expect_s3_class(g, "igraph")
  expect_equal(igraph::vcount(g), 4)
})

test_that("surf_to_world getter returns 4x4 matrix", {
  geom <- make_test_geom()
  xform <- surf_to_world(geom)

  expect_true(is.matrix(xform))
  expect_equal(dim(xform), c(4, 4))
  expect_equal(xform, diag(4))  # default is identity
})

test_that("surf_to_world setter updates transform", {
  geom <- make_test_geom()

  new_xform <- diag(4)
  new_xform[1:3, 4] <- c(10, 20, 30)  # translation

  surf_to_world(geom) <- new_xform
  expect_equal(surf_to_world(geom), new_xform)
})

test_that("surf_to_world setter rejects invalid matrices", {
  geom <- make_test_geom()

  expect_error(surf_to_world(geom) <- matrix(1:9, 3, 3), "4x4 matrix")
  # Non-matrix input causes method dispatch failure
  expect_error(surf_to_world(geom) <- 1:16)
})

test_that("read_surf_geometry loads FreeSurfer ASCII surface", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(!file.exists(surf_file), "Test data not available")

  geom <- read_surf_geometry(surf_file)

  expect_s4_class(geom, "SurfaceGeometry")
  expect_true(length(nodes(geom)) > 0)
  expect_true(nrow(faces(geom)) > 0)
})

test_that("example_surface_geometry returns valid geometry", {
  geom <- example_surface_geometry()

  expect_s4_class(geom, "SurfaceGeometry")
  expect_equal(length(nodes(geom)), 4)
  expect_equal(nrow(faces(geom)), 4)
})

test_that("SurfaceGeometry show method works", {
  geom <- make_test_geom()
  expect_output(show(geom), "SurfaceGeometry")
})

test_that("coords on igraph extracts vertex attributes", {
  geom <- make_test_geom()
  g <- graph(geom)

  # coords method for igraph
  crds <- coords(g)
  expect_true(is.matrix(crds))
  expect_equal(ncol(crds), 3)
})
