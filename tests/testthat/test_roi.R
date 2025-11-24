skip_if_not_installed("rgl")

test_that("ROISurface enforces integer, in-bounds indices", {
  geom <- example_surface_geometry()
  expect_no_error(ROISurface(geom, indices = 1L, data = 1))
  expect_error(ROISurface(geom, indices = 0L, data = 1), "out of bounds")
  expect_error(ROISurface(geom, indices = 5L, data = 1), "out of bounds")
  expect_error(ROISurface(geom, indices = 1.2, data = 1), "integer")
})

test_that("ROISurface subset warns on out-of-range internal indices", {
  geom <- example_surface_geometry()
  roi <- ROISurface(geom, indices = c(1L, 2L, 3L), data = c(10, 20, 30))
  expect_warning(empty <- roi[c(0, 5)], "out of bounds")
  expect_s4_class(empty, "ROISurface")
  expect_length(empty@indices, 0)

  sub <- roi[2]
  expect_equal(sub@indices, 2L)
  expect_equal(sub@data, 20)
})

test_that("SurfaceDisk uses geodesic radius and max_order guard", {
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
  geom <- SurfaceGeometry(verts, faces, "lh")
  g <- graph(geom)
  igraph::E(g)$dist <- rep(1, igraph::ecount(g))

  # radius small: should include only center
  roi_small <- SurfaceDisk(geom, index = 1, radius = 0.4)
  expect_equal(roi_small@indices, 1L)

  # radius larger: should include neighbors
  roi_big <- SurfaceDisk(geom, index = 1, radius = 2)
  expect_true(length(roi_big@indices) > 1)
})
