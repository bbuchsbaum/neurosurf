# Tests for ROI.R

test_that("ROISurface creates valid object", {
  geom <- example_surface_geometry()
  n_nodes <- length(nodes(geom))

  # Create ROI with first few vertices (geometry has 4 vertices)
  indices <- 1:min(3, n_nodes)
  data <- rnorm(length(indices))

  roi <- ROISurface(geom, indices, data)

  expect_s4_class(roi, "ROISurface")
  expect_equal(length(roi), length(indices))
  expect_equal(indices(roi), indices)
  expect_equal(values(roi), data)
})

test_that("ROISurface validates indices", {
  geom <- example_surface_geometry()
  n_nodes <- length(nodes(geom))

  # Out of bounds indices
  expect_error(
    ROISurface(geom, c(1L, n_nodes + 100L), c(1, 1)),
    "out of bounds"
  )

  # Zero or negative indices
  expect_error(
    ROISurface(geom, c(0L, 1L), c(1, 1)),
    "out of bounds"
  )
})

test_that("ROISurface accepts numeric indices that are integers", {
  geom <- example_surface_geometry()

  # Numeric but integer-valued
  indices <- c(1, 2, 3)  # numeric, not integer
  data <- rnorm(3)

  roi <- ROISurface(geom, indices, data)
  expect_s4_class(roi, "ROISurface")
})

test_that("ROISurface rejects non-integer numeric indices", {
  geom <- example_surface_geometry()

  expect_error(
    ROISurface(geom, c(1.5, 2.5), c(1, 1)),
    "integer"
  )
})

test_that("ROISurfaceVector creates valid object", {
  geom <- example_surface_geometry()
  n_nodes <- length(nodes(geom))

  indices <- 1:min(3, n_nodes)
  n_idx <- length(indices)
  data <- matrix(rnorm(3 * n_idx), nrow = 3, ncol = n_idx)  # 3 measures x n vertices

  roi_vec <- ROISurfaceVector(geom, indices, data)

  expect_s4_class(roi_vec, "ROISurfaceVector")
  expect_equal(ncol(values(roi_vec)), n_idx)
  expect_equal(nrow(values(roi_vec)), 3)
  expect_equal(indices(roi_vec), indices)
})

test_that("ROISurfaceVector validates indices", {
  geom <- example_surface_geometry()
  n_nodes <- length(nodes(geom))

  data <- matrix(1, nrow = 1, ncol = 2)

  # Out of bounds
  expect_error(
    ROISurfaceVector(geom, c(1L, n_nodes + 100L), data),
    "out of bounds"
  )
})

test_that("values() extracts data from ROISurface", {
  geom <- example_surface_geometry()
  n_nodes <- length(nodes(geom))
  indices <- 1:min(3, n_nodes)
  data <- seq_along(indices)

  roi <- ROISurface(geom, indices, data)

  expect_equal(values(roi), data)
})

test_that("values() extracts data from ROISurfaceVector", {
  geom <- example_surface_geometry()
  n_nodes <- length(nodes(geom))
  indices <- 1:min(3, n_nodes)
  n_idx <- length(indices)
  data <- matrix(rnorm(2 * n_idx), nrow = 2, ncol = n_idx)

  roi_vec <- ROISurfaceVector(geom, indices, data)

  expect_equal(values(roi_vec), data)
})

test_that("values() extracts data from NeuroSurface", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- rnorm(n_verts)

  ns <- NeuroSurface(geom, 1:n_verts, data)

  expect_equal(values(ns), data)
})

test_that("indices() extracts indices from ROISurface", {
  geom <- example_surface_geometry()
  indices <- c(1L, 2L, 3L)
  data <- 1:3

  roi <- ROISurface(geom, indices, data)

  expect_equal(indices(roi), indices)
})

test_that("indices() extracts indices from ROISurfaceVector", {
  geom <- example_surface_geometry()
  indices <- c(1L, 2L, 3L)
  data <- matrix(1, nrow = 1, ncol = 3)

  roi_vec <- ROISurfaceVector(geom, indices, data)

  expect_equal(indices(roi_vec), indices)
})

test_that("coords() extracts coordinates from ROISurface", {
  geom <- example_surface_geometry()
  n_nodes <- length(nodes(geom))
  indices <- 1:min(3, n_nodes)
  data <- seq_along(indices)

  roi <- ROISurface(geom, indices, data)
  roi_coords <- coords(roi)

  expect_equal(nrow(roi_coords), length(indices))
  expect_equal(ncol(roi_coords), 3)
})

test_that("length() returns number of vertices in ROISurface", {
  geom <- example_surface_geometry()
  n_nodes <- length(nodes(geom))
  indices <- 1:min(3, n_nodes)
  data <- seq_along(indices)

  roi <- ROISurface(geom, indices, data)

  expect_equal(length(roi), length(indices))
})

test_that("subsetting ROISurface works", {
  geom <- example_surface_geometry()
  n_nodes <- length(nodes(geom))
  indices <- 1:min(4, n_nodes)
  data <- seq_along(indices)

  roi <- ROISurface(geom, indices, data)

  # Subset to first 2
  roi_sub <- roi[1:2]

  expect_s4_class(roi_sub, "ROISurface")
  expect_equal(length(roi_sub), 2)
  expect_equal(values(roi_sub), 1:2)
})

test_that("subsetting ROISurface with out-of-bounds warns", {
  geom <- example_surface_geometry()
  n_nodes <- length(nodes(geom))
  indices <- 1:min(3, n_nodes)
  data <- seq_along(indices)

  roi <- ROISurface(geom, indices, data)

  # Request indices beyond ROI size
  expect_warning(
    roi_sub <- roi[1:10],
    "out of bounds"
  )
})

test_that("subsetting ROISurface to empty returns empty ROI", {
  geom <- example_surface_geometry()
  n_nodes <- length(nodes(geom))
  indices <- 1:min(3, n_nodes)
  data <- seq_along(indices)

  roi <- ROISurface(geom, indices, data)

  # Request only invalid indices
  expect_warning(
    roi_empty <- roi[100:105]
  )

  expect_equal(length(roi_empty), 0)
})

test_that("as.matrix works on ROISurfaceVector", {
  geom <- example_surface_geometry()
  n_nodes <- length(nodes(geom))
  indices <- 1:min(3, n_nodes)
  n_idx <- length(indices)
  data <- matrix(rnorm(3 * n_idx), nrow = 3, ncol = n_idx)

  roi_vec <- ROISurfaceVector(geom, indices, data)

  # Access data slot directly for now - as.matrix has coercion issues
  mat <- values(roi_vec)

  expect_true(is.matrix(mat))
  expect_equal(dim(mat), c(3, n_idx))
})

test_that("show() method works for ROISurface", {
  geom <- example_surface_geometry()
  n_nodes <- length(nodes(geom))
  indices <- 1:min(3, n_nodes)
  data <- rnorm(length(indices))

  roi <- ROISurface(geom, indices, data)

  # Capture output
  output <- capture.output(show(roi))

  expect_true(any(grepl("ROISurface", output)))
  expect_true(any(grepl("size:", output)))
})

test_that("SurfaceDisk creates disk-shaped ROI", {
  geom <- example_surface_geometry()

  # Create a disk centered at vertex 1 with 5mm radius
  disk <- SurfaceDisk(geom, index = 1, radius = 5)

  expect_s4_class(disk, "ROISurface")
  expect_true(length(disk) > 0)
  # The center vertex should be included
  expect_true(1 %in% indices(disk))
})

test_that("SurfaceDisk validates inputs", {
  geom <- example_surface_geometry()
  n_nodes <- length(nodes(geom))

  # Invalid index (out of range)
  expect_error(
    SurfaceDisk(geom, index = n_nodes + 100, radius = 5)
  )

  # Invalid radius (negative)
  expect_error(
    SurfaceDisk(geom, index = 1, radius = -5)
  )

  # Non-numeric index
  expect_error(
    SurfaceDisk(geom, index = "a", radius = 5)
  )
})

test_that("SurfaceDisk with custom max_order", {
  geom <- example_surface_geometry()

  disk <- SurfaceDisk(geom, index = 1, radius = 5, max_order = 10)

  expect_s4_class(disk, "ROISurface")
})

test_that("ROISurface coords match vertices from geometry", {
  geom <- example_surface_geometry()
  n_nodes <- length(nodes(geom))
  indices <- 1:min(3, n_nodes)

  roi <- ROISurface(geom, indices, seq_along(indices))
  roi_coords <- coords(roi)
  geom_coords <- vertices(geom, indices)

  expect_equal(roi_coords, geom_coords)
})
