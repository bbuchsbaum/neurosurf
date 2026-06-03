# Tests for neuro_surface.R

# NeuroSurface Constructor Tests ---------------------------------------------

test_that("NeuroSurface constructor works correctly", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)
  data <- rnorm(n_verts)

  ns <- NeuroSurface(geom, indices, data)

  expect_s4_class(ns, "NeuroSurface")
  expect_equal(length(ns@data), n_verts)
  expect_equal(length(ns@indices), n_verts)
})

test_that("NeuroSurface works with subset of indices", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  subset_size <- min(3, n_verts)
  subset_indices <- sample(seq_len(n_verts), subset_size)
  data <- rnorm(subset_size)

  ns <- NeuroSurface(geom, subset_indices, data)

  expect_s4_class(ns, "NeuroSurface")
  expect_equal(length(ns@indices), subset_size)
  expect_equal(length(ns@data), subset_size)
})

test_that("NeuroSurface validation requires matching data and indices length", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))

  expect_error(
    NeuroSurface(geom, seq_len(n_verts), c(1, 2)),
    "length of 'data' must equal length of 'indices'"
  )
})

# NeuroSurface Method Tests --------------------------------------------------

test_that("NeuroSurface coords method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  ns <- NeuroSurface(geom, seq_len(n_verts), rnorm(n_verts))

  crds <- coords(ns)
  expect_true(is.matrix(crds))
  expect_equal(ncol(crds), 3)
  expect_equal(nrow(crds), n_verts)
})

test_that("NeuroSurface geometry method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  ns <- NeuroSurface(geom, seq_len(n_verts), rnorm(n_verts))

  g <- geometry(ns)
  expect_s4_class(g, "SurfaceGeometry")
})

test_that("NeuroSurface vertices method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  ns <- NeuroSurface(geom, seq_len(n_verts), rnorm(n_verts))

  v <- vertices(ns)
  expect_true(is.matrix(v))
  expect_equal(ncol(v), 3)
})

test_that("NeuroSurface faces method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  ns <- NeuroSurface(geom, seq_len(n_verts), rnorm(n_verts))

  f <- faces(ns)
  expect_true(is.matrix(f))
  expect_equal(ncol(f), 3)
})

test_that("NeuroSurface indices method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)
  ns <- NeuroSurface(geom, indices, rnorm(n_verts))

  expect_equal(indices(ns), indices)
})

test_that("NeuroSurface nodes method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  ns <- NeuroSurface(geom, seq_len(n_verts), rnorm(n_verts))

  n <- nodes(ns)
  expect_true(is.integer(n) || is.numeric(n))
  expect_equal(length(n), n_verts)
})

test_that("NeuroSurface graph method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  ns <- NeuroSurface(geom, seq_len(n_verts), rnorm(n_verts))

  g <- graph(ns)
  expect_s3_class(g, "igraph")
})

test_that("NeuroSurface as.vector works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- rnorm(n_verts)
  ns <- NeuroSurface(geom, seq_len(n_verts), data)

  vec <- as.vector(ns)
  expect_type(vec, "double")
  expect_equal(length(vec), n_verts)
  expect_equal(vec, data)
})

test_that("NeuroSurface values method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- rnorm(n_verts)
  ns <- NeuroSurface(geom, seq_len(n_verts), data)

  vals <- values(ns)
  expect_equal(vals, data)
})

test_that("NeuroSurface series method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- rnorm(n_verts)
  ns <- NeuroSurface(geom, seq_len(n_verts), data)

  ser <- series(ns, c(1, 2))
  expect_true(is.matrix(ser) || inherits(ser, "Matrix"))
})

test_that("NeuroSurface show method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  ns <- NeuroSurface(geom, seq_len(n_verts), rnorm(n_verts))

  output <- capture.output(show(ns))
  expect_true(length(output) > 0)
  expect_true(any(grepl("NeuroSurface", output)))
})

test_that("NeuroSurface show method handles empty data", {
  geom <- example_surface_geometry()
  ns <- NeuroSurface(geom, integer(0), numeric(0))
  expect_no_error(capture.output(show(ns)))
})

test_that("NeuroSurface to vector coercion works", {
  geom <- example_surface_geometry()
  n <- length(nodes(geom))
  data <- rnorm(n)
  ns <- NeuroSurface(geom, seq_len(n), data)

  result <- as(ns, "vector")
  expect_true(is.numeric(result))
  expect_equal(length(result), n)
})

# NeuroSurfaceVector Constructor Tests ---------------------------------------

test_that("NeuroSurfaceVector constructor works correctly", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)
  mat <- matrix(rnorm(n_verts * 10), nrow = n_verts, ncol = 10)

  nsv <- NeuroSurfaceVector(geom, indices, mat)

  expect_s4_class(nsv, "NeuroSurfaceVector")
  expect_equal(nrow(nsv@data), n_verts)
  expect_equal(ncol(nsv@data), 10)
})

# NeuroSurfaceVector Method Tests --------------------------------------------

test_that("NeuroSurfaceVector coords method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(rnorm(n_verts * 5), nrow = n_verts, ncol = 5)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  crds <- coords(nsv)
  expect_true(is.matrix(crds))
  expect_equal(ncol(crds), 3)
})

test_that("NeuroSurfaceVector geometry method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(rnorm(n_verts * 5), nrow = n_verts, ncol = 5)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  g <- geometry(nsv)
  expect_s4_class(g, "SurfaceGeometry")
})

test_that("NeuroSurfaceVector as.matrix works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(rnorm(n_verts * 5), nrow = n_verts, ncol = 5)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  m <- as.matrix(nsv)
  expect_true(is.matrix(m))
  expect_equal(dim(m), c(n_verts, 5))
})

test_that("NeuroSurfaceVector indices method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)
  mat <- matrix(rnorm(n_verts * 5), nrow = n_verts, ncol = 5)
  nsv <- NeuroSurfaceVector(geom, indices, mat)

  expect_equal(indices(nsv), indices)
})

test_that("NeuroSurfaceVector nodes method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(rnorm(n_verts * 5), nrow = n_verts, ncol = 5)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  n <- nodes(nsv)
  expect_true(is.integer(n) || is.numeric(n))
})

test_that("NeuroSurfaceVector graph method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(rnorm(n_verts * 5), nrow = n_verts, ncol = 5)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  g <- graph(nsv)
  expect_s3_class(g, "igraph")
})

test_that("NeuroSurfaceVector faces method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(rnorm(n_verts * 5), nrow = n_verts, ncol = 5)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  f <- faces(nsv)
  expect_true(is.matrix(f))
  expect_equal(ncol(f), 3)
})

test_that("NeuroSurfaceVector vertices method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(rnorm(n_verts * 5), nrow = n_verts, ncol = 5)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  v <- vertices(nsv)
  expect_true(is.matrix(v))
  expect_equal(ncol(v), 3)
})

test_that("NeuroSurfaceVector series method works with numeric indices", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  n_cols <- 10
  mat <- matrix(rnorm(n_verts * n_cols), nrow = n_verts, ncol = n_cols)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  valid_indices <- seq_len(min(3, n_verts))
  ser <- series(nsv, valid_indices)
  expect_true(is.matrix(ser) || inherits(ser, "Matrix"))
  expect_equal(ncol(ser), length(valid_indices))
  expect_equal(nrow(ser), n_cols)
})

test_that("NeuroSurfaceVector series method works with integer indices", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  n_cols <- 10
  mat <- matrix(rnorm(n_verts * n_cols), nrow = n_verts, ncol = n_cols)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  valid_indices <- as.integer(seq_len(min(3, n_verts)))
  ser <- series(nsv, valid_indices)
  expect_true(is.matrix(ser) || inherits(ser, "Matrix"))
})

test_that("NeuroSurfaceVector series method works with ROISurface", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  n_cols <- 5
  mat <- matrix(rnorm(n_verts * n_cols), nrow = n_verts, ncol = n_cols)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  roi <- ROISurface(geom, indices = c(1L, 2L), data = c(1, 1))
  ser <- series(nsv, roi)

  expect_true(is.matrix(ser) || inherits(ser, "Matrix"))
  expect_equal(ncol(ser), 2)
  expect_equal(nrow(ser), n_cols)
})

test_that("NeuroSurfaceVector series_roi method works with numeric indices", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  n_cols <- 5
  mat <- matrix(rnorm(n_verts * n_cols), nrow = n_verts, ncol = n_cols)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  roi_vec <- series_roi(nsv, c(1L, 2L))
  expect_s4_class(roi_vec, "ROISurfaceVector")
  expect_equal(length(indices(roi_vec)), 2)
})

test_that("NeuroSurfaceVector series_roi method works with ROISurface", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  n_cols <- 5
  mat <- matrix(rnorm(n_verts * n_cols), nrow = n_verts, ncol = n_cols)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  roi <- ROISurface(geom, indices = c(1L, 2L), data = c(1, 1))
  roi_vec <- series_roi(nsv, roi)
  expect_s4_class(roi_vec, "ROISurfaceVector")
})

test_that("NeuroSurfaceVector show method works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(rnorm(n_verts * 5), nrow = n_verts, ncol = 5)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  output <- capture.output(show(nsv))
  expect_true(length(output) > 0)
  expect_true(any(grepl("NeuroSurfaceVector", output)))
})

# NeuroSurfaceVector Subsetting Tests ----------------------------------------

test_that("NeuroSurfaceVector [[ extraction works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  n_cols <- 5
  mat <- matrix(rnorm(n_verts * n_cols), nrow = n_verts, ncol = n_cols)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  col1 <- nsv[[1]]
  expect_equal(length(col1), n_verts)
})

test_that("NeuroSurfaceVector [i,j] subsetting works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  n_cols <- 5
  mat <- matrix(rnorm(n_verts * n_cols), nrow = n_verts, ncol = n_cols)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  subset <- nsv[1:2, 1:3]
  expect_equal(dim(subset), c(2, 3))
})

test_that("NeuroSurfaceVector [,j] column subsetting works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  n_cols <- 5
  mat <- matrix(rnorm(n_verts * n_cols), nrow = n_verts, ncol = n_cols)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  cols <- nsv[, 1:2]
  expect_equal(ncol(cols), 2)
})

test_that("NeuroSurfaceVector [i,] row subsetting works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  n_cols <- 5
  mat <- matrix(rnorm(n_verts * n_cols), nrow = n_verts, ncol = n_cols)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  rows <- nsv[1:2, ]
  expect_equal(nrow(rows), 2)
})

test_that("NeuroSurfaceVector [,] full extraction works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  n_cols <- 5
  mat <- matrix(rnorm(n_verts * n_cols), nrow = n_verts, ncol = n_cols)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  full <- nsv[, ]
  expect_equal(dim(full), c(n_verts, n_cols))
})

# BilatNeuroSurfaceVector Tests ----------------------------------------------

# Helper: build a BilatNeuroSurfaceVector with `n_cols` columns per hemisphere.
make_bilat <- function(n_cols) {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  left_geom <- geom
  left_geom@hemi <- "left"
  right_geom <- geom
  right_geom@hemi <- "right"
  support <- seq_len(n_verts)

  left <- NeuroSurfaceVector(
    left_geom, support,
    matrix(seq_len(n_verts * n_cols), nrow = n_verts, ncol = n_cols)
  )
  right <- NeuroSurfaceVector(
    right_geom, support,
    matrix(seq_len(n_verts * n_cols) + 1000, nrow = n_verts, ncol = n_cols)
  )
  methods::new("BilatNeuroSurfaceVector", left = left, right = right)
}

test_that("as.matrix works for single-column BilatNeuroSurfaceVector (GH #70)", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  bilat <- make_bilat(1L)

  # Single-column case used to error with "invalid 'times' argument" because
  # `[` dropped the 1-column Matrix to a vector.
  m <- expect_no_error(as.matrix(bilat))
  expect_true(is.matrix(m))
  # Hemispheres are stacked by row, one column preserved.
  expect_equal(dim(m), c(2 * n_verts, 1))
  expect_equal(m[seq_len(n_verts), 1], as.numeric(seq_len(n_verts)))
  expect_equal(m[n_verts + seq_len(n_verts), 1], as.numeric(seq_len(n_verts) + 1000))

  # Attributes are well-formed and consistent with the row count.
  expect_equal(attr(m, "hemi"), c(rep(1, n_verts), rep(2, n_verts)))
  expect_equal(nrow(attr(m, "coords")), 2 * n_verts)
  expect_length(attr(m, "indices"), 2 * n_verts)
})

test_that("as.matrix preserves orientation for multi-column BilatNeuroSurfaceVector", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  bilat <- make_bilat(5L)

  m <- as.matrix(bilat)
  expect_equal(dim(m), c(2 * n_verts, 5))
  expect_equal(attr(m, "hemi"), c(rep(1, n_verts), rep(2, n_verts)))
})

# ColorMappedNeuroSurface Tests ----------------------------------------------

test_that("ColorMappedNeuroSurface constructor works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)
  data <- rnorm(n_verts)
  cmap <- colorRampPalette(c("blue", "white", "red"))(256)
  irange <- c(-2, 2)
  thresh <- c(-1, 1)

  cmns <- ColorMappedNeuroSurface(geom, indices, data, cmap, irange, thresh)

  expect_s4_class(cmns, "ColorMappedNeuroSurface")
  expect_equal(cmns@cmap, cmap)
  expect_equal(cmns@irange, irange)
  expect_equal(cmns@thresh, thresh)
})

test_that("ColorMappedNeuroSurface validation requires irange length 2", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- rnorm(n_verts)
  cmap <- colorRampPalette(c("blue", "red"))(256)

  expect_error(
    ColorMappedNeuroSurface(geom, seq_len(n_verts), data, cmap, c(0), c(-1, 1)),
    "irange"
  )
})

test_that("ColorMappedNeuroSurface validation requires thresh length 2", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- rnorm(n_verts)
  cmap <- colorRampPalette(c("blue", "red"))(256)

  expect_error(
    ColorMappedNeuroSurface(geom, seq_len(n_verts), data, cmap, c(-2, 2), c(0)),
    "thresh"
  )
})

test_that("ColorMappedNeuroSurface validation requires at least 2 colors in cmap", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- rnorm(n_verts)

  expect_error(
    ColorMappedNeuroSurface(geom, seq_len(n_verts), data, c("#FF0000"), c(-2, 2), c(-1, 1)),
    "cmap"
  )
})

# VertexColoredNeuroSurface Tests --------------------------------------------

test_that("VertexColoredNeuroSurface constructor works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)
  colors <- rep("#FF0000", n_verts)

  vcns <- VertexColoredNeuroSurface(geom, indices, colors)

  expect_s4_class(vcns, "VertexColoredNeuroSurface")
  expect_equal(vcns@colors, colors)
})

test_that("VertexColoredNeuroSurface uses default data when not provided", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)
  colors <- rep("#00FF00", n_verts)

  vcns <- VertexColoredNeuroSurface(geom, indices, colors)
  expect_equal(vcns@data, rep(0, n_verts))
})

test_that("VertexColoredNeuroSurface accepts custom data", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)
  colors <- rep("#0000FF", n_verts)
  data <- rnorm(n_verts)

  vcns <- VertexColoredNeuroSurface(geom, indices, colors, data = data)
  expect_equal(vcns@data, data)
})

test_that("VertexColoredNeuroSurface validation requires colors length to match indices", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))

  expect_error(
    VertexColoredNeuroSurface(geom, seq_len(n_verts), c("#FF0000", "#00FF00")),
    "Length of 'colors' must equal length of 'indices'"
  )
})

test_that("VertexColoredNeuroSurface validation requires valid hex codes", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))

  expect_error(
    VertexColoredNeuroSurface(geom, seq_len(n_verts), c("red", "blue", "green", "yellow")),
    "valid hex color codes"
  )
})

# map_values Tests -----------------------------------------------------------

test_that("map_values with matrix lookup works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)
  data <- c(1, 2, 3, 4)

  ns <- NeuroSurface(geom, indices, data)

  lookup <- matrix(c(1, 10, 2, 20, 3, 30, 4, 40), ncol = 2, byrow = TRUE)
  mapped <- map_values(ns, lookup)

  expect_s4_class(mapped, "NeuroSurface")
  expect_equal(as.vector(mapped), c(10, 20, 30, 40))
})

test_that("map_values matrix lookup validates columns", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  ns <- NeuroSurface(geom, seq_len(n_verts), rnorm(n_verts))

  bad_lookup <- matrix(1:9, ncol = 3)
  expect_error(map_values(ns, bad_lookup), "two columns")
})

test_that("map_values matrix lookup validates rows", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  ns <- NeuroSurface(geom, seq_len(n_verts), rnorm(n_verts))

  empty_lookup <- matrix(numeric(0), ncol = 2)
  expect_error(map_values(ns, empty_lookup), "at least one row")
})

test_that("map_values with matrix handles missing keys", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)
  data <- c(1, 2, 99, 4)

  ns <- NeuroSurface(geom, indices, data)

  lookup <- matrix(c(1, 10, 2, 20, 4, 40), ncol = 2, byrow = TRUE)
  mapped <- map_values(ns, lookup)

  expect_equal(as.vector(mapped)[3], 0)
})

test_that("map_values with list lookup works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)
  data <- c(1, 2, 1, 2)

  ns <- NeuroSurface(geom, indices, data)

  lookup <- list(10, 20)
  mapped <- map_values(ns, lookup)

  expect_s4_class(mapped, "NeuroSurface")
  expect_equal(as.vector(mapped)[1], 10)
  expect_equal(as.vector(mapped)[2], 20)
})

# BilatNeuroSurfaceVector Tests ----------------------------------------------

test_that("BilatNeuroSurfaceVector left accessor works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)
  mat_left <- matrix(rnorm(n_verts * 3), nrow = n_verts, ncol = 3)
  mat_right <- matrix(rnorm(n_verts * 3), nrow = n_verts, ncol = 3)

  nsv_left <- NeuroSurfaceVector(geom, indices, mat_left)
  nsv_right <- NeuroSurfaceVector(geom, indices, mat_right)

  bilat <- new("BilatNeuroSurfaceVector", left = nsv_left, right = nsv_right)

  expect_identical(left(bilat), nsv_left)
})

test_that("BilatNeuroSurfaceVector right accessor works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)
  mat_left <- matrix(rnorm(n_verts * 3), nrow = n_verts, ncol = 3)
  mat_right <- matrix(rnorm(n_verts * 3), nrow = n_verts, ncol = 3)

  nsv_left <- NeuroSurfaceVector(geom, indices, mat_left)
  nsv_right <- NeuroSurfaceVector(geom, indices, mat_right)

  bilat <- new("BilatNeuroSurfaceVector", left = nsv_left, right = nsv_right)

  expect_identical(right(bilat), nsv_right)
})

test_that("BilatNeuroSurfaceVector as.matrix works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)
  mat_left <- matrix(1:(n_verts * 3), nrow = n_verts, ncol = 3)
  mat_right <- matrix((n_verts * 3 + 1):(n_verts * 6), nrow = n_verts, ncol = 3)

  nsv_left <- NeuroSurfaceVector(geom, indices, mat_left)
  nsv_right <- NeuroSurfaceVector(geom, indices, mat_right)

  bilat <- new("BilatNeuroSurfaceVector", left = nsv_left, right = nsv_right)

  result <- as.matrix(bilat)

  # Result may be a Matrix or base matrix
  expect_true(is.matrix(result) || inherits(result, "Matrix"))
  expect_equal(nrow(result), n_verts * 2)
  expect_equal(ncol(result), 3)
  expect_true(!is.null(attr(result, "coords")))
  expect_true(!is.null(attr(result, "indices")))
  expect_true(!is.null(attr(result, "hemi")))
})

# normalize helper function Tests --------------------------------------------

test_that("normalize helper function works", {
  normalize <- neurosurf:::normalize

  vals <- c(0, 5, 10)
  result <- normalize(vals)
  expect_equal(result, c(0, 0.5, 1))
})

test_that("normalize handles constant values", {
  normalize <- neurosurf:::normalize

  vals <- c(5, 5, 5)
  result <- normalize(vals)
  expect_equal(result, c(0, 0, 0))
})

test_that("normalize handles negative values", {
  normalize <- neurosurf:::normalize

  vals <- c(-10, 0, 10)
  result <- normalize(vals)
  expect_equal(min(result), 0)
  expect_equal(max(result), 1)
  expect_equal(result[2], 0.5)
})

# Arithmetic Operations Tests ------------------------------------------------

test_that("NeuroSurface arithmetic with numeric works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- c(1, 2, 3, 4)
  ns <- NeuroSurface(geom, seq_len(n_verts), data)

  result <- ns + 10
  expect_s4_class(result, "NeuroSurface")
  expect_equal(result@data, data + 10)

  result <- ns * 2
  expect_equal(result@data, data * 2)

  result <- ns - 1
  expect_equal(result@data, data - 1)

  result <- ns / 2
  expect_equal(result@data, data / 2)
})

test_that("numeric arithmetic with NeuroSurface works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- c(1, 2, 3, 4)
  ns <- NeuroSurface(geom, seq_len(n_verts), data)

  result <- 10 + ns
  expect_s4_class(result, "NeuroSurface")
  expect_equal(result@data, 10 + data)

  result <- 10 - ns
  expect_equal(result@data, 10 - data)
})

test_that("NeuroSurface arithmetic with NeuroSurface works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data1 <- c(1, 2, 3, 4)
  data2 <- c(4, 3, 2, 1)
  ns1 <- NeuroSurface(geom, seq_len(n_verts), data1)
  ns2 <- NeuroSurface(geom, seq_len(n_verts), data2)

  result <- ns1 + ns2
  expect_s4_class(result, "NeuroSurface")
  expect_equal(result@data, data1 + data2)

  result <- ns1 - ns2
  expect_equal(result@data, data1 - data2)

  result <- ns1 * ns2
  expect_equal(result@data, data1 * data2)
})

test_that("NeuroSurface comparison with numeric works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- c(1, 2, 3, 4)
  ns <- NeuroSurface(geom, seq_len(n_verts), data)

  result <- ns > 2
  expect_s4_class(result, "NeuroSurface")
  expect_equal(result@data, as.numeric(data > 2))

  result <- ns <= 2
  expect_equal(result@data, as.numeric(data <= 2))

  result <- ns == 3
  expect_equal(result@data, as.numeric(data == 3))
})

test_that("NeuroSurface comparison with NeuroSurface works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data1 <- c(1, 2, 3, 4)
  data2 <- c(2, 2, 2, 2)
  ns1 <- NeuroSurface(geom, seq_len(n_verts), data1)
  ns2 <- NeuroSurface(geom, seq_len(n_verts), data2)

  result <- ns1 > ns2
  expect_s4_class(result, "NeuroSurface")
  expect_equal(result@data, as.numeric(data1 > data2))
})

test_that("NeuroSurfaceVector arithmetic with numeric works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(1:(n_verts * 3), nrow = n_verts, ncol = 3)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  result <- nsv + 10
  expect_s4_class(result, "NeuroSurfaceVector")
  expect_equal(as.matrix(result), mat + 10)

  result <- nsv * 2
  expect_equal(as.matrix(result), mat * 2)
})

test_that("numeric arithmetic with NeuroSurfaceVector works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(1:(n_verts * 3), nrow = n_verts, ncol = 3)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  result <- 10 + nsv
  expect_s4_class(result, "NeuroSurfaceVector")
  expect_equal(as.matrix(result), 10 + mat)
})

test_that("NeuroSurfaceVector arithmetic with NeuroSurfaceVector works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat1 <- matrix(1:(n_verts * 3), nrow = n_verts, ncol = 3)
  mat2 <- matrix((n_verts * 3):1, nrow = n_verts, ncol = 3)
  nsv1 <- NeuroSurfaceVector(geom, seq_len(n_verts), mat1)
  nsv2 <- NeuroSurfaceVector(geom, seq_len(n_verts), mat2)

  result <- nsv1 + nsv2
  expect_s4_class(result, "NeuroSurfaceVector")
  expect_equal(as.matrix(result), mat1 + mat2)
})

test_that("NeuroSurfaceVector comparison with numeric works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(1:(n_verts * 3), nrow = n_verts, ncol = 3)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  result <- nsv > 5
  expect_s4_class(result, "NeuroSurfaceVector")
})

test_that("numeric comparison with NeuroSurfaceVector works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(1:(n_verts * 3), nrow = n_verts, ncol = 3)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  result <- 5 < nsv
  expect_s4_class(result, "NeuroSurfaceVector")
})

test_that("NeuroSurfaceVector comparison with NeuroSurfaceVector works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat1 <- matrix(1:(n_verts * 3), nrow = n_verts, ncol = 3)
  mat2 <- matrix((n_verts * 3):1, nrow = n_verts, ncol = 3)
  nsv1 <- NeuroSurfaceVector(geom, seq_len(n_verts), mat1)
  nsv2 <- NeuroSurfaceVector(geom, seq_len(n_verts), mat2)

  result <- nsv1 > nsv2
  expect_s4_class(result, "NeuroSurfaceVector")
})

test_that("NeuroSurface + NeuroSurfaceVector works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- c(1, 2, 3, 4)
  mat <- matrix(1:(n_verts * 3), nrow = n_verts, ncol = 3)
  ns <- NeuroSurface(geom, seq_len(n_verts), data)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  result <- ns + nsv
  expect_s4_class(result, "NeuroSurfaceVector")
})

test_that("NeuroSurfaceVector + NeuroSurface works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- c(1, 2, 3, 4)
  mat <- matrix(1:(n_verts * 3), nrow = n_verts, ncol = 3)
  ns <- NeuroSurface(geom, seq_len(n_verts), data)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  result <- nsv + ns
  expect_s4_class(result, "NeuroSurfaceVector")
})

test_that("arithmetic operations fail for incompatible NeuroSurface objects", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))

  ns1 <- NeuroSurface(geom, seq_len(n_verts), rnorm(n_verts))
  ns2 <- NeuroSurface(geom, c(2L, 3L, 4L, 1L), rnorm(n_verts))

  expect_error(ns1 + ns2, "indices differ")
})

# conn_comp and cluster_threshold Tests --------------------------------------

test_that("conn_comp works on NeuroSurface", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)

  set.seed(123)
  data <- rnorm(n_verts, mean = 0, sd = 0.5)

  g <- graph(geom)
  centers <- c(10, 100, 200)
  for (center in centers) {
    neighbors <- unlist(igraph::neighborhood(g, 2, center))
    data[neighbors] <- 3
  }

  ns <- NeuroSurface(geom, indices, data)
  components <- conn_comp(ns, c(-Inf, 2))

  expect_type(components, "list")
  expect_named(components, c("index", "size"))
  expect_s4_class(components$index, "NeuroSurface")
  expect_s4_class(components$size, "NeuroSurface")
})

test_that("cluster_threshold works on NeuroSurface", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)

  set.seed(456)
  data <- rnorm(n_verts, mean = 0, sd = 0.5)

  g <- graph(geom)
  neighbors <- unlist(igraph::neighborhood(g, 3, 50))
  data[neighbors] <- 3

  ns <- NeuroSurface(geom, indices, data)
  thresholded <- cluster_threshold(ns, c(-Inf, 2), size = 5)

  expect_s4_class(thresholded, "NeuroSurface")
})

test_that("conn_comp works on NeuroSurfaceVector", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)
  n_verts <- nrow(coords(geom))
  n_cols <- 5

  set.seed(789)
  mat <- matrix(rnorm(n_verts * n_cols, mean = 0, sd = 0.5), nrow = n_verts, ncol = n_cols)

  g <- graph(geom)
  neighbors <- unlist(igraph::neighborhood(g, 2, 30))
  mat[neighbors, 1] <- 3

  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)
  components <- conn_comp(nsv, c(-Inf, 2), index = 1)

  expect_type(components, "list")
  expect_named(components, c("index", "size"))
})

test_that("cluster_threshold works on NeuroSurfaceVector", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)
  n_verts <- nrow(coords(geom))
  n_cols <- 5

  set.seed(101)
  mat <- matrix(rnorm(n_verts * n_cols, mean = 0, sd = 0.5), nrow = n_verts, ncol = n_cols)

  g <- graph(geom)
  neighbors <- unlist(igraph::neighborhood(g, 3, 60))
  mat[neighbors, 1] <- 3

  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)
  thresholded <- cluster_threshold(nsv, c(-Inf, 2), size = 5, index = 1)

  expect_s4_class(thresholded, "NeuroSurface")
})
