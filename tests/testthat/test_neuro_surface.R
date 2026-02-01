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
  # Use smaller subset that fits the example geometry
  subset_size <- min(3, n_verts)
  subset_indices <- sample(seq_len(n_verts), subset_size)
  data <- rnorm(subset_size)

  ns <- NeuroSurface(geom, subset_indices, data)

  expect_s4_class(ns, "NeuroSurface")
  expect_equal(length(ns@indices), subset_size)
  expect_equal(length(ns@data), subset_size)
})

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

test_that("NeuroSurfaceVector series method works with numeric indices", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  n_cols <- 10
  mat <- matrix(rnorm(n_verts * n_cols), nrow = n_verts, ncol = n_cols)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  # Extract series for valid vertices
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

  # Extract series for valid vertices
  valid_indices <- as.integer(seq_len(min(3, n_verts)))
  ser <- series(nsv, valid_indices)
  expect_true(is.matrix(ser) || inherits(ser, "Matrix"))
})

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

test_that("conn_comp works on NeuroSurface", {
  # Use the std.8 surface for proper neighborhood testing
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)

  # Create data with distinct clusters
  set.seed(123)
  data <- rnorm(n_verts, mean = 0, sd = 0.5)

  # Set a few regions to high values to create clusters
  g <- graph(geom)
  centers <- c(10, 100, 200)
  for (center in centers) {
    neighbors <- unlist(igraph::neighborhood(g, 2, center))
    data[neighbors] <- 3
  }

  ns <- NeuroSurface(geom, indices, data)

  # Find connected components above threshold
  components <- conn_comp(ns, c(-Inf, 2))

  expect_type(components, "list")
  expect_named(components, c("index", "size"))
  expect_s4_class(components$index, "NeuroSurface")
  expect_s4_class(components$size, "NeuroSurface")
})

test_that("cluster_threshold works on NeuroSurface", {
  # Use the std.8 surface for proper neighborhood testing
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)

  # Create data with distinct clusters
  set.seed(456)
  data <- rnorm(n_verts, mean = 0, sd = 0.5)

  # Set a region to high values
  g <- graph(geom)
  neighbors <- unlist(igraph::neighborhood(g, 3, 50))
  data[neighbors] <- 3

  ns <- NeuroSurface(geom, indices, data)

  # Apply cluster thresholding
  thresholded <- cluster_threshold(ns, c(-Inf, 2), size = 5)

  expect_s4_class(thresholded, "NeuroSurface")
})

test_that("conn_comp works on NeuroSurfaceVector", {
  # Use the std.8 surface for proper neighborhood testing
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)
  n_verts <- nrow(coords(geom))
  n_cols <- 5

  # Create data with clusters in first column
  set.seed(789)
  mat <- matrix(rnorm(n_verts * n_cols, mean = 0, sd = 0.5), nrow = n_verts, ncol = n_cols)

  # Add cluster in first column
  g <- graph(geom)
  neighbors <- unlist(igraph::neighborhood(g, 2, 30))
  mat[neighbors, 1] <- 3

  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  # Find connected components in first column
  components <- conn_comp(nsv, c(-Inf, 2), index = 1)

  expect_type(components, "list")
  expect_named(components, c("index", "size"))
})

test_that("cluster_threshold works on NeuroSurfaceVector", {
  # Use the std.8 surface for proper neighborhood testing
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)
  n_verts <- nrow(coords(geom))
  n_cols <- 5

  # Create data with clusters
  set.seed(101)
  mat <- matrix(rnorm(n_verts * n_cols, mean = 0, sd = 0.5), nrow = n_verts, ncol = n_cols)

  # Add cluster in first column
  g <- graph(geom)
  neighbors <- unlist(igraph::neighborhood(g, 3, 60))
  mat[neighbors, 1] <- 3

  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  # Apply cluster thresholding to first column
  thresholded <- cluster_threshold(nsv, c(-Inf, 2), size = 5, index = 1)

  expect_s4_class(thresholded, "NeuroSurface")
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

# Additional tests for neuro_surface.R coverage

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

test_that("map_values with matrix lookup works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  indices <- seq_len(n_verts)
  data <- c(1, 2, 3, 4)

  ns <- NeuroSurface(geom, indices, data)

  # Matrix: col1=key, col2=value
  lookup <- matrix(c(1, 10, 2, 20, 3, 30, 4, 40), ncol = 2, byrow = TRUE)
  mapped <- map_values(ns, lookup)

  expect_s4_class(mapped, "NeuroSurface")
  expect_equal(as.vector(mapped), c(10, 20, 30, 40))
})

test_that("map_values matrix lookup validates columns", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  ns <- NeuroSurface(geom, seq_len(n_verts), rnorm(n_verts))

  # Matrix with wrong number of columns
  bad_lookup <- matrix(1:9, ncol = 3)

  expect_error(map_values(ns, bad_lookup), "two columns")
})

test_that("map_values matrix lookup validates rows", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  ns <- NeuroSurface(geom, seq_len(n_verts), rnorm(n_verts))

  # Empty matrix
  empty_lookup <- matrix(numeric(0), ncol = 2)

  expect_error(map_values(ns, empty_lookup), "at least one row")
})

test_that("series_roi with numeric indices works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  n_cols <- 5
  mat <- matrix(rnorm(n_verts * n_cols), nrow = n_verts, ncol = n_cols)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  roi_indices <- c(1L, 2L)
  roi_vec <- series_roi(nsv, roi_indices)

  expect_s4_class(roi_vec, "ROISurfaceVector")
  expect_equal(length(indices(roi_vec)), 2)
})

test_that("series_roi with ROISurface works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  n_cols <- 5
  mat <- matrix(rnorm(n_verts * n_cols), nrow = n_verts, ncol = n_cols)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  # Create an ROISurface
  roi <- ROISurface(geom, indices = c(1L, 2L), data = c(1, 1))

  roi_vec <- series_roi(nsv, roi)

  expect_s4_class(roi_vec, "ROISurfaceVector")
})

test_that("series with ROISurface works", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  n_cols <- 5
  mat <- matrix(rnorm(n_verts * n_cols), nrow = n_verts, ncol = n_cols)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  roi <- ROISurface(geom, indices = c(1L, 2L), data = c(1, 1))

  ser <- series(nsv, roi)

  expect_true(is.matrix(ser) || inherits(ser, "Matrix"))
  expect_equal(ncol(ser), 2)
})

test_that("series method works on NeuroSurface", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- rnorm(n_verts)
  ns <- NeuroSurface(geom, seq_len(n_verts), data)

  ser <- series(ns, c(1, 2))

  expect_true(is.matrix(ser) || inherits(ser, "Matrix"))
})

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

test_that("show method works for NeuroSurface", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  ns <- NeuroSurface(geom, seq_len(n_verts), rnorm(n_verts))

  output <- capture.output(show(ns))

  expect_true(length(output) > 0)
  expect_true(any(grepl("NeuroSurface", output)))
})

test_that("show method works for NeuroSurfaceVector", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  mat <- matrix(rnorm(n_verts * 5), nrow = n_verts, ncol = 5)
  nsv <- NeuroSurfaceVector(geom, seq_len(n_verts), mat)

  output <- capture.output(show(nsv))

  expect_true(length(output) > 0)
  expect_true(any(grepl("NeuroSurfaceVector", output)))
})

test_that("show method for NeuroSurface handles empty data", {
  geom <- example_surface_geometry()

  # Create with no indices/data
  ns <- NeuroSurface(geom, integer(0), numeric(0))

  # Should not error
  expect_no_error(capture.output(show(ns)))
})
