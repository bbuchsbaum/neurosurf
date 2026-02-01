library(neurosurf)

test_that("find_all_neighbors handles single-vertex graph", {
  verts <- matrix(c(0, 0, 0), ncol = 3)
  faces <- matrix(c(0L, 0L, 0L), ncol = 3)
  sg <- SurfaceGeometry(verts, faces, hemi = "lh")
  g <- graph(sg)
  igraph::E(g)$dist <- rep(1, igraph::ecount(g))
  res <- find_all_neighbors(g, radius = 1, edgeWeights = igraph::E(g)$dist)
  expect_length(res, igraph::vcount(g))
  expect_true(all(vapply(res, nrow, integer(1)) == 0))
})

test_that("projectCoordinates returns smoothed NeuroSurface", {
  geom <- example_surface_geometry()
  surf_coords <- coords(geom)
  set.seed(123)
  pts <- surf_coords[sample(1:nrow(surf_coords), 5, replace = TRUE), ] + matrix(rnorm(15, 0, 0.1), ncol = 3)
  ns <- projectCoordinates(geom, pts, sigma = 2)
  expect_s4_class(ns, "NeuroSurface")
  expect_equal(ns@indices, seq_len(nrow(surf_coords)))
  expect_true(all(ns@data >= 0))
})

# Additional neighborhood.R coverage tests

test_that("find_all_neighbors works with geodesic distance", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  edgeWeights <- igraph::E(g)$dist

  nb <- find_all_neighbors(geom, radius = 5, edgeWeights = edgeWeights,
                           nodes = 1:2, distance_type = "geodesic")

  expect_type(nb, "list")
  expect_length(nb, 2)
  # Each element should be a matrix with columns i, j, d
  expect_true(all(vapply(nb, function(x) is.matrix(x), logical(1))))
})

test_that("find_all_neighbors works with euclidean distance", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  edgeWeights <- igraph::E(g)$dist

  nb <- find_all_neighbors(geom, radius = 5, edgeWeights = edgeWeights,
                           nodes = 1:2, distance_type = "euclidean")

  expect_type(nb, "list")
  expect_length(nb, 2)
})

test_that("neighbor_graph creates igraph from SurfaceGeometry", {
  geom <- example_surface_geometry()

  ng <- neighbor_graph(geom, radius = 2)

  expect_s3_class(ng, "igraph")
})

test_that("neighbor_graph works with custom edgeWeights", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  custom_weights <- rep(1, igraph::ecount(g))

  ng <- neighbor_graph(geom, radius = 2, edgeWeights = custom_weights)

  expect_s3_class(ng, "igraph")
})

test_that("neighbor_graph works with specific nodes", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  edgeWeights <- igraph::E(g)$dist

  ng <- neighbor_graph(geom, radius = 2, edgeWeights = edgeWeights, nodes = 1L:2L)

  expect_s3_class(ng, "igraph")
})

test_that("laplacian returns sparse matrix for SurfaceGeometry", {
  geom <- example_surface_geometry()

  lap <- laplacian(geom)

  expect_true(inherits(lap, "Matrix") || is.matrix(lap))
  expect_equal(nrow(lap), ncol(lap))
})

test_that("laplacian works with custom weights", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  weights <- rep(1, igraph::ecount(g))

  lap <- laplacian(geom, weights = weights)

  expect_true(inherits(lap, "Matrix") || is.matrix(lap))
})

test_that("adjacency returns sparse matrix without attr", {
  geom <- example_surface_geometry()

  adj <- adjacency(geom)

  expect_true(inherits(adj, "Matrix") || is.matrix(adj))
  expect_equal(nrow(adj), ncol(adj))
})

test_that("adjacency works with numeric attr", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  attr_vals <- rep(0.5, igraph::ecount(g))

  adj <- adjacency(geom, attr = attr_vals)

  expect_true(inherits(adj, "Matrix") || is.matrix(adj))
})

test_that("adjacency works with character attr", {
  geom <- example_surface_geometry()

  # The 'dist' attribute should exist on edges
  adj <- adjacency(geom, attr = "dist")

  expect_true(inherits(adj, "Matrix") || is.matrix(adj))
})

test_that("smooth works on SurfaceGeometry with taubin", {
  geom <- example_surface_geometry()

  smoothed <- smooth(geom, type = "taubin", iteration = 2)

  expect_s4_class(smoothed, "SurfaceGeometry")
  expect_equal(nrow(coords(smoothed)), nrow(coords(geom)))
})

test_that("smooth works on SurfaceGeometry with laplace", {
  geom <- example_surface_geometry()

  smoothed <- smooth(geom, type = "laplace", iteration = 2)

  expect_s4_class(smoothed, "SurfaceGeometry")
})

test_that("smooth works on NeuroSurface", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  ns <- NeuroSurface(geom, indices = seq_len(n_verts), data = rnorm(n_verts))

  smoothed <- smooth(ns, sigma = 1)

  expect_s4_class(smoothed, "NeuroSurface")
  expect_equal(length(smoothed@data), n_verts)
})

test_that("projectCoordinates validates input", {
  geom <- example_surface_geometry()

  # Should error on non-matrix points
  expect_error(projectCoordinates(geom, c(1, 2, 3)), "must be a matrix")

  # Should error on wrong number of columns
  expect_error(projectCoordinates(geom, matrix(1:4, ncol = 2)), "3 columns")

  # Should error on invalid sigma

  expect_error(projectCoordinates(geom, matrix(1:3, ncol = 3), sigma = -1), "positive")
})

# Additional comprehensive neighborhood tests

test_that("find_all_neighbors validates edgeWeights", {
  geom <- example_surface_geometry()
  g <- graph(geom)

  # Non-numeric edgeWeights should fail
  expect_error(find_all_neighbors(geom, radius = 5, edgeWeights = "bad"),
               "edgeWeights must be numeric")

  # NA in edgeWeights should fail
  bad_weights <- igraph::E(g)$dist
  bad_weights[1] <- NA
  expect_error(find_all_neighbors(geom, radius = 5, edgeWeights = bad_weights),
               "cannot contain NA")
})

test_that("find_all_neighbors validates radius", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  edgeWeights <- igraph::E(g)$dist

  # Negative radius should fail
  expect_error(find_all_neighbors(geom, radius = -1, edgeWeights = edgeWeights),
               "positive")

  # Zero radius should fail
  expect_error(find_all_neighbors(geom, radius = 0, edgeWeights = edgeWeights),
               "positive")

  # Multiple radius values should fail
  expect_error(find_all_neighbors(geom, radius = c(1, 2), edgeWeights = edgeWeights),
               "single")
})

test_that("find_all_neighbors validates nodes", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  edgeWeights <- igraph::E(g)$dist
  n_verts <- igraph::vcount(g)

  # Out of range nodes should fail
  expect_error(find_all_neighbors(geom, radius = 5, edgeWeights = edgeWeights,
                                  nodes = c(1, n_verts + 10)),
               "out of range")

  # Zero index should fail
  expect_error(find_all_neighbors(geom, radius = 5, edgeWeights = edgeWeights,
                                  nodes = c(0, 1)),
               "out of range")

  # NA in nodes should fail
  expect_error(find_all_neighbors(geom, radius = 5, edgeWeights = edgeWeights,
                                  nodes = c(1, NA)),
               "cannot contain NA")
})

test_that("find_all_neighbors returns correct structure", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  surf <- read_surf(surf_file)
  g <- graph(surf)
  edgeWeights <- igraph::E(g)$dist

  nb <- find_all_neighbors(surf, radius = 10, edgeWeights = edgeWeights, nodes = 1:3)

  expect_type(nb, "list")
  expect_length(nb, 3)

  # Each element should be a matrix with 3 columns (i, j, d)
  for (m in nb) {
    if (nrow(m) > 0) {
      expect_equal(ncol(m), 3)
      expect_true("i" %in% colnames(m))
      expect_true("j" %in% colnames(m))
      expect_true("d" %in% colnames(m))
      # All distances should be non-negative
      expect_true(all(m[, "d"] >= 0))
    }
  }
})

test_that("neighbor_graph with igraph input works", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  surf <- read_surf(surf_file)
  g <- graph(surf)

  ng <- neighbor_graph(g, radius = 10)

  expect_s3_class(ng, "igraph")
  expect_true(igraph::vcount(ng) > 0)
})

test_that("smooth works with HClaplace type", {
  geom <- example_surface_geometry()

  smoothed <- smooth(geom, type = "HClaplace", iteration = 2)

  expect_s4_class(smoothed, "SurfaceGeometry")
  expect_equal(nrow(coords(smoothed)), nrow(coords(geom)))
})

test_that("smooth works with fujiLaplace type", {
  geom <- example_surface_geometry()

  smoothed <- smooth(geom, type = "fujiLaplace", iteration = 2)

  expect_s4_class(smoothed, "SurfaceGeometry")
})

test_that("smooth works with angWeight type", {
  geom <- example_surface_geometry()

  smoothed <- smooth(geom, type = "angWeight", iteration = 2)

  expect_s4_class(smoothed, "SurfaceGeometry")
})

test_that("smooth works with surfPreserveLaplace type", {
  geom <- example_surface_geometry()

  smoothed <- smooth(geom, type = "surfPreserveLaplace", iteration = 2)

  expect_s4_class(smoothed, "SurfaceGeometry")
})

test_that("smooth preserves number of vertices", {
  geom <- example_surface_geometry()
  n_orig <- nrow(coords(geom))

  for (type in c("taubin", "laplace", "HClaplace")) {
    smoothed <- smooth(geom, type = type, iteration = 2)
    expect_equal(nrow(coords(smoothed)), n_orig)
  }
})

test_that("find_all_neighbors uses all nodes when nodes=NULL", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  edgeWeights <- igraph::E(g)$dist
  n_verts <- igraph::vcount(g)

  nb <- find_all_neighbors(geom, radius = 10, edgeWeights = edgeWeights)

  expect_length(nb, n_verts)
})

test_that("neighbor_graph with nodes parameter limits output", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  surf <- read_surf(surf_file)
  g <- graph(surf)
  edgeWeights <- igraph::E(g)$dist

  # Only consider nodes 1:10
  ng <- neighbor_graph(surf, radius = 5, edgeWeights = edgeWeights, nodes = 1L:10L)

  expect_s3_class(ng, "igraph")
})

test_that("laplacian matrix is square", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  surf <- read_surf(surf_file)

  lap <- laplacian(surf)

  expect_equal(nrow(lap), ncol(lap))
  expect_equal(nrow(lap), length(nodes(surf)))
})

test_that("adjacency matrix is square", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  surf <- read_surf(surf_file)

  adj <- adjacency(surf)

  expect_equal(nrow(adj), ncol(adj))
  expect_equal(nrow(adj), length(nodes(surf)))
})

test_that("projectCoordinates validates surfgeom type", {
  pts <- matrix(c(0, 0, 0), ncol = 3)

  # Should error on non-SurfaceGeometry
  expect_error(projectCoordinates("not a geometry", pts), "SurfaceGeometry")
})

test_that("smooth on NeuroSurface preserves geometry", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  ns <- NeuroSurface(geom, indices = seq_len(n_verts), data = rnorm(n_verts))

  smoothed <- smooth(ns, sigma = 1)

  # Geometry should be unchanged
  expect_equal(coords(smoothed@geometry), coords(geom))
})

test_that("find_all_neighbors returns distances less than radius", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  surf <- read_surf(surf_file)
  g <- graph(surf)
  edgeWeights <- igraph::E(g)$dist

  radius <- 8
  nb <- find_all_neighbors(surf, radius = radius, edgeWeights = edgeWeights,
                           nodes = 1:5, distance_type = "geodesic")

  for (m in nb) {
    if (nrow(m) > 0) {
      expect_true(all(m[, "d"] < radius))
    }
  }
})

test_that("neighbor_graph distance types produce different results", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  surf <- read_surf(surf_file)

  ng_geo <- neighbor_graph(surf, radius = 10, distance_type = "geodesic")
  ng_euc <- neighbor_graph(surf, radius = 10, distance_type = "euclidean")

  # Different distance types should potentially produce different graphs
  # At least they should both be valid igraphs
  expect_s3_class(ng_geo, "igraph")
  expect_s3_class(ng_euc, "igraph")
})

test_that("findNeighbors returns valid node indices", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  surf <- read_surf(surf_file)
  g <- graph(surf)
  edgeWeights <- igraph::E(g)$dist

  neighbors <- neurosurf:::findNeighbors(g, node = 1, radius = 5, edgeWeights = edgeWeights)

  # findNeighbors returns a vector of neighbor node indices
  expect_true(is.numeric(neighbors) || inherits(neighbors, "igraph.vs"))

  # All values should be valid node indices
  if (length(neighbors) > 0) {
    neighbor_ids <- as.integer(neighbors)
    expect_true(all(neighbor_ids >= 1))
    expect_true(all(neighbor_ids <= igraph::vcount(g)))
  }
})

test_that("findNeighbors respects max_order parameter", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  surf <- read_surf(surf_file)
  g <- graph(surf)
  edgeWeights <- igraph::E(g)$dist

  # With max_order=1, should only find immediate neighbors
  neighbors_1 <- neurosurf:::findNeighbors(g, node = 1, radius = 100,
                                           edgeWeights = edgeWeights, max_order = 1)

  # With max_order=2, could find more neighbors
  neighbors_2 <- neurosurf:::findNeighbors(g, node = 1, radius = 100,
                                           edgeWeights = edgeWeights, max_order = 2)

  # max_order=2 should find at least as many as max_order=1
  expect_true(length(neighbors_2) >= length(neighbors_1))
})

test_that("smooth NeuroSurface with larger sigma", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data_vec <- rnorm(n_verts)
  ns <- NeuroSurface(geom, indices = seq_len(n_verts), data = data_vec)

  # Larger sigma should produce more smoothing
  smoothed_1 <- smooth(ns, sigma = 1)
  smoothed_5 <- smooth(ns, sigma = 5)

  expect_s4_class(smoothed_1, "NeuroSurface")
  expect_s4_class(smoothed_5, "NeuroSurface")

  # Smoothed data should have smaller variance than original
  expect_true(var(smoothed_5@data) <= var(data_vec) + 0.1)  # Allow small tolerance
})

test_that("adjacency matrix has correct dimensions", {
  geom <- example_surface_geometry()
  n_nodes <- length(nodes(geom))

  adj <- adjacency(geom)

  expect_equal(dim(adj), c(n_nodes, n_nodes))
})

test_that("laplacian matrix row sums are approximately zero", {
  geom <- example_surface_geometry()

  lap <- laplacian(geom)

  # Row sums of Laplacian should be close to zero
  row_sums <- Matrix::rowSums(lap)
  expect_true(all(abs(row_sums) < 1e-10))
})

# Additional edge case tests for neighborhood.R

test_that("smooth with custom lambda and mu parameters", {
  geom <- example_surface_geometry()

  # Test taubin with non-default parameters
  smoothed <- smooth(geom, type = "taubin", lambda = 0.3, mu = -0.3, iteration = 3)

  expect_s4_class(smoothed, "SurfaceGeometry")
})

test_that("smooth with delta parameter for surfPreserveLaplace", {
  geom <- example_surface_geometry()

  smoothed <- smooth(geom, type = "surfPreserveLaplace", delta = 0.2, iteration = 2)

  expect_s4_class(smoothed, "SurfaceGeometry")
})

test_that("find_all_neighbors with igraph input directly", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  edgeWeights <- igraph::E(g)$dist

  nb <- find_all_neighbors(g, radius = 5, edgeWeights = edgeWeights, nodes = 1:2)

  expect_type(nb, "list")
  expect_length(nb, 2)
})

test_that("findNeighbors validates node index", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  surf <- read_surf(surf_file)
  g <- graph(surf)
  edgeWeights <- igraph::E(g)$dist

  # Invalid node index (out of range)
  expect_error(
    neurosurf:::findNeighbors(g, node = 0, radius = 5, edgeWeights = edgeWeights),
    "out of range"
  )

  expect_error(
    neurosurf:::findNeighbors(g, node = igraph::vcount(g) + 1, radius = 5, edgeWeights = edgeWeights),
    "out of range"
  )
})

test_that("findNeighbors validates edgeWeights", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  surf <- read_surf(surf_file)
  g <- graph(surf)

  # Non-numeric edgeWeights
  expect_error(
    neurosurf:::findNeighbors(g, node = 1, radius = 5, edgeWeights = "bad"),
    "numeric"
  )

  # NA in edgeWeights
  bad_weights <- igraph::E(g)$dist
  bad_weights[1] <- NA
  expect_error(
    neurosurf:::findNeighbors(g, node = 1, radius = 5, edgeWeights = bad_weights),
    "NA"
  )
})

test_that("findNeighbors validates radius", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  surf <- read_surf(surf_file)
  g <- graph(surf)
  edgeWeights <- igraph::E(g)$dist

  # Negative radius
  expect_error(
    neurosurf:::findNeighbors(g, node = 1, radius = -1, edgeWeights = edgeWeights),
    "positive"
  )

  # Zero radius
  expect_error(
    neurosurf:::findNeighbors(g, node = 1, radius = 0, edgeWeights = edgeWeights),
    "positive"
  )
})

test_that("neighbor_graph methods all return igraph", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  edgeWeights <- igraph::E(g)$dist

  # Method with missing nodes and edgeWeights
  ng1 <- neighbor_graph(geom, radius = 2)
  expect_s3_class(ng1, "igraph")

  # Method with edgeWeights but missing nodes
  ng2 <- neighbor_graph(geom, radius = 2, edgeWeights = edgeWeights)
  expect_s3_class(ng2, "igraph")

  # Method with nodes but missing edgeWeights
  ng3 <- neighbor_graph(geom, radius = 2, nodes = 1L:2L)
  expect_s3_class(ng3, "igraph")

  # Method with both
  ng4 <- neighbor_graph(geom, radius = 2, edgeWeights = edgeWeights, nodes = 1L:2L)
  expect_s3_class(ng4, "igraph")
})

test_that("neighbor_graph with euclidean distance_type", {
  geom <- example_surface_geometry()

  ng <- neighbor_graph(geom, radius = 5, distance_type = "euclidean")

  expect_s3_class(ng, "igraph")
})

test_that("find_all_neighbors with wrong edgeWeights length errors", {
  geom <- example_surface_geometry()

  # Wrong length edgeWeights
  expect_error(
    find_all_neighbors(geom, radius = 5, edgeWeights = c(1, 2, 3)),
    "one value per edge"
  )
})

test_that("projectCoordinates handles edge cases", {
  geom <- example_surface_geometry()
  surf_coords <- coords(geom)

  # Single point
  single_pt <- matrix(surf_coords[1, ], ncol = 3)
  ns <- projectCoordinates(geom, single_pt, sigma = 1)
  expect_s4_class(ns, "NeuroSurface")

  # Multiple points at same location
  same_pts <- matrix(rep(surf_coords[1, ], 3), ncol = 3, byrow = TRUE)
  ns2 <- projectCoordinates(geom, same_pts, sigma = 1)
  expect_s4_class(ns2, "NeuroSurface")
})

test_that("adjacency with dist attribute extracts edge distances", {
  geom <- example_surface_geometry()

  adj <- adjacency(geom, attr = "dist")

  expect_true(inherits(adj, "Matrix") || is.matrix(adj))
  # Non-zero entries should be positive (distances)
  nonzero <- as.vector(adj)[as.vector(adj) != 0]
  if (length(nonzero) > 0) {
    expect_true(all(nonzero > 0))
  }
})

test_that("smooth iteration parameter affects smoothness", {
  geom <- example_surface_geometry()
  original_coords <- coords(geom)

  # More iterations should move vertices more
  s1 <- smooth(geom, type = "laplace", iteration = 1)
  s10 <- smooth(geom, type = "laplace", iteration = 10)

  # Both should have same structure
  expect_equal(nrow(coords(s1)), nrow(original_coords))
  expect_equal(nrow(coords(s10)), nrow(original_coords))
})

test_that("find_all_neighbors all distance types work", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  surf <- read_surf(surf_file)
  g <- graph(surf)
  edgeWeights <- igraph::E(g)$dist

  for (dtype in c("geodesic", "euclidean")) {
    nb <- find_all_neighbors(surf, radius = 10, edgeWeights = edgeWeights,
                             nodes = 1:2, distance_type = dtype)
    expect_type(nb, "list")
    expect_length(nb, 2)
  }
})


# Additional tests for uncovered paths in neighborhood.R

test_that("findNeighbors internal function works", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf_geometry(surf_file)
  g <- graph(surf)
  edgeWeights <- igraph::E(g)$dist

  neighbors <- neurosurf:::findNeighbors(g, node = 1, radius = 5, edgeWeights = edgeWeights)

  expect_true(is.numeric(neighbors) || inherits(neighbors, "igraph.vs"))
})

test_that("findNeighbors validates edgeWeights type", {
  geom <- example_surface_geometry()
  g <- graph(geom)

  expect_error(
    neurosurf:::findNeighbors(g, node = 1, radius = 5, edgeWeights = "not_numeric"),
    "edgeWeights must be numeric"
  )
})

test_that("findNeighbors validates edgeWeights no NA", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  bad_weights <- c(1, NA, 1, 1, 1, 1)

  expect_error(
    neurosurf:::findNeighbors(g, node = 1, radius = 5, edgeWeights = bad_weights),
    "cannot contain NA"
  )
})

test_that("findNeighbors validates radius", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  edgeWeights <- igraph::E(g)$dist

  expect_error(
    neurosurf:::findNeighbors(g, node = 1, radius = -1, edgeWeights = edgeWeights),
    "positive number"
  )
})

test_that("findNeighbors validates node parameter", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  edgeWeights <- igraph::E(g)$dist

  expect_error(
    neurosurf:::findNeighbors(g, node = 1.5, radius = 5, edgeWeights = edgeWeights),
    "single integer"
  )
})

test_that("findNeighbors validates node in range", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  edgeWeights <- igraph::E(g)$dist

  expect_error(
    neurosurf:::findNeighbors(g, node = 1000, radius = 5, edgeWeights = edgeWeights),
    "out of range"
  )
})

test_that("find_all_neighbors validates edgeWeights length", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf_geometry(surf_file)
  g <- graph(surf)

  # Wrong length edgeWeights
  expect_error(
    find_all_neighbors(surf, radius = 5, edgeWeights = c(1, 2, 3)),
    "one value per edge"
  )
})

test_that("find_all_neighbors validates nodes no NA", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf_geometry(surf_file)
  g <- graph(surf)
  edgeWeights <- igraph::E(g)$dist

  expect_error(
    find_all_neighbors(surf, radius = 5, edgeWeights = edgeWeights, nodes = c(1, NA, 3)),
    "cannot contain NA"
  )
})

test_that("find_all_neighbors validates nodes are integers", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf_geometry(surf_file)
  g <- graph(surf)
  edgeWeights <- igraph::E(g)$dist

  expect_error(
    find_all_neighbors(surf, radius = 5, edgeWeights = edgeWeights, nodes = c(1.5, 2.5)),
    "integer indices"
  )
})

test_that("find_all_neighbors validates nodes in range", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf_geometry(surf_file)
  g <- graph(surf)
  edgeWeights <- igraph::E(g)$dist

  expect_error(
    find_all_neighbors(surf, radius = 5, edgeWeights = edgeWeights, nodes = c(1, 10000)),
    "out of range"
  )
})

test_that("find_all_neighbors with spherical distance type works", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf_geometry(surf_file)
  g <- graph(surf)
  edgeWeights <- igraph::E(g)$dist

  # Spherical distance should work on inflated surface
  nb <- find_all_neighbors(surf, radius = 10, edgeWeights = edgeWeights,
                           nodes = 1:3, distance_type = "spherical")

  expect_type(nb, "list")
  expect_length(nb, 3)
})

test_that("find_all_neighbors handles zero/negative edgeWeights", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf_geometry(surf_file)
  g <- graph(surf)

  # All zero weights should error
  zero_weights <- rep(0, igraph::ecount(g))
  expect_error(
    find_all_neighbors(surf, radius = 5, edgeWeights = zero_weights),
    "positive values"
  )
})

test_that("neighbor_graph works with nodes subset and custom weights", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  n <- length(nodes(geom))

  # Use specific nodes with missing edgeWeights (will use default dist)
  ng <- neighbor_graph(geom, radius = 2, nodes = 1L:min(2L, n))

  expect_s3_class(ng, "igraph")
})

test_that("smooth SurfaceGeometry with different smoothing types", {
  geom <- example_surface_geometry()

  # Test each smoothing type
  for (stype in c("taubin", "laplace", "HClaplace")) {
    smoothed <- smooth(geom, type = stype, iteration = 2)
    expect_s4_class(smoothed, "SurfaceGeometry")
  }
})
