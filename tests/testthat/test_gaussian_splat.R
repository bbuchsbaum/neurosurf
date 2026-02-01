test_that("find_nearest_vertex returns expected index", {
  geom <- example_surface_geometry()
  idx <- find_nearest_vertex(geom, c(0.9, 0, 0))
  expect_equal(idx, 2L)
})


test_that("gaussian_splat computes Euclidean kernel", {
  geom <- example_surface_geometry()
  splat <- gaussian_splat(geom, center = c(0, 0, 0), sigma = 1)

  expect_s4_class(splat, "NeuroSurface")
  expect_equal(length(splat@data), igraph::vcount(graph(geom)))
  expect_equal(splat@data[1], 1)
  expect_equal(splat@data[2], exp(-0.5), tolerance = 1e-6)
  expect_equal(splat@data[3], exp(-0.5), tolerance = 1e-6)
  expect_equal(splat@data[4], exp(-0.5), tolerance = 1e-6)
})


test_that("geodesic splat uses nearest vertex centre", {
  geom <- example_surface_geometry()
  center <- c(0.1, 0, 0)
  idx <- find_nearest_vertex(geom, center)

  geo <- gaussian_splat(
    geom,
    center = center,
    sigma = 1,
    use_geodesic = TRUE
  )
  vertex_geo <- gaussian_splat_vertex(
    geom,
    vertex_idx = idx,
    sigma = 1,
    use_geodesic = TRUE
  )

  expect_equal(geo@data, vertex_geo@data)
})


test_that("gaussian_splat_multi sums weighted kernels", {
  geom <- example_surface_geometry()
  centers <- rbind(c(0, 0, 0), c(1, 0, 0))
  sigmas <- c(0.5, 1.5)
  weights <- c(1, 2)

  multi <- gaussian_splat_multi(
    geom,
    centers = centers,
    sigmas = sigmas,
    weights = weights,
    use_geodesic = FALSE
  )
  g1 <- gaussian_splat(geom, center = centers[1, ], sigma = sigmas[1])
  g2 <- gaussian_splat(geom, center = centers[2, ], sigma = sigmas[2])

  expect_equal(
    multi@data,
    g1@data * weights[1] + g2@data * weights[2],
    tolerance = 1e-10
  )
})

# Additional tests for gaussian_splat.R coverage

test_that("gaussian_splat_vertex with Euclidean distance works", {
  geom <- example_surface_geometry()
  splat <- gaussian_splat_vertex(geom, vertex_idx = 1, sigma = 1, use_geodesic = FALSE)

  expect_s4_class(splat, "NeuroSurface")
  expect_equal(splat@data[1], 1)  # Center vertex has value 1
})

test_that("gaussian_splat_vertex with geodesic distance works", {
  geom <- example_surface_geometry()
  splat <- gaussian_splat_vertex(geom, vertex_idx = 1, sigma = 1, use_geodesic = TRUE)

  expect_s4_class(splat, "NeuroSurface")
  expect_equal(splat@data[1], 1)  # Center vertex has value 1
})

test_that("gaussian_splat_multi with geodesic distance works", {
  geom <- example_surface_geometry()
  centers <- rbind(c(0, 0, 0), c(1, 0, 0))
  sigmas <- c(0.5, 1.5)

  multi <- gaussian_splat_multi(
    geom,
    centers = centers,
    sigmas = sigmas,
    use_geodesic = TRUE
  )

  expect_s4_class(multi, "NeuroSurface")
  expect_equal(length(multi@data), nrow(coords(geom)))
})

test_that("gaussian_splat_multi with single sigma works", {
  geom <- example_surface_geometry()
  centers <- rbind(c(0, 0, 0), c(1, 0, 0))

  multi <- gaussian_splat_multi(geom, centers = centers, sigmas = 1)

  expect_s4_class(multi, "NeuroSurface")
})

test_that("gaussian_splat_multi with single weight works", {
  geom <- example_surface_geometry()
  centers <- rbind(c(0, 0, 0), c(1, 0, 0))

  multi <- gaussian_splat_multi(geom, centers = centers, sigmas = c(1, 1), weights = 2)

  expect_s4_class(multi, "NeuroSurface")
})

test_that("gaussian_splat_multi with NULL weights defaults to 1", {
  geom <- example_surface_geometry()
  centers <- rbind(c(0, 0, 0))

  multi_null <- gaussian_splat_multi(geom, centers = centers, sigmas = 1, weights = NULL)
  multi_one <- gaussian_splat_multi(geom, centers = centers, sigmas = 1, weights = 1)

  expect_equal(multi_null@data, multi_one@data)
})

test_that("find_nearest_vertex finds correct vertex", {
  geom <- example_surface_geometry()

  # Vertex 1 is at (0,0,0)
  idx1 <- find_nearest_vertex(geom, c(0, 0, 0))
  expect_equal(idx1, 1L)

  # Vertex 2 is at (1,0,0)
  idx2 <- find_nearest_vertex(geom, c(1, 0, 0))
  expect_equal(idx2, 2L)
})

# Validation error tests

test_that("gaussian_splat errors on invalid surface", {
  expect_error(gaussian_splat(list(), c(0, 0, 0), 1), "SurfaceGeometry")
})

test_that("gaussian_splat errors on invalid center", {
  geom <- example_surface_geometry()

  expect_error(gaussian_splat(geom, c(0, 0), 1), "length 3")
  expect_error(gaussian_splat(geom, c(NA, 0, 0), 1), "length 3")
})

test_that("gaussian_splat errors on invalid sigma", {
  geom <- example_surface_geometry()

  expect_error(gaussian_splat(geom, c(0, 0, 0), -1), "positive")
  expect_error(gaussian_splat(geom, c(0, 0, 0), 0), "positive")
  expect_error(gaussian_splat(geom, c(0, 0, 0), NA), "positive")
  expect_error(gaussian_splat(geom, c(0, 0, 0), c(1, 2)), "single")
})

test_that("gaussian_splat_vertex errors on invalid vertex_idx", {
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))

  expect_error(gaussian_splat_vertex(geom, 0, 1), "out of range")
  expect_error(gaussian_splat_vertex(geom, n + 1, 1), "out of range")
  expect_error(gaussian_splat_vertex(geom, NA, 1), "single integer")
  expect_error(gaussian_splat_vertex(geom, 1.5, 1), "single integer")
  expect_error(gaussian_splat_vertex(geom, c(1, 2), 1), "single integer")
})

test_that("gaussian_splat_multi errors on empty centers", {
  geom <- example_surface_geometry()
  empty_centers <- matrix(numeric(0), ncol = 3)

  expect_error(gaussian_splat_multi(geom, empty_centers, 1), "at least one row")
})

test_that("gaussian_splat_multi errors on wrong center columns", {
  geom <- example_surface_geometry()
  bad_centers <- matrix(1:4, ncol = 2)

  expect_error(gaussian_splat_multi(geom, bad_centers, 1), "three columns")
})

test_that("gaussian_splat_multi errors on NA centers", {
  geom <- example_surface_geometry()
  na_centers <- matrix(c(0, 0, NA), ncol = 3)

  expect_error(gaussian_splat_multi(geom, na_centers, 1), "missing values")
})

test_that("gaussian_splat_multi errors on wrong sigmas length", {
  geom <- example_surface_geometry()
  centers <- rbind(c(0, 0, 0), c(1, 0, 0))

  expect_error(
    gaussian_splat_multi(geom, centers, c(1, 2, 3)),
    "match the number of centers"
  )
})

test_that("gaussian_splat_multi errors on negative sigmas", {
  geom <- example_surface_geometry()
  centers <- rbind(c(0, 0, 0))

  expect_error(gaussian_splat_multi(geom, centers, -1), "positive")
})

test_that("gaussian_splat_multi errors on NA sigmas", {
  geom <- example_surface_geometry()
  centers <- rbind(c(0, 0, 0))

  expect_error(gaussian_splat_multi(geom, centers, NA), "non-missing")
})

test_that("gaussian_splat_multi errors on wrong weights length", {
  geom <- example_surface_geometry()
  centers <- rbind(c(0, 0, 0), c(1, 0, 0))

  expect_error(
    gaussian_splat_multi(geom, centers, c(1, 1), weights = c(1, 2, 3)),
    "match the number of centers"
  )
})

test_that("gaussian_splat_multi errors on NA weights", {
  geom <- example_surface_geometry()
  centers <- rbind(c(0, 0, 0))

  expect_error(gaussian_splat_multi(geom, centers, 1, weights = NA), "non-missing")
})

test_that("gaussian_splat errors on invalid use_geodesic", {
  geom <- example_surface_geometry()

  expect_error(gaussian_splat(geom, c(0, 0, 0), 1, use_geodesic = NA), "logical")
  expect_error(gaussian_splat(geom, c(0, 0, 0), 1, use_geodesic = "yes"), "logical")
})

test_that("gaussian_splat_multi accepts vector as single center", {
  geom <- example_surface_geometry()

  # Single vector should be converted to 1-row matrix
  multi <- gaussian_splat_multi(geom, centers = c(0, 0, 0), sigmas = 1)

  expect_s4_class(multi, "NeuroSurface")
})
