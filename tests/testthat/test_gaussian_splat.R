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
