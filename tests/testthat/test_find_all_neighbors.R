test_that("find_all_neighbors validates inputs", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  surf <- read_surf(surf_file)
  g <- graph(surf)
  edge_weights <- rep(1, length(E(g)))

  expect_error(find_all_neighbors(surf, radius = 0, edgeWeights = edge_weights))
  expect_error(find_all_neighbors(surf, radius = -1, edgeWeights = edge_weights))
  bad_weights <- edge_weights[-1]
  expect_error(find_all_neighbors(surf, radius = 5, edgeWeights = bad_weights))
  expect_error(find_all_neighbors(surf, radius = 5, edgeWeights = "a"))
})
