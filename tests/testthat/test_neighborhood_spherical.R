library(neurosurf)

test_that("find_all_neighbors spherical distances produce no NaN", {
  surf_file <- system.file("extdata", "std.8_lh.sphere.asc", package="neurosurf")
  surf <- read_surf(surf_file)
  g <- graph(surf)
  edgeWeights <- igraph::E(g)$dist
  nb <- find_all_neighbors(surf, radius = 12, edgeWeights = edgeWeights,
                           nodes = 1:5, distance_type = "spherical")
  dvals <- unlist(lapply(nb, function(x) x[ ,"d"]))
  expect_false(any(is.nan(dvals)))
})

