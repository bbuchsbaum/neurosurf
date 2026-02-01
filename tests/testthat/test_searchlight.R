library(testthat)
library(neurosurf)

test_that("SurfaceSearchlight iterator works correctly", {
  # Load test surface
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)
  
  # Create iterator
  sl <- SurfaceSearchlight(surf, radius=12)
  
  # Test basic properties
  expect_true(inherits(sl, "Searchlight"))
  expect_true(inherits(sl, "iter"))
  
  # Get first region
  nodes <- sl$nextElem()

  # Test returned nodes
  expect_true(is.numeric(nodes))
  expect_true(length(nodes) > 1)
  expect_false(is.null(attr(nodes, "center")))
  expect_false(is.null(attr(nodes, "length")))

  # Progress should reflect number of centers visited
  num_nodes <- length(nodes(surf))
  expect_equal(sl$progress(), 1 / num_nodes)
})

test_that("SurfaceSearchlight deflist works correctly", {
  # Load test surface
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)
  
  # Create deflist
  sl <- SurfaceSearchlight(surf, radius=12, as_deflist=TRUE)
  
  # Test basic properties
  expect_true(inherits(sl, "deflist"))
  
  # Get first region
  nodes <- sl[[1]]
  
  # Test returned nodes
  expect_true(is.numeric(nodes))
  expect_true(length(nodes) > 1)
  expect_false(is.null(attr(nodes, "center")))
  expect_false(is.null(attr(nodes, "length")))
})

test_that("RandomSurfaceSearchlight iterator works correctly", {
  # Load test surface
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)
  
  # Create iterator
  sl <- RandomSurfaceSearchlight(surf, radius=12)
  
  # Test basic properties
  expect_true(inherits(sl, "RandomSurfaceSearchlight"))
  expect_true(inherits(sl, "iter"))
  
  # Get first region
  nodes <- sl$nextElem()

  # Test returned nodes
  expect_true(is.numeric(nodes))
  expect_true(length(nodes) > 1)
  expect_false(is.null(attr(nodes, "center")))
  expect_false(is.null(attr(nodes, "length")))

  # Progress should reflect proportion of nodes covered
  num_nodes <- length(nodes(surf))
  expect_equal(sl$progress(), attr(nodes, "length") / num_nodes)
})

test_that("RandomSurfaceSearchlight deflist works correctly", {
  # Load test surface
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)
  
  # Create deflist
  sl <- RandomSurfaceSearchlight(surf, radius=12, as_deflist=TRUE)
  
  # Test basic properties
  expect_true(inherits(sl, "deflist"))
  
  # Get first region
  nodes <- sl[[1]]
  
  # Test returned nodes
  expect_true(is.numeric(nodes))
  expect_true(length(nodes) > 1)
  expect_false(is.null(attr(nodes, "center")))
  expect_false(is.null(attr(nodes, "length")))
})


test_that("RandomSurfaceSearchlight deflist order changes with seed", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  set.seed(123)
  sl1 <- RandomSurfaceSearchlight(surf, radius=12, as_deflist=TRUE)
  centers1 <- vapply(seq_len(length(sl1)), function(i) attr(sl1[[i]], "center.index"), numeric(1))

  set.seed(124)
  sl2 <- RandomSurfaceSearchlight(surf, radius=12, as_deflist=TRUE)
  centers2 <- vapply(seq_len(length(sl2)), function(i) attr(sl2[[i]], "center.index"), numeric(1))

  expect_false(identical(centers1, centers2))
})

test_that("SurfaceSearchlight deflist rejects invalid indices", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)
  sl <- SurfaceSearchlight(surf, radius=12, as_deflist=TRUE)
  expect_error(sl[[0]])
  expect_error(sl[[-1]])
})

test_that("RandomSurfaceSearchlight deflist rejects invalid indices", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)
  sl <- RandomSurfaceSearchlight(surf, radius=12, as_deflist=TRUE)
  expect_error(sl[[0]])
  expect_error(sl[[-1]])
})

test_that("RandomSurfaceSearchlight validates inputs", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  expect_error(RandomSurfaceSearchlight(surf, radius=c(1,2)))
  expect_error(RandomSurfaceSearchlight(surf, radius=-1))
  expect_error(RandomSurfaceSearchlight(surf, radius=8, nodeset=integer()))
})

test_that("RandomSurfaceSearchlight deflist uses random ordering", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  set.seed(1)
  sl1 <- RandomSurfaceSearchlight(surf, radius=12, as_deflist=TRUE)
  first1 <- attr(sl1[[1]], "center")

  set.seed(2)
  sl2 <- RandomSurfaceSearchlight(surf, radius=12, as_deflist=TRUE)
  first2 <- attr(sl2[[1]], "center")

  expect_false(identical(first1, first2))

})

test_that("SurfaceSearchlight with geodesic distance_type works", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  sl <- SurfaceSearchlight(surf, radius=12, distance_type = "geodesic")
  expect_true(inherits(sl, "Searchlight"))

  nodes <- sl$nextElem()
  expect_true(is.numeric(nodes))
  expect_true(length(nodes) > 1)
})

test_that("SurfaceSearchlight with euclidean distance_type works", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  sl <- SurfaceSearchlight(surf, radius=12, distance_type = "euclidean")
  expect_true(inherits(sl, "Searchlight"))

  nodes <- sl$nextElem()
  expect_true(is.numeric(nodes))
})

test_that("SurfaceSearchlight with nodeset works", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  # Use a subset of nodes
  nodeset <- 1:50
  sl <- SurfaceSearchlight(surf, radius=12, nodeset = nodeset)

  expect_true(inherits(sl, "Searchlight"))

  nodes <- sl$nextElem()
  # All returned nodes should be within the nodeset
  expect_true(all(nodes %in% nodeset))
})

test_that("RandomSurfaceSearchlight with nodeset works", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  # Use a subset of nodes
  nodeset <- 1:50
  sl <- RandomSurfaceSearchlight(surf, radius=12, nodeset = nodeset)

  expect_true(inherits(sl, "RandomSurfaceSearchlight"))

  nodes <- sl$nextElem()
  # All returned nodes should be within the nodeset
  expect_true(all(nodes %in% nodeset))
})

test_that("SurfaceSearchlight iterator reports index", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  sl <- SurfaceSearchlight(surf, radius=12)

  expect_equal(sl$index(), 0)
  sl$nextElem()
  expect_equal(sl$index(), 1)
  sl$nextElem()
  expect_equal(sl$index(), 2)
})

test_that("print.Searchlight works", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  sl <- SurfaceSearchlight(surf, radius=12)

  # Should not error
  expect_output(print(sl), "Searchlight|progress")
})

test_that("SurfaceSearchlight validates radius", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  expect_error(SurfaceSearchlight(surf, radius = c(1, 2)))
})

test_that("SurfaceSearchlight validates nodeset length", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  # nodeset with only 1 element should fail
  expect_error(SurfaceSearchlight(surf, radius = 12, nodeset = 1))
})

# Additional comprehensive tests

test_that("SurfaceSearchlight throws StopIteration when exhausted", {
  geom <- example_surface_geometry()  # Small geometry with 4 vertices

  sl <- SurfaceSearchlight(geom, radius=100)  # Large radius

  # Iterate through all nodes
  n_nodes <- length(nodes(geom))
  for (i in seq_len(n_nodes)) {
    sl$nextElem()
  }

  expect_error(sl$nextElem(), "StopIteration")
})

test_that("Searchlight nodes have center.index attribute", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  sl <- SurfaceSearchlight(surf, radius=12)
  nodes_result <- sl$nextElem()

  expect_false(is.null(attr(nodes_result, "center.index")))
  expect_true(is.numeric(attr(nodes_result, "center.index")))
  expect_equal(attr(nodes_result, "center"), attr(nodes_result, "center.index"))
})

test_that("Searchlight nodes have correct length attribute", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  sl <- SurfaceSearchlight(surf, radius=12)
  nodes_result <- sl$nextElem()

  expect_equal(attr(nodes_result, "length"), length(nodes_result))
})

test_that("SurfaceSearchlight deflist works with nodeset", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  nodeset <- 1:50
  sl <- SurfaceSearchlight(surf, radius=12, nodeset=nodeset, as_deflist=TRUE)

  expect_true(inherits(sl, "deflist"))
  expect_equal(length(sl), length(nodeset))

  # First element should have nodes from nodeset
  nodes_result <- sl[[1]]
  expect_true(all(nodes_result %in% nodeset))
})

test_that("RandomSurfaceSearchlight deflist works with nodeset", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  nodeset <- 1:50
  sl <- RandomSurfaceSearchlight(surf, radius=12, nodeset=nodeset, as_deflist=TRUE)

  expect_true(inherits(sl, "deflist"))
  expect_equal(length(sl), length(nodeset))

  nodes_result <- sl[[1]]
  expect_true(all(nodes_result %in% nodeset))
})

test_that("RandomSurfaceSearchlight is reproducible with set.seed", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  set.seed(42)
  sl1 <- RandomSurfaceSearchlight(surf, radius=12, as_deflist=TRUE)
  centers1 <- vapply(1:5, function(i) attr(sl1[[i]], "center"), numeric(1))

  set.seed(42)
  sl2 <- RandomSurfaceSearchlight(surf, radius=12, as_deflist=TRUE)
  centers2 <- vapply(1:5, function(i) attr(sl2[[i]], "center"), numeric(1))

  expect_equal(centers1, centers2)
})

test_that("SurfaceSearchlight progress reflects iteration count", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  sl <- SurfaceSearchlight(surf, radius=12)
  n_nodes <- length(nodes(surf))

  # Initial progress should be 0
  expect_equal(sl$progress(), 0)

  # After 3 iterations
  sl$nextElem()
  sl$nextElem()
  sl$nextElem()
  expect_equal(sl$progress(), 3 / n_nodes)
})

test_that("SurfaceSearchlight with spherical distance works", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  sl <- SurfaceSearchlight(surf, radius=12, distance_type = "spherical")
  expect_true(inherits(sl, "Searchlight"))

  nodes_result <- sl$nextElem()
  expect_true(is.numeric(nodes_result))
  expect_true(length(nodes_result) >= 1)
})

test_that("SurfaceSearchlight deflist rejects out of bounds index", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  sl <- SurfaceSearchlight(surf, radius=12, as_deflist=TRUE)
  n <- length(sl)

  expect_error(sl[[n + 1]])
})

test_that("RandomSurfaceSearchlight deflist rejects out of bounds index", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  sl <- RandomSurfaceSearchlight(surf, radius=12, as_deflist=TRUE)
  n <- length(sl)

  expect_error(sl[[n + 1]])
})

test_that("Searchlight nodeset returns center in nodeset range", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  nodeset <- 10:60
  sl <- SurfaceSearchlight(surf, radius=12, nodeset=nodeset)

  nodes_result <- sl$nextElem()
  center <- attr(nodes_result, "center")

  expect_true(center %in% nodeset)
})

test_that("RandomSurfaceSearchlight nodeset returns center in nodeset range", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
  surf <- read_surf(surf_file)

  nodeset <- 10:60
  sl <- RandomSurfaceSearchlight(surf, radius=12, nodeset=nodeset)

  nodes_result <- sl$nextElem()
  center <- attr(nodes_result, "center")

  expect_true(center %in% nodeset)
})
