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
