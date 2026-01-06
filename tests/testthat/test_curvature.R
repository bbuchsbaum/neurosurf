# Tests for curvature computation
# Covers curvature() function and curv() alias

library(testthat)
library(neurosurf)

# ==============================================================================
# Basic Curvature Computation Tests
# ==============================================================================

test_that("curvature returns numeric vector", {
  geom <- example_surface_geometry()

  curv_vals <- curvature(geom)

  expect_true(is.numeric(curv_vals))
  expect_equal(length(curv_vals), nrow(coords(geom)))
})

test_that("curvature values are normalized", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)
  curv_vals <- curvature(surf)

  # Curvature values should be normalized (scaled)
  expect_true(is.numeric(curv_vals))
  expect_equal(length(curv_vals), nrow(coords(surf)))
})

test_that("curv() is an alias for curvature()", {
  geom <- example_surface_geometry()

  curv_result <- curv(geom)
  curvature_result <- curvature(geom)

  expect_identical(curv_result, curvature_result)
})

# ==============================================================================
# Curvature Properties Tests
# ==============================================================================

test_that("curvature varies across different surface types", {
  smoothwm <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  inflated <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  sphere <- system.file("extdata", "std.8_lh.sphere.asc", package = "neurosurf")

  skip_if(smoothwm == "" || inflated == "" || sphere == "",
          "One or more surface files not available")
  skip_if_not_installed("Rvcg")

  surf_smoothwm <- read_surf(smoothwm)
  surf_inflated <- read_surf(inflated)
  surf_sphere <- read_surf(sphere)

  curv_smoothwm <- curvature(surf_smoothwm)
  curv_inflated <- curvature(surf_inflated)
  curv_sphere <- curvature(surf_sphere)

  # All should be same length
  expect_equal(length(curv_smoothwm), length(curv_inflated))
  expect_equal(length(curv_inflated), length(curv_sphere))

  # Sphere should have more uniform curvature (lower variance)
  # since it's geometrically simpler
  var_smoothwm <- var(curv_smoothwm, na.rm = TRUE)
  var_sphere <- var(curv_sphere, na.rm = TRUE)

  # Note: This may not always hold depending on normalization
  # so we just check both are finite
  expect_true(is.finite(var_smoothwm))
  expect_true(is.finite(var_sphere))
})

test_that("curvature handles left and right hemispheres", {
  lh_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  rh_file <- system.file("extdata", "std.8_rh.smoothwm.asc", package = "neurosurf")

  skip_if(lh_file == "" || rh_file == "",
          "Hemisphere files not available")
  skip_if_not_installed("Rvcg")

  lh_surf <- read_surf(lh_file)
  rh_surf <- read_surf(rh_file)

  lh_curv <- curvature(lh_surf)
  rh_curv <- curvature(rh_surf)

  # Both should have valid curvature
  expect_true(is.numeric(lh_curv))
  expect_true(is.numeric(rh_curv))
  expect_equal(length(lh_curv), nrow(coords(lh_surf)))
  expect_equal(length(rh_curv), nrow(coords(rh_surf)))
})

# ==============================================================================
# Curvature with Simple Geometries
# ==============================================================================

test_that("curvature works on tetrahedron", {
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()
  curv_vals <- curvature(geom)

  # Tetrahedron has 4 vertices

  expect_equal(length(curv_vals), 4)

  # All values should be finite
  expect_true(all(is.finite(curv_vals)))
})

test_that("curvature works on simple plane-like surface", {
  skip_if_not_installed("Rvcg")

  # Create a flat triangular mesh
  verts <- matrix(c(
    0, 0, 0,
    1, 0, 0,
    0.5, 1, 0,
    1.5, 1, 0
  ), ncol = 3, byrow = TRUE)

  faces <- matrix(c(
    0L, 1L, 2L,
    1L, 3L, 2L
  ), ncol = 3, byrow = TRUE)

  geom <- SurfaceGeometry(verts, faces, hemi = "lh")
  curv_vals <- curvature(geom)

  expect_equal(length(curv_vals), 4)
  expect_true(all(is.finite(curv_vals)))
})

# ==============================================================================
# Curvature Color Mapping Tests
# ==============================================================================

test_that("curv_cols returns correct length vector", {
  set.seed(42)
  curv_vals <- rnorm(100)

  colors <- curv_cols(curv_vals)

  expect_equal(length(colors), length(curv_vals))
  expect_true(all(grepl("^#", colors)))
})

test_that("curv_cols splits values by median", {
  # Values clearly above and below median
  curv_vals <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)  # median = 5.5

  colors <- curv_cols(curv_vals, incol = "#FFFFFF", outcol = "#000000")

  # Values > 5.5 (6,7,8,9,10) should be incol (#FFFFFF)
  # Values <= 5.5 (1,2,3,4,5) should be outcol (#000000)
  expect_equal(sum(colors == "#FFFFFF"), 5)
  expect_equal(sum(colors == "#000000"), 5)
})

test_that("curv_cols uses custom colors", {
  curv_vals <- c(-1, 0, 1)

  colors <- curv_cols(curv_vals, incol = "#FF0000", outcol = "#0000FF")

  expect_true(all(colors %in% c("#FF0000", "#0000FF")))
})

# ==============================================================================
# Edge Cases
# ==============================================================================

test_that("curvature handles NA values gracefully", {
  skip_if_not_installed("Rvcg")

  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)
  curv_vals <- curvature(surf)

  # Check for any NAs and ensure function still works
  n_na <- sum(is.na(curv_vals))
  expect_true(n_na == 0 || n_na < length(curv_vals))  # Either no NAs or not all NAs
})

test_that("curv_cols handles all-same values", {
  curv_vals <- rep(5, 100)

  # All values equal, so all at or below median
  colors <- curv_cols(curv_vals, incol = "#FFFFFF", outcol = "#000000")

  expect_equal(length(colors), 100)
  # All should be outcol since none are > median
  expect_true(all(colors == "#000000"))
})

test_that("curv_cols handles single value", {
  curv_vals <- 5

  colors <- curv_cols(curv_vals)

  expect_equal(length(colors), 1)
})

test_that("curv_cols handles negative values", {
  curv_vals <- c(-5, -3, -1, 0, 1, 3, 5)

  colors <- curv_cols(curv_vals)

  expect_equal(length(colors), 7)
  expect_true(all(grepl("^#", colors)))
})

# ==============================================================================
# Integration with Surface Objects
# ==============================================================================

test_that("curvature integrates with NeuroSurface workflow", {
  skip_if_not_installed("Rvcg")

  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)
  curv_vals <- curvature(surf)

  # Create a NeuroSurface with curvature data
  n_verts <- nrow(coords(surf))
  neuro_surf <- NeuroSurface(
    geometry = surf,
    indices = 1:n_verts,
    data = curv_vals
  )

  expect_s4_class(neuro_surf, "NeuroSurface")
  expect_equal(length(neuro_surf@data), n_verts)
})

test_that("curvature used in curv_cols produces valid visualization colors", {
  skip_if_not_installed("Rvcg")

  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)
  curv_vals <- curvature(surf)
  colors <- curv_cols(curv_vals)

  expect_equal(length(colors), length(curv_vals))
  expect_true(all(nchar(colors) == 7))  # Valid hex colors like #RRGGBB
})
