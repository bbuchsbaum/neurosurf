# Comprehensive tests for surface smoothing functionality
# Covers basic smoothing, edge cases, and parameter validation

library(testthat)
library(neurosurf)

# ==============================================================================
# Basic Smoothing Tests
# ==============================================================================

test_that("smoothing radius influences output", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf_geometry(surf_file)
  n_vertices <- nrow(coords(surf))
  set.seed(42)
  data_vals <- rnorm(n_vertices)
  nsurf <- NeuroSurface(geometry = surf, indices = 1:n_vertices, data = data_vals)

  sm_small <- smooth(nsurf, sigma = 2)
  sm_large <- smooth(nsurf, sigma = 6)

  sd_small <- sd(as.vector(series(sm_small, indices(sm_small))))
  sd_large <- sd(as.vector(series(sm_large, indices(sm_large))))

  expect_true(sd_large < sd_small)
  expect_false(isTRUE(all.equal(as.vector(series(sm_small, indices(sm_small))),
                                as.vector(series(sm_large, indices(sm_large))))))
})

test_that("smoothing returns NeuroSurface object", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)
  n_vertices <- nrow(coords(surf))
  set.seed(42)
  data_vals <- rnorm(n_vertices)
  nsurf <- NeuroSurface(geometry = surf, indices = 1:n_vertices, data = data_vals)

  smoothed <- smooth(nsurf, sigma = 4)

  expect_s4_class(smoothed, "NeuroSurface")
  expect_equal(length(indices(smoothed)), n_vertices)
})

test_that("smoothing preserves geometry", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)
  n_vertices <- nrow(coords(surf))
  data_vals <- rnorm(n_vertices)
  nsurf <- NeuroSurface(geometry = surf, indices = 1:n_vertices, data = data_vals)

  smoothed <- smooth(nsurf, sigma = 4)

  # Geometry should be unchanged
  expect_equal(nrow(coords(smoothed@geometry)), nrow(coords(surf)))
})

# ==============================================================================
# Smoothing with Different Sigma Values
# ==============================================================================

test_that("smoothing with very small sigma has minimal effect", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)
  n_vertices <- nrow(coords(surf))
  set.seed(42)
  data_vals <- rnorm(n_vertices)
  nsurf <- NeuroSurface(geometry = surf, indices = 1:n_vertices, data = data_vals)

  # Very small sigma
  smoothed <- smooth(nsurf, sigma = 0.1)

  smoothed_data <- series(smoothed, indices(smoothed))
  original_data <- series(nsurf, indices(nsurf))

  # Correlation should be very high (almost no change)
  cor_val <- cor(smoothed_data, original_data)
  expect_true(cor_val > 0.99)
})

test_that("smoothing with increasing sigma progressively reduces variance", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)
  n_vertices <- nrow(coords(surf))
  set.seed(42)
  data_vals <- rnorm(n_vertices)
  nsurf <- NeuroSurface(geometry = surf, indices = 1:n_vertices, data = data_vals)

  sigmas <- c(1, 2, 4, 8)
  variances <- numeric(length(sigmas))

  for (i in seq_along(sigmas)) {
    smoothed <- smooth(nsurf, sigma = sigmas[i])
    smoothed_data <- series(smoothed, indices(smoothed))
    variances[i] <- var(smoothed_data)
  }

  # Variances should decrease with increasing sigma
  for (i in 2:length(variances)) {
    expect_true(variances[i] < variances[i - 1],
                info = paste("Variance at sigma", sigmas[i], "should be less than at sigma", sigmas[i - 1]))
  }
})

# ==============================================================================
# Edge Cases
# ==============================================================================

test_that("smoothing handles constant data", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)
  n_vertices <- nrow(coords(surf))

  # All values are the same
  constant_val <- 5.0
  data_vals <- rep(constant_val, n_vertices)
  nsurf <- NeuroSurface(geometry = surf, indices = 1:n_vertices, data = data_vals)

  smoothed <- smooth(nsurf, sigma = 4)
  smoothed_data <- series(smoothed, indices(smoothed))

  # Smoothing constant data should return constant data
  expect_true(all(abs(smoothed_data - constant_val) < 1e-6))
})

test_that("smoothing handles data with single outlier", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)
  n_vertices <- nrow(coords(surf))

  # All zeros except one large value
  data_vals <- rep(0, n_vertices)
  data_vals[100] <- 100  # Single outlier

  nsurf <- NeuroSurface(geometry = surf, indices = 1:n_vertices, data = data_vals)

  smoothed <- smooth(nsurf, sigma = 4)
  smoothed_data <- series(smoothed, indices(smoothed))

  # Outlier should be reduced
  expect_true(smoothed_data[100] < 100)

  # Neighbors should have picked up some value
  expect_true(sum(smoothed_data > 0) > 1)
})

test_that("smoothing handles negative values correctly", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)
  n_vertices <- nrow(coords(surf))

  # Mix of positive and negative values
  set.seed(42)
  data_vals <- rnorm(n_vertices, mean = -5, sd = 2)
  nsurf <- NeuroSurface(geometry = surf, indices = 1:n_vertices, data = data_vals)

  smoothed <- smooth(nsurf, sigma = 4)
  smoothed_data <- series(smoothed, indices(smoothed))

  # Mean should be preserved approximately
  expect_true(abs(mean(smoothed_data) - mean(data_vals)) < 0.5)
})

test_that("smoothing handles sparse data pattern", {
  geom <- example_surface_geometry()
  n_vertices <- nrow(coords(geom))

  # Only odd indices have non-zero values
  data_vals <- numeric(n_vertices)
  data_vals[c(1, 3)] <- c(10, 20)

  nsurf <- NeuroSurface(geometry = geom, indices = 1:n_vertices, data = data_vals)

  smoothed <- smooth(nsurf, sigma = 1)
  smoothed_data <- series(smoothed, indices(smoothed))

  # All values should now have some contribution
  expect_true(sum(smoothed_data != 0) >= 2)
})

# ==============================================================================
# Smoothing on Simple Geometries
# ==============================================================================

test_that("smoothing works on tetrahedron", {
  geom <- example_surface_geometry()
  n_vertices <- 4
  data_vals <- c(1, 2, 3, 4)

  nsurf <- NeuroSurface(geometry = geom, indices = 1:n_vertices, data = data_vals)

  smoothed <- smooth(nsurf, sigma = 1)

  expect_s4_class(smoothed, "NeuroSurface")
  expect_equal(length(indices(smoothed)), 4)

  # Smoothed values should be more similar to each other
  smoothed_data <- series(smoothed, indices(smoothed))
  expect_true(sd(smoothed_data) < sd(data_vals))
})

# ==============================================================================
# Reproducibility Tests
# ==============================================================================

test_that("smoothing is deterministic", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)
  n_vertices <- nrow(coords(surf))
  set.seed(42)
  data_vals <- rnorm(n_vertices)
  nsurf <- NeuroSurface(geometry = surf, indices = 1:n_vertices, data = data_vals)

  smoothed1 <- smooth(nsurf, sigma = 4)
  smoothed2 <- smooth(nsurf, sigma = 4)

  data1 <- series(smoothed1, indices(smoothed1))
  data2 <- series(smoothed2, indices(smoothed2))

  expect_equal(data1, data2)
})

# ==============================================================================
# Smoothing Different Surface Types
# ==============================================================================

test_that("smoothing works on different surface types", {
  surf_types <- c("smoothwm", "pial", "inflated")

  for (stype in surf_types) {
    fname <- sprintf("std.8_lh.%s.asc", stype)
    surf_file <- system.file("extdata", fname, package = "neurosurf")

    if (surf_file != "") {
      surf <- read_surf(surf_file)
      n_vertices <- nrow(coords(surf))
      set.seed(42)
      data_vals <- rnorm(n_vertices)
      nsurf <- NeuroSurface(geometry = surf, indices = 1:n_vertices, data = data_vals)

      smoothed <- smooth(nsurf, sigma = 4)

      expect_s4_class(smoothed, "NeuroSurface",
                      info = paste("Failed for surface type:", stype))
    }
  }
})

# ==============================================================================
# Mean Preservation Tests
# ==============================================================================

test_that("smoothing approximately preserves mean", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)
  n_vertices <- nrow(coords(surf))
  set.seed(42)
  data_vals <- rnorm(n_vertices, mean = 10, sd = 5)
  nsurf <- NeuroSurface(geometry = surf, indices = 1:n_vertices, data = data_vals)

  smoothed <- smooth(nsurf, sigma = 4)
  smoothed_data <- series(smoothed, indices(smoothed))

  original_mean <- mean(data_vals)
  smoothed_mean <- mean(smoothed_data)

  # Mean should be preserved within reasonable tolerance
  expect_true(abs(smoothed_mean - original_mean) < 0.5)
})

# ==============================================================================
# Multiple Smoothing Applications
# ==============================================================================

test_that("repeated smoothing further reduces variance", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)
  n_vertices <- nrow(coords(surf))
  set.seed(42)
  data_vals <- rnorm(n_vertices)
  nsurf <- NeuroSurface(geometry = surf, indices = 1:n_vertices, data = data_vals)

  # First smoothing
  smooth1 <- smooth(nsurf, sigma = 2)
  var1 <- var(series(smooth1, indices(smooth1)))

  # Second smoothing (on already smoothed data)
  smooth2 <- smooth(smooth1, sigma = 2)
  var2 <- var(series(smooth2, indices(smooth2)))

  expect_true(var2 < var1)
})

# ==============================================================================
# Boundary Behavior Tests
# ==============================================================================

test_that("smoothing handles vertices with few neighbors", {
  # Example geometry has vertices that might have different neighbor counts
  geom <- example_surface_geometry()
  n_vertices <- nrow(coords(geom))
  data_vals <- 1:n_vertices

  nsurf <- NeuroSurface(geometry = geom, indices = 1:n_vertices, data = as.numeric(data_vals))

  # Should complete without error
  smoothed <- smooth(nsurf, sigma = 1)

  expect_s4_class(smoothed, "NeuroSurface")
  expect_true(all(is.finite(series(smoothed, indices(smoothed)))))
})
