test_that("optimized C++ boundary detection produces correct results", {
  skip_on_cran()

  # Simple cube mesh with two ROIs (bottom vs top)
  vertices <- matrix(c(
    0,0,0, 1,0,0, 1,1,0, 0,1,0,
    0,0,1, 1,0,1, 1,1,1, 0,1,1
  ), ncol = 3, byrow = TRUE)

  faces <- matrix(c(
    1,2,3, 1,3,4,
    5,6,7, 5,7,8,
    1,2,6, 1,6,5,
    3,4,8, 3,8,7,
    1,4,8, 1,8,5,
    2,3,7, 2,7,6
  ), ncol = 3, byrow = TRUE)

  roi <- c(1,1,1,1, 2,2,2,2)

  # Test the new optimized function
  result_cpp <- find_roi_boundaries(vertices, faces, roi,
                                    boundary_method = "edge_vertices",
                                    use_cpp = TRUE,
                                    verbose = FALSE)

  # Test the old R+igraph implementation
  result_r <- find_roi_boundaries(vertices, faces, roi,
                                  boundary_method = "edge_vertices",
                                  use_cpp = FALSE,
                                  verbose = FALSE)

  # Both should have same number of boundaries
  expect_equal(length(result_cpp$boundary), length(result_r$boundary))

  # Both should have same ROI IDs
  expect_equal(sort(result_cpp$boundary_roi_id), sort(result_r$boundary_roi_id))

  # Both should have same number of components per ROI
  expect_equal(result_cpp$roi_components, result_r$roi_components)

  # Check that boundaries are valid (have at least 3 vertices)
  for (b in result_cpp$boundary) {
    expect_true(nrow(b) >= 3)
  }
})

test_that("optimized implementation handles edge cases", {
  skip_on_cran()

  # Single ROI - no boundaries
  vertices <- matrix(c(
    0,0,0, 1,0,0, 1,1,0, 0,1,0
  ), ncol = 3, byrow = TRUE)

  faces <- matrix(c(
    1,2,3, 1,3,4
  ), ncol = 3, byrow = TRUE)

  roi <- c(1,1,1,1)

  result <- find_roi_boundaries(vertices, faces, roi,
                                boundary_method = "edge_vertices",
                                use_cpp = TRUE,
                                verbose = FALSE)

  # Should have no boundaries
  expect_equal(length(result$boundary), 0)
})

# Benchmark test (only runs if benchmark package is available)
test_that("optimized implementation performance (benchmark)", {
  skip_on_cran()
  skip_if_not_installed("bench")

  # Create a larger test mesh (simple grid)
  n <- 20  # grid size
  vertices <- expand.grid(x = 1:n, y = 1:n)
  vertices$z <- 0
  vertices <- as.matrix(vertices)

  # Create triangular faces for the grid
  faces <- matrix(0, nrow = 2 * (n-1) * (n-1), ncol = 3)
  idx <- 1
  for (i in 1:(n-1)) {
    for (j in 1:(n-1)) {
      v1 <- (i-1)*n + j
      v2 <- (i-1)*n + j + 1
      v3 <- i*n + j
      v4 <- i*n + j + 1

      faces[idx, ] <- c(v1, v2, v3)
      faces[idx+1, ] <- c(v2, v4, v3)
      idx <- idx + 2
    }
  }

  # Create ROI pattern (checkerboard)
  roi <- rep(0, nrow(vertices))
  for (i in 1:nrow(vertices)) {
    x <- vertices[i, 1]
    y <- vertices[i, 2]
    roi[i] <- ((floor(x/5) + floor(y/5)) %% 2) + 1
  }

  # Benchmark
  cat("\n=== Performance Benchmark ===\n")
  cat("Mesh size:", nrow(vertices), "vertices,", nrow(faces), "faces\n")
  cat("Number of ROIs:", length(unique(roi)), "\n\n")

  bm <- bench::mark(
    cpp = find_roi_boundaries(vertices, faces, roi,
                              boundary_method = "edge_vertices",
                              use_cpp = TRUE,
                              verbose = FALSE),
    r_igraph = find_roi_boundaries(vertices, faces, roi,
                                    boundary_method = "edge_vertices",
                                    use_cpp = FALSE,
                                    verbose = FALSE),
    iterations = 10,
    check = FALSE  # Don't check equality (order may differ)
  )

  print(bm)

  # Extract timing
  cpp_time <- as.numeric(bm$median[1])
  r_time <- as.numeric(bm$median[2])
  speedup <- r_time / cpp_time

  cat("\nSpeedup:", round(speedup, 2), "x\n")

  # Optimized version should be faster
  expect_true(speedup > 1.0)
})
