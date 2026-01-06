# Performance and scale tests for large meshes
# Tests behavior with realistic mesh sizes (1k-10k+ vertices)

library(testthat)
library(neurosurf)

# ==============================================================================
# Helper Functions for Creating Test Meshes
# ==============================================================================

#' Create a grid mesh with specified dimensions
#' @param nx Number of vertices in x direction
#' @param ny Number of vertices in y direction
#' @return SurfaceGeometry object
create_grid_mesh <- function(nx, ny) {
  # Create vertex grid
  x <- seq(0, 1, length.out = nx)
  y <- seq(0, 1, length.out = ny)
  grid <- expand.grid(x = x, y = y)
  verts <- as.matrix(cbind(grid$x, grid$y, rep(0, nrow(grid))))

  # Create triangular faces
  faces <- list()
  for (i in 1:(nx - 1)) {
    for (j in 1:(ny - 1)) {
      # Bottom-left vertex of current cell (0-indexed)
      bl <- (j - 1) * nx + (i - 1)
      br <- bl + 1
      tl <- bl + nx
      tr <- tl + 1

      # Two triangles per cell
      faces[[length(faces) + 1]] <- c(bl, br, tl)
      faces[[length(faces) + 1]] <- c(br, tr, tl)
    }
  }

  face_mat <- do.call(rbind, faces)
  storage.mode(face_mat) <- "integer"

  SurfaceGeometry(verts, face_mat, hemi = "lh")
}

# ==============================================================================
# Basic Large Mesh Tests
# ==============================================================================

test_that("SurfaceGeometry handles 1000+ vertex mesh", {
  # 32x32 grid = 1024 vertices
  geom <- create_grid_mesh(32, 32)

  expect_s4_class(geom, "SurfaceGeometry")
  expect_equal(nrow(coords(geom)), 1024)
  expect_true(igraph::vcount(geom@graph) == 1024)
})

test_that("SurfaceGeometry handles 5000+ vertex mesh", {
  # 72x72 grid = 5184 vertices
  geom <- create_grid_mesh(72, 72)

  expect_s4_class(geom, "SurfaceGeometry")
  expect_equal(nrow(coords(geom)), 5184)
})

test_that("fsaverage std.8 surface has expected size", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  geom <- read_surf(surf_file)
  n_verts <- nrow(coords(geom))

  # std.8 should have several thousand vertices
  expect_true(n_verts > 1000)
  expect_true(n_verts < 50000)
})

# ==============================================================================
# Performance Tests for Core Operations
# ==============================================================================

test_that("curvature computation completes in reasonable time for large mesh", {
  skip_if_not_installed("Rvcg")

  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  geom <- read_surf(surf_file)

  # Time the curvature computation
  timing <- system.time({
    curv_vals <- curvature(geom)
  })

  # Should complete in under 10 seconds for std.8
  expect_true(timing["elapsed"] < 10)
  expect_equal(length(curv_vals), nrow(coords(geom)))
})

test_that("neighbor finding completes in reasonable time for large mesh", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  geom <- read_surf(surf_file)

  timing <- system.time({
    neighbors <- find_all_neighbors(geom, radius = 10)
  })

  # Should complete in under 30 seconds
  expect_true(timing["elapsed"] < 30)
  expect_true(is.list(neighbors))
})

test_that("smoothing completes in reasonable time for large mesh", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  geom <- read_surf(surf_file)
  n_verts <- nrow(coords(geom))
  set.seed(42)
  data_vals <- rnorm(n_verts)
  neuro_surf <- NeuroSurface(geom, 1:n_verts, data_vals)

  timing <- system.time({
    smoothed <- smooth(neuro_surf, sigma = 4)
  })

  # Should complete in under 60 seconds
  expect_true(timing["elapsed"] < 60)
  expect_s4_class(smoothed, "NeuroSurface")
})

test_that("searchlight creation completes in reasonable time", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  geom <- read_surf(surf_file)

  timing <- system.time({
    sl <- SurfaceSearchlight(geom, radius = 12, as_deflist = TRUE)
  })

  # Should complete in under 30 seconds
  expect_true(timing["elapsed"] < 30)
  expect_s4_class(sl, "deflist")
})

# ==============================================================================
# Memory Efficiency Tests
# ==============================================================================
test_that("large NeuroSurface doesn't duplicate geometry", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  geom <- read_surf(surf_file)
  n_verts <- nrow(coords(geom))
  data_vals <- rnorm(n_verts)

  # Create multiple NeuroSurfaces sharing the same geometry
  ns1 <- NeuroSurface(geom, 1:n_verts, data_vals)
  ns2 <- NeuroSurface(geom, 1:n_verts, data_vals * 2)
  ns3 <- NeuroSurface(geom, 1:n_verts, data_vals * 3)

  # All should reference the same geometry object
  expect_identical(ns1@geometry, ns2@geometry)
  expect_identical(ns2@geometry, ns3@geometry)
})

test_that("sparse geodesic matrix is memory efficient", {
  geom <- create_grid_mesh(32, 32)  # 1024 vertices

  # Sparse mode should use less memory than dense
  gdm_sparse <- geodesic_distance_matrix(geom, mode = "sparse", cache = FALSE)

  expect_s4_class(gdm_sparse, "dgCMatrix")

  # Sparse matrix should have many zeros
  n_nonzero <- Matrix::nnzero(gdm_sparse)
  n_total <- nrow(gdm_sparse) * ncol(gdm_sparse)

  # For a sparse representation, non-zero should be << total
  expect_true(n_nonzero < n_total / 2)
})

# ==============================================================================
# Stress Tests
# ==============================================================================

test_that("multiple operations on large mesh complete successfully", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  geom <- read_surf(surf_file)
  n_verts <- nrow(coords(geom))

  # Chain of operations
  set.seed(42)
  data_vals <- rnorm(n_verts)

  # Create NeuroSurface
  ns <- NeuroSurface(geom, 1:n_verts, data_vals)

  # Smooth
  smoothed <- smooth(ns, sigma = 2)

  # Extract subset
  subset_data <- series(smoothed, 1:100)

  # Create ROI
  disk <- SurfaceDisk(geom, source = 500, radius = 8)

  # All should complete successfully

  expect_s4_class(smoothed, "NeuroSurface")
  expect_equal(length(subset_data), 100)
  expect_s4_class(disk, "ROISurface")
})

test_that("NeuroSurfaceVector with many columns handles large data", {
  geom <- create_grid_mesh(20, 20)  # 400 vertices
  n_verts <- 400
  n_cols <- 100

  # Create matrix with many columns
  mat <- matrix(rnorm(n_verts * n_cols), nrow = n_verts, ncol = n_cols)

  nsv <- NeuroSurfaceVector(
    geometry = geom,
    indices = 1:n_verts,
    mat = mat
  )

  expect_s4_class(nsv, "NeuroSurfaceVector")
  expect_equal(ncol(nsv@data), n_cols)
})

# ==============================================================================
# Boundary Detection Performance
# ==============================================================================

test_that("boundary detection handles large labeled surfaces", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  geom <- read_surf(surf_file)
  n_verts <- nrow(coords(geom))

  # Create multiple regions
  n_regions <- 10
  labels <- sample(1:n_regions, n_verts, replace = TRUE)

  lsurf <- new(
    "LabeledNeuroSurface",
    geometry = geom,
    indices = 1:n_verts,
    data = as.numeric(labels),
    labels = paste0("Region", 1:n_regions),
    cols = grDevices::rainbow(n_regions)
  )

  timing <- system.time({
    boundaries <- find_roi_boundaries(lsurf)
  })

  # Should complete in reasonable time
  expect_true(timing["elapsed"] < 60)
  expect_true(is.list(boundaries))
})

# ==============================================================================
# Gaussian Splat Performance
# ==============================================================================

test_that("multiple gaussian splats complete efficiently", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  geom <- read_surf(surf_file)

  # Create many splats
  n_splats <- 20
  cds <- coords(geom)
  centers <- cds[sample(1:nrow(cds), n_splats), ]
  sigmas <- rep(5, n_splats)

  timing <- system.time({
    result <- gaussian_splat_multi(
      geom,
      centers = centers,
      sigmas = sigmas
    )
  })

  # Should complete in under 30 seconds
  expect_true(timing["elapsed"] < 30)
  expect_equal(length(result), nrow(coords(geom)))
})

# ==============================================================================
# Widget Performance (Smoke Test)
# ==============================================================================

test_that("surfwidget handles large mesh without timeout", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  geom <- read_surf(surf_file)
  n_verts <- nrow(coords(geom))
  data_vals <- rnorm(n_verts)

  timing <- system.time({
    widget <- surfwidget(geom, data = data_vals)
  })

  # Widget creation should be fast (data prep, not rendering)
  expect_true(timing["elapsed"] < 10)
  expect_s3_class(widget, "htmlwidget")
})

# ==============================================================================
# Scaling Behavior Tests
# ==============================================================================

test_that("operations scale reasonably with mesh size", {
  # Test with different mesh sizes
  sizes <- c(10, 20, 30)  # 100, 400, 900 vertices
  times <- numeric(length(sizes))

  for (i in seq_along(sizes)) {
    geom <- create_grid_mesh(sizes[i], sizes[i])

    times[i] <- system.time({
      # Simple operation: neighbor graph
      g <- neighbor_graph(geom, radius = 0.1)
    })["elapsed"]
  }

  # Times should increase but not exponentially
  # (ratio of times should be roughly proportional to ratio of vertices)
  ratios <- times[-1] / times[-length(times)]

  # Allow for some variation but shouldn't explode
  expect_true(all(ratios < 20))
})

# ==============================================================================
# Concurrent Access Tests
# ==============================================================================

test_that("multiple NeuroSurfaces can coexist", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  geom <- read_surf(surf_file)
  n_verts <- nrow(coords(geom))

  # Create multiple surfaces with different data
  surfaces <- lapply(1:5, function(i) {
    data_vals <- rnorm(n_verts, mean = i)
    NeuroSurface(geom, 1:n_verts, data_vals)
  })

  # All should be valid
  for (i in seq_along(surfaces)) {
    expect_s4_class(surfaces[[i]], "NeuroSurface")

    # Data should have appropriate mean
    mean_data <- mean(series(surfaces[[i]], 1:100))
    expect_true(abs(mean_data - i) < 1)  # Within 1 SD
  }
})
