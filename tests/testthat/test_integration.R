# Integration tests for end-to-end workflows
# Tests complete pipelines from data loading through analysis

library(testthat)
library(neurosurf)

# ==============================================================================
# Complete Surface Loading and Analysis Workflow
# ==============================================================================

test_that("complete workflow: load surface -> compute curvature -> create NeuroSurface", {
  skip_if_not_installed("Rvcg")

  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  # Step 1: Load surface geometry
  geom <- read_surf(surf_file)
  expect_s4_class(geom, "SurfaceGeometry")

  # Step 2: Compute curvature
  curv_vals <- curvature(geom)
  expect_equal(length(curv_vals), nrow(coords(geom)))

  # Step 3: Create NeuroSurface
  n_verts <- nrow(coords(geom))
  neuro_surf <- NeuroSurface(
    geometry = geom,
    indices = 1:n_verts,
    data = curv_vals
  )
  expect_s4_class(neuro_surf, "NeuroSurface")

  # Step 4: Verify data access
  retrieved_data <- series(neuro_surf, 1:10)
  expect_equal(length(retrieved_data), 10)
})

test_that("complete workflow: load surface -> searchlight analysis", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  # Load surface
  geom <- read_surf(surf_file)

  # Create searchlight
  sl <- SurfaceSearchlight(geom, radius = 12, as_deflist = TRUE)
  expect_s4_class(sl, "deflist")

  # Access first few regions
  region1 <- sl[[1]]
  expect_true(is.numeric(region1))
  expect_true(length(region1) > 0)
  expect_false(is.null(attr(region1, "center")))

  region2 <- sl[[2]]
  expect_true(is.numeric(region2))
})

test_that("complete workflow: surface with data -> smoothing -> analysis", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  # Load surface
  geom <- read_surf(surf_file)
  n_verts <- nrow(coords(geom))

  # Create random data
  set.seed(42)
  data_vals <- rnorm(n_verts)

  # Create NeuroSurface
  neuro_surf <- NeuroSurface(
    geometry = geom,
    indices = 1:n_verts,
    data = data_vals
  )

  # Apply smoothing
  smoothed <- smooth(neuro_surf, sigma = 4)
  expect_s4_class(smoothed, "NeuroSurface")

  # Verify smoothing reduced variance
  original_sd <- sd(data_vals)
  smoothed_data <- series(smoothed, indices(smoothed))
  smoothed_sd <- sd(smoothed_data)

  expect_true(smoothed_sd < original_sd)
})

# ==============================================================================
# Bilateral Surface Workflow
# ==============================================================================

test_that("bilateral surface loading workflow", {
  lh_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  rh_file <- system.file("extdata", "std.8_rh.smoothwm.asc", package = "neurosurf")
  skip_if(lh_file == "" || rh_file == "", "Bilateral surface files not available")

  # Load both hemispheres
  lh_geom <- read_surf(lh_file)
  rh_geom <- read_surf(rh_file)

  expect_equal(lh_geom@hemi, "lh")
  expect_equal(rh_geom@hemi, "rh")

  # Both should have similar structure
  expect_equal(nrow(coords(lh_geom)), nrow(coords(rh_geom)))
})

test_that("load_fsaverage_std8 returns both hemispheres", {
  fs <- load_fsaverage_std8("smoothwm")

  expect_true(is.list(fs))
  expect_true("lh" %in% names(fs))
  expect_true("rh" %in% names(fs))
  expect_s4_class(fs$lh, "SurfaceGeometry")
  expect_s4_class(fs$rh, "SurfaceGeometry")
})

test_that("load_fsaverage_bundle returns SurfaceSets", {
  bundle <- load_fsaverage_bundle(
    surfs = c("smoothwm", "inflated"),
    default_label = "smoothwm"
  )

  expect_true(is.list(bundle))
  expect_true("lh" %in% names(bundle))
  expect_true("rh" %in% names(bundle))
  expect_s4_class(bundle$lh, "SurfaceSet")
  expect_s4_class(bundle$rh, "SurfaceSet")

  # Can access specific surfaces
  lh_smoothwm <- get_surface(bundle$lh, "smoothwm")
  lh_inflated <- get_surface(bundle$lh, "inflated")

  expect_s4_class(lh_smoothwm, "SurfaceGeometry")
  expect_s4_class(lh_inflated, "SurfaceGeometry")
})

# ==============================================================================
# ROI Analysis Workflow
# ==============================================================================

test_that("ROI workflow: create ROI -> extract data -> analyze", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  geom <- read_surf(surf_file)
  n_verts <- nrow(coords(geom))

  # Create data
  set.seed(123)
  data_vals <- rnorm(n_verts)
  neuro_surf <- NeuroSurface(geom, 1:n_verts, data_vals)

  # Create an ROI (disk around vertex 100)
  disk <- SurfaceDisk(geometry = geom, source = 100, radius = 10)

  expect_s4_class(disk, "ROISurface")
  expect_true(length(disk@index) > 0)

  # Extract data from ROI
  roi_data <- series(neuro_surf, disk@index)
  expect_equal(length(roi_data), length(disk@index))
})

test_that("ROI boundary detection workflow", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  geom <- read_surf(surf_file)
  n_verts <- nrow(coords(geom))

  # Create labeled surface with two regions
  labels <- rep(1L, n_verts)
  labels[1:(n_verts %/% 2)] <- 2L

  lsurf <- new(
    "LabeledNeuroSurface",
    geometry = geom,
    indices = 1:n_verts,
    data = as.numeric(labels),
    labels = c("Region1", "Region2"),
    cols = c("#FF0000", "#00FF00")
  )

  # Find boundaries
  boundaries <- find_roi_boundaries(lsurf)

  expect_true(is.list(boundaries))
})

# ==============================================================================
# Geodesic Distance Workflow
# ==============================================================================

test_that("geodesic distance workflow: compute distances -> find neighbors", {
  geom <- example_surface_geometry()

  # Compute geodesic distance matrix
  gdm <- geodesic_distance_matrix(geom, mode = "dense", cache = FALSE)

  expect_equal(dim(gdm), c(4, 4))
  expect_true(all(diag(gdm) == 0))
  expect_true(all(gdm >= 0))

  # Use for neighbor finding
  neighbors <- find_all_neighbors(geom, radius = 1.5)

  expect_true(is.list(neighbors))
  expect_equal(length(neighbors), 4)
})

test_that("parcel centroid workflow", {
  geom <- example_surface_geometry()

  # Create labeled surface
  lsurf <- new(
    "LabeledNeuroSurface",
    geometry = geom,
    indices = 1:4,
    data = c(1, 1, 2, 2),
    labels = c("A", "B"),
    cols = c("#ff0000", "#00ff00")
  )

  # Get centroids
  centroids <- parcel_geodesic_centroid(lsurf, cache = FALSE)

  expect_equal(nrow(centroids), 2)
  expect_true("parcel" %in% names(centroids))
  expect_true("centroid_vertex" %in% names(centroids))
})

# ==============================================================================
# Visualization Workflow
# ==============================================================================

test_that("visualization workflow: load -> color map -> widget", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  # Load surface
  geom <- read_surf(surf_file)
  n_verts <- nrow(coords(geom))

  # Create data
  set.seed(42)
  data_vals <- rnorm(n_verts)

  # Create NeuroSurface
  neuro_surf <- NeuroSurface(geom, 1:n_verts, data_vals)

  # Create ColorMappedNeuroSurface
  color_mapped <- ColorMappedNeuroSurface(
    geometry = geom,
    indices = 1:n_verts,
    data = data_vals,
    cmap = grDevices::colorRampPalette(c("blue", "white", "red"))(100),
    irange = c(-2, 2),
    thresh = c(-1, 1)
  )

  expect_s4_class(color_mapped, "ColorMappedNeuroSurface")

  # Create widget
  widget <- surfwidget(color_mapped)
  expect_s3_class(widget, "htmlwidget")
})

# ==============================================================================
# Gaussian Splat Workflow
# ==============================================================================

test_that("gaussian splat workflow: create splats -> combine -> analyze", {
  geom <- example_surface_geometry()

  # Create multiple splats
  splat1 <- gaussian_splat(geom, center = c(0, 0, 0), sigma = 0.5)
  splat2 <- gaussian_splat(geom, center = c(1, 0, 0), sigma = 0.5)

  expect_equal(length(splat1), 4)
  expect_equal(length(splat2), 4)

  # Combine splats
  combined <- splat1 + splat2

  expect_equal(length(combined), 4)
  expect_true(all(combined >= 0))
})

test_that("multi-splat workflow", {
  geom <- example_surface_geometry()

  centers <- rbind(
    c(0, 0, 0),
    c(1, 0, 0),
    c(0, 1, 0)
  )
  sigmas <- c(0.5, 0.5, 0.5)
  weights <- c(1, 0.5, 0.25)

  multi_splat <- gaussian_splat_multi(
    geom,
    centers = centers,
    sigmas = sigmas,
    weights = weights
  )

  expect_equal(length(multi_splat), 4)
  expect_true(all(is.finite(multi_splat)))
})

# ==============================================================================
# Data Write/Read Cycle Workflow
# ==============================================================================

test_that("write/read cycle preserves data integrity", {
  geom <- example_surface_geometry()
  idx <- 1:4
  original_vals <- c(1.5, 2.5, 3.5, 4.5)
  surf <- NeuroSurface(geometry = geom, indices = idx, data = original_vals)

  # Write to temp file
  outstem <- tempfile("roundtrip")
  fname <- paste0(outstem, ".1D.dset")
  on.exit(unlink(fname), add = TRUE)

  write_surf_data(surf, outstem = outstem, hemi = "")

  # Read back
  tab <- read.table(fname, header = FALSE)

  # Verify
  expect_equal(nrow(tab), length(idx))
  expect_equal(tab[, 1], idx - 1)  # 0-indexed
  expect_equal(tab[, 2], original_vals, tolerance = 1e-6)
})

# ==============================================================================
# Surface Set Workflow
# ==============================================================================

test_that("surface set workflow: create set -> access surfaces -> analyze", {
  smoothwm <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  inflated <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  skip_if(smoothwm == "" || inflated == "", "Surface files not available")

  # Load surfaces
  geom_smoothwm <- read_surf(smoothwm)
  geom_inflated <- read_surf(inflated)

  # Create surface set
  ss <- surface_set(
    list(smoothwm = geom_smoothwm, inflated = geom_inflated),
    default_label = "smoothwm"
  )

  expect_s4_class(ss, "SurfaceSet")

  # Access default surface
  default_surf <- get_surface(ss)
  expect_s4_class(default_surf, "SurfaceGeometry")

  # Access specific surface
  inflated_surf <- get_surface(ss, "inflated")
  expect_s4_class(inflated_surf, "SurfaceGeometry")

  # Vertices should match across surface types
  expect_equal(nrow(coords(default_surf)), nrow(coords(inflated_surf)))
})

# ==============================================================================
# Sparse Data Workflow
# ==============================================================================

test_that("sparse data workflow: partial coverage -> fill -> smooth", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  geom <- read_surf(surf_file)
  n_verts <- nrow(coords(geom))

  # Create sparse data (only 10% of vertices)
  sparse_idx <- sample(1:n_verts, size = n_verts %/% 10)
  sparse_vals <- rnorm(length(sparse_idx))

  # Create sparse NeuroSurface
  sparse_surf <- NeuroSurface(
    geometry = geom,
    indices = sparse_idx,
    data = sparse_vals
  )

  expect_s4_class(sparse_surf, "NeuroSurface")
  expect_equal(length(sparse_surf@indices), length(sparse_idx))
})

# ==============================================================================
# NeuroSurfaceVector Workflow
# ==============================================================================

test_that("NeuroSurfaceVector workflow: create -> access columns", {
  geom <- example_surface_geometry()
  idx <- 1:4
  mat <- matrix(1:12, nrow = 4, ncol = 3)

  nsv <- NeuroSurfaceVector(geometry = geom, indices = idx, mat = mat)

  expect_s4_class(nsv, "NeuroSurfaceVector")

  # Access data
  col1 <- series(nsv, 1:4, 1)
  expect_equal(length(col1), 4)
})

# ==============================================================================
# Complex Multi-Step Workflow
# ==============================================================================

test_that("complex workflow: bilateral analysis with smoothing and ROI", {
  lh_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  rh_file <- system.file("extdata", "std.8_rh.smoothwm.asc", package = "neurosurf")
  skip_if(lh_file == "" || rh_file == "", "Bilateral files not available")

  # Load both hemispheres
  lh_geom <- read_surf(lh_file)
  rh_geom <- read_surf(rh_file)

  n_lh <- nrow(coords(lh_geom))
  n_rh <- nrow(coords(rh_geom))

  # Create random data for both
  set.seed(42)
  lh_data <- rnorm(n_lh)
  rh_data <- rnorm(n_rh)

  # Create NeuroSurfaces
  lh_surf <- NeuroSurface(lh_geom, 1:n_lh, lh_data)
  rh_surf <- NeuroSurface(rh_geom, 1:n_rh, rh_data)

  # Smooth both
  lh_smoothed <- smooth(lh_surf, sigma = 4)
  rh_smoothed <- smooth(rh_surf, sigma = 4)

  # Create ROIs on both
  lh_disk <- SurfaceDisk(lh_geom, source = 100, radius = 10)
  rh_disk <- SurfaceDisk(rh_geom, source = 100, radius = 10)

  # Extract ROI data
  lh_roi_data <- series(lh_smoothed, lh_disk@index)
  rh_roi_data <- series(rh_smoothed, rh_disk@index)

  expect_true(length(lh_roi_data) > 0)
  expect_true(length(rh_roi_data) > 0)

  # Compare means
  lh_mean <- mean(lh_roi_data)
  rh_mean <- mean(rh_roi_data)

  expect_true(is.finite(lh_mean))
  expect_true(is.finite(rh_mean))
})
