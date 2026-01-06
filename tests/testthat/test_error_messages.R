# Tests for error handling and error message clarity
# Ensures user-facing errors are informative and helpful

library(testthat)
library(neurosurf)

# ==============================================================================
# SurfaceGeometry Error Tests
# ==============================================================================

test_that("SurfaceGeometry errors on invalid vertices", {
  faces <- matrix(c(0L, 1L, 2L), ncol = 3, byrow = TRUE)

  # NULL vertices
  expect_error(SurfaceGeometry(NULL, faces))

  # Wrong dimensions
  expect_error(SurfaceGeometry(matrix(1:4, ncol = 2), faces))

  # Non-numeric
  expect_error(SurfaceGeometry(matrix("a", 3, 3), faces))
})

test_that("SurfaceGeometry errors on invalid faces", {
  verts <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)

  # NULL faces
  expect_error(SurfaceGeometry(verts, NULL))

  # Wrong number of columns (should be 3)
  expect_error(SurfaceGeometry(verts, matrix(1:4, ncol = 2)))
})

test_that("SurfaceGeometry errors on out-of-bounds face indices", {
  verts <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
  # Faces reference vertex index 10, but only 3 vertices exist
  faces <- matrix(c(0L, 1L, 10L), ncol = 3, byrow = TRUE)

  expect_error(SurfaceGeometry(verts, faces))
})

# ==============================================================================
# NeuroSurface Error Tests
# ==============================================================================

test_that("NeuroSurface errors on mismatched data and indices length", {
  geom <- example_surface_geometry()
  idx <- 1:4
  vals <- c(1, 2, 3)  # Only 3 values but 4 indices

  expect_error(
    NeuroSurface(geometry = geom, indices = idx, data = vals),
    regexp = "length"
  )
})

test_that("NeuroSurface errors on out-of-bounds indices", {
  geom <- example_surface_geometry()
  idx <- c(1, 2, 100)  # Index 100 doesn't exist
  vals <- c(1, 2, 3)

  expect_error(
    NeuroSurface(geometry = geom, indices = idx, data = vals)
  )
})

test_that("NeuroSurface errors on negative indices", {
  geom <- example_surface_geometry()
  idx <- c(-1, 1, 2)
  vals <- c(1, 2, 3)

  expect_error(
    NeuroSurface(geometry = geom, indices = idx, data = vals)
  )
})

# ==============================================================================
# ColorMappedNeuroSurface Error Tests
# ==============================================================================

test_that("ColorMappedNeuroSurface errors on invalid irange", {
  geom <- example_surface_geometry()
  idx <- 1:4
  vals <- c(1, 2, 3, 4)
  cmap <- grDevices::rainbow(10)

  # irange with wrong length
  expect_error(
    ColorMappedNeuroSurface(geom, idx, vals, cmap = cmap, irange = c(1)),
    regexp = "irange"
  )
})

test_that("ColorMappedNeuroSurface errors on empty color map", {
  geom <- example_surface_geometry()
  idx <- 1:4
  vals <- c(1, 2, 3, 4)

  expect_error(
    ColorMappedNeuroSurface(geom, idx, vals, cmap = character(0), irange = c(0, 5))
  )
})

# ==============================================================================
# ROISurface Error Tests
# ==============================================================================

test_that("ROISurface errors on invalid source indices", {
  geom <- example_surface_geometry()

  # Source index out of bounds
  expect_error(
    ROISurface(geometry = geom, source = 100, index = 1:2, vals = c(1, 2))
  )
})

test_that("SurfaceDisk errors on invalid radius", {
  geom <- example_surface_geometry()

  expect_error(
    SurfaceDisk(geometry = geom, source = 1, radius = -5)
  )
})

# ==============================================================================
# Geodesic Distance Error Tests
# ==============================================================================

test_that("geodesic_distance_matrix errors on invalid source", {
  geom <- example_surface_geometry()

  # Negative source index
  expect_error(
    geodesic_distance_matrix(geom, source = -1)
  )

  # Source index out of bounds
  expect_error(
    geodesic_distance_matrix(geom, source = 1000)
  )
})

# ==============================================================================
# Neighborhood Error Tests
# ==============================================================================

test_that("find_all_neighbors errors on invalid radius", {
  geom <- example_surface_geometry()

  # Negative radius
  expect_error(
    find_all_neighbors(geom, radius = -1),
    regexp = "radius"
  )

  # Zero radius (may or may not be an error depending on implementation)
  result <- tryCatch(
    find_all_neighbors(geom, radius = 0),
    error = function(e) "error"
  )
  # Just check it handles the case
  expect_true(is.list(result) || identical(result, "error"))
})

test_that("find_all_neighbors errors on invalid edge weights", {
  geom <- example_surface_geometry()

  # Negative edge weights
  n_edges <- igraph::ecount(geom@graph)
  bad_weights <- rep(-1, n_edges)

  expect_error(
    find_all_neighbors(geom, radius = 1, edgeWeights = bad_weights),
    regexp = "edge"
  )
})

# ==============================================================================
# Searchlight Error Tests
# ==============================================================================

test_that("SurfaceSearchlight errors on invalid radius", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)

  # Negative radius
  expect_error(
    SurfaceSearchlight(surf, radius = -5),
    regexp = "radius"
  )
})

test_that("RandomSurfaceSearchlight errors on empty nodeset", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)

  expect_error(
    RandomSurfaceSearchlight(surf, radius = 12, nodeset = integer()),
    regexp = "nodeset"
  )
})

# ==============================================================================
# Gaussian Splat Error Tests
# ==============================================================================

test_that("gaussian_splat errors on invalid center", {
  geom <- example_surface_geometry()

  # Wrong length center
  expect_error(
    gaussian_splat(geom, center = c(0, 0), sigma = 1),
    regexp = "center"
  )
})

test_that("gaussian_splat errors on invalid sigma", {
  geom <- example_surface_geometry()

  # Negative sigma
  expect_error(
    gaussian_splat(geom, center = c(0, 0, 0), sigma = -1),
    regexp = "sigma"
  )

  # Zero sigma
  expect_error(
    gaussian_splat(geom, center = c(0, 0, 0), sigma = 0),
    regexp = "sigma"
  )
})

test_that("find_nearest_vertex errors on wrong point dimension", {
  geom <- example_surface_geometry()

  expect_error(
    find_nearest_vertex(geom, point = c(0, 0)),
    regexp = "point"
  )
})

# ==============================================================================
# File I/O Error Tests
# ==============================================================================

test_that("read_surf errors on non-existent file", {
  expect_error(
    read_surf("/path/that/does/not/exist/surface.asc"),
    regexp = "cannot open|does not exist|No such file"
  )
})

test_that("read_freesurfer_annot errors on missing geometry", {
  # Even if the file existed, NULL geometry should error
  expect_error(
    read_freesurfer_annot("fake_file.annot", geometry = NULL)
  )
})

test_that("write_surf_data errors on invalid input object", {
  expect_error(
    write_surf_data(list(a = 1), outstem = "test"),
    regexp = "NeuroSurface|NeuroSurfaceVector"
  )
})

# ==============================================================================
# fetch_surfaces Error Tests
# ==============================================================================

test_that("load_fsaverage_std8 errors on invalid surface type", {
  expect_error(
    load_fsaverage_std8("nonexistent_surface_type")
  )
})

test_that("load_fsaverage errors on unsupported density", {
  expect_error(
    load_fsaverage(density = "1k", surf = "smoothwm")
  )
})

# ==============================================================================
# view_surface Error Tests
# ==============================================================================

test_that("view_surface errors on sparse vals without vals_vertices", {
  skip_if_not_installed("rgl")

  old_opt <- getOption("rgl.useNULL")
  options(rgl.useNULL = TRUE)
  on.exit(options(rgl.useNULL = old_opt), add = TRUE)

  geom <- example_surface_geometry()
  # 4 vertices but only 2 values without specifying which vertices
  vals <- c(1, 2)

  expect_error(
    view_surface(geom, vals = vals),
    regexp = "vals_vertices"
  )
})

# ==============================================================================
# SurfaceSet Error Tests
# ==============================================================================

test_that("surface_set errors on empty geometry list", {
  expect_error(
    surface_set(list())
  )
})

test_that("surface_set errors on non-SurfaceGeometry elements", {
  expect_error(
    surface_set(list(smoothwm = "not a geometry"))
  )
})

test_that("get_surface errors on invalid label", {
  geom <- example_surface_geometry()
  ss <- surface_set(list(smoothwm = geom))

  expect_error(
    get_surface(ss, "nonexistent_label")
  )
})

# ==============================================================================
# vol_to_surf Error Tests
# ==============================================================================

test_that("vol_to_surf errors on invalid method", {
  skip("Requires neuroim2 test data")
})

# ==============================================================================
# Error Message Quality Tests
# ==============================================================================

test_that("error messages mention the problematic parameter", {
  geom <- example_surface_geometry()

  # Check that error mentions "radius" for radius issues
  tryCatch(
    find_all_neighbors(geom, radius = -1),
    error = function(e) {
      expect_true(
        grepl("radius", e$message, ignore.case = TRUE),
        info = paste("Error should mention 'radius':", e$message)
      )
    }
  )
})

test_that("error messages provide useful context", {
  geom <- example_surface_geometry()

  # Test that error messages include relevant numbers/context
  tryCatch(
    gaussian_splat(geom, center = c(0, 0), sigma = 1),
    error = function(e) {
      # Should mention something about the center or length
      expect_true(
        grepl("center|length|3", e$message, ignore.case = TRUE),
        info = paste("Error should provide context:", e$message)
      )
    }
  )
})

# ==============================================================================
# Warning Tests
# ==============================================================================

test_that("surfwidget warns on invalid config", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()

  expect_warning(
    surfwidget(geom, config = list(unknownParam = 123)),
    regexp = "unknown"
  )
})

test_that("process_config warns on invalid shininess", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()

  expect_warning(
    surfwidget(geom, config = list(shininess = 500)),
    regexp = "shininess"
  )
})

test_that("process_config warns on invalid directionalLightIntensity", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()

  expect_warning(
    surfwidget(geom, config = list(directionalLightIntensity = 5)),
    regexp = "directionalLightIntensity"
  )
})
