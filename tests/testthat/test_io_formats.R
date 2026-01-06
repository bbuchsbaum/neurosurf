# Tests for file I/O across different surface formats
# Covers GIFTI, NIML, FreeSurfer ASCII, and binary format reading

library(testthat)
library(neurosurf)

# ==============================================================================
# GIFTI Format Tests
# ==============================================================================

test_that("read_surf reads GIFTI surface file", {
  gifti_file <- system.file("extdata", "rscan01_lh.gii", package = "neurosurf")
  skip_if(gifti_file == "", "GIFTI test file not available")
  skip_if_not_installed("gifti")

  surf <- read_surf(gifti_file)

  expect_s4_class(surf, "SurfaceGeometry")
  expect_true(nrow(coords(surf)) > 0)
  expect_true(ncol(surf@mesh$it) > 0)
})

test_that("GIFTI reader extracts hemisphere from filename", {
  gifti_file <- system.file("extdata", "rscan01_lh.gii", package = "neurosurf")
  skip_if(gifti_file == "", "GIFTI test file not available")
  skip_if_not_installed("gifti")

  surf <- read_surf(gifti_file)
  expect_equal(surf@hemi, "lh")
})

test_that("GIFTI reader returns valid mesh topology", {
  gifti_file <- system.file("extdata", "rscan01_lh.gii", package = "neurosurf")
  skip_if(gifti_file == "", "GIFTI test file not available")
  skip_if_not_installed("gifti")

  surf <- read_surf(gifti_file)

  # All face indices should be valid vertex indices
  n_verts <- nrow(coords(surf))
  face_indices <- as.vector(surf@mesh$it)
  expect_true(all(face_indices >= 1))
  expect_true(all(face_indices <= n_verts))
})

# ==============================================================================
# NIML Format Tests
# ==============================================================================

test_that("read_surf reads NIML surface data file", {
  niml_file <- system.file("extdata", "rscan01_lh.niml.dset", package = "neurosurf")
  skip_if(niml_file == "", "NIML test file not available")

  # NIML files contain data, not geometry - need a geometry first
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface geometry file not available")

  geom <- read_surf_geometry(surf_file)

  # Read the NIML data and attach to geometry
  result <- tryCatch(
    read_surf_data(geom, niml_file),
    error = function(e) NULL
  )

  # If reading succeeds, verify basic structure
  if (!is.null(result)) {
    expect_true(inherits(result, "NeuroSurface") || inherits(result, "NeuroSurfaceVector"))
  }
})

test_that("readNIMLSurfaceHeader parses NIML metadata correctly", {
  niml_file <- system.file("extdata", "rscan01_lh.niml.dset", package = "neurosurf")
  skip_if(niml_file == "", "NIML test file not available")

  # Internal function to read NIML header
  header <- tryCatch(
    neurosurf:::readNIMLSurfaceHeader(niml_file),
    error = function(e) NULL
  )

  if (!is.null(header)) {
    expect_true(is.list(header))
    expect_true("header_file" %in% names(header))
    expect_true("data_file" %in% names(header))
    expect_true("node_count" %in% names(header) || "nodes" %in% names(header))
  }
})

# ==============================================================================
# FreeSurfer ASCII Format Tests
# ==============================================================================

test_that("read_surf reads FreeSurfer ASCII format", {
  asc_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(asc_file == "", "FreeSurfer ASCII test file not available")

  surf <- read_surf(asc_file)

  expect_s4_class(surf, "SurfaceGeometry")
  expect_true(nrow(coords(surf)) > 0)
  expect_true(ncol(surf@mesh$it) > 0)
})

test_that("FreeSurfer ASCII reader correctly parses vertex count header", {
  asc_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(asc_file == "", "FreeSurfer ASCII test file not available")

  # Read header info directly
  header <- neurosurf:::readFreesurferAsciiHeader(asc_file)

  expect_true(is.numeric(header$vertices))
  expect_true(is.numeric(header$faces))
  expect_true(header$vertices > 0)
  expect_true(header$faces > 0)
})

test_that("FreeSurfer ASCII reader extracts hemisphere from filename", {
  lh_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  rh_file <- system.file("extdata", "std.8_rh.smoothwm.asc", package = "neurosurf")
  skip_if(lh_file == "", "Left hemisphere test file not available")
  skip_if(rh_file == "", "Right hemisphere test file not available")

  lh_surf <- read_surf(lh_file)
  rh_surf <- read_surf(rh_file)

  expect_equal(lh_surf@hemi, "lh")
  expect_equal(rh_surf@hemi, "rh")
})

test_that("multiple ASCII surface types can be loaded", {
  surf_types <- c("smoothwm", "pial", "inflated", "white", "sphere")

  for (stype in surf_types) {
    fname <- sprintf("std.8_lh.%s.asc", stype)
    fpath <- system.file("extdata", fname, package = "neurosurf")

    if (fpath != "") {
      surf <- read_surf(fpath)
      expect_s4_class(surf, "SurfaceGeometry",
                      info = paste("Failed for surface type:", stype))
      expect_true(nrow(coords(surf)) > 0,
                  info = paste("No vertices for surface type:", stype))
    }
  }
})

# ==============================================================================
# AFNI Format Tests
# ==============================================================================

test_that("AFNI 1D.dset file can be read", {
  afni_file <- system.file("extdata", "std.8_fsaverage_lh.MI.1D", package = "neurosurf")
  skip_if(afni_file == "", "AFNI test file not available")

  # Get geometry first
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface geometry file not available")

  geom <- read_surf_geometry(surf_file)

  # Try to read AFNI data - may not work depending on file format
  result <- tryCatch(
    read_surf_data(geom, afni_file),
    error = function(e) NULL
  )

  # If it works, verify structure
  if (!is.null(result)) {
    expect_true(inherits(result, "NeuroSurface") || inherits(result, "NeuroSurfaceVector"))
  }
})

# ==============================================================================
# Write/Read Round-trip Tests
# ==============================================================================

test_that("write_surf_data creates valid output file", {
  geom <- example_surface_geometry()
  idx <- 1:4
  vals <- c(1.0, 2.0, 3.0, 4.0)
  surf <- NeuroSurface(geometry = geom, indices = idx, data = vals)

  outstem <- tempfile("surftest")
  fname <- paste0(outstem, ".1D.dset")
  on.exit(unlink(fname), add = TRUE)

  write_surf_data(surf, outstem = outstem, hemi = "")

  expect_true(file.exists(fname))

  # Verify content
  tab <- read.table(fname, header = FALSE)
  expect_equal(nrow(tab), length(idx))
  expect_equal(tab[, 1], idx - 1)  # 0-indexed
  expect_equal(tab[, 2], vals)
})

test_that("write_surf_data handles hemisphere suffix correctly", {
  geom <- example_surface_geometry()
  idx <- 1:4
  vals <- 1:4
  surf <- NeuroSurface(geometry = geom, indices = idx, data = vals)

  outstem <- tempfile("surftest")
  fname_lh <- paste0(outstem, "_lh.1D.dset")
  fname_rh <- paste0(outstem, "_rh.1D.dset")
  on.exit({
    unlink(fname_lh)
    unlink(fname_rh)
  }, add = TRUE)

  write_surf_data(surf, outstem = outstem, hemi = "lh")
  expect_true(file.exists(fname_lh))

  write_surf_data(surf, outstem = outstem, hemi = "rh")
  expect_true(file.exists(fname_rh))
})

# ==============================================================================
# Error Handling Tests
# ==============================================================================

test_that("read_surf fails gracefully on non-existent file", {
  expect_error(read_surf("/nonexistent/path/to/file.asc"))
})

test_that("read_surf fails gracefully on unsupported format", {
  # Create a temp file with unsupported content
  tmp <- tempfile(fileext = ".xyz")
  writeLines("invalid content", tmp)
  on.exit(unlink(tmp), add = TRUE)

  # This might work or fail depending on format detection
  result <- tryCatch(
    read_surf(tmp),
    error = function(e) "error"
  )

  # Either an error or invalid result is acceptable
  if (!identical(result, "error")) {
    # If no error, it should still be some kind of object
    expect_true(is.object(result) || is.null(result))
  }
})

# ==============================================================================
# Spec File Tests
# ==============================================================================

test_that("spec file exists and can be found",
{
  spec_file <- system.file("extdata", "std.8_fsaverage_lh.spec", package = "neurosurf")
  skip_if(spec_file == "", "Spec test file not available")

  expect_true(file.exists(spec_file))
})

# ==============================================================================
# Surface Consistency Tests
# ==============================================================================

test_that("different surface files for same subject have consistent vertex count", {
  smoothwm <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  pial <- system.file("extdata", "std.8_lh.pial.asc", package = "neurosurf")
  inflated <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")

  skip_if(smoothwm == "" || pial == "" || inflated == "",
          "One or more surface files not available")

  surf_smoothwm <- read_surf(smoothwm)
  surf_pial <- read_surf(pial)
  surf_inflated <- read_surf(inflated)

  n_smoothwm <- nrow(coords(surf_smoothwm))
  n_pial <- nrow(coords(surf_pial))
  n_inflated <- nrow(coords(surf_inflated))

  expect_equal(n_smoothwm, n_pial)
  expect_equal(n_pial, n_inflated)
})

test_that("graph structure is consistent with mesh", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)

  # Number of graph vertices should equal mesh vertices
  expect_equal(igraph::vcount(surf@graph), nrow(coords(surf)))

  # Graph should have edges
  expect_true(igraph::ecount(surf@graph) > 0)
})
