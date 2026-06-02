# Tests for IO.R file format support
# This file adds coverage for internal parsing functions and file descriptors

# Tests for NIML parsing internal functions --------------------------------

test_that(".parse_niml_element handles multiple attributes", {
  result <- neurosurf:::.parse_niml_element(
    "SPARSE_DATA ni_type=3*float ni_dimen=500 ni_form=binary.lsbfirst"
  )
  expect_equal(result$label, "SPARSE_DATA")
  expect_equal(result$attr$ni_type, "3*float")
  expect_equal(result$attr$ni_dimen, "500")
  expect_equal(result$attr$ni_form, "binary.lsbfirst")
})

test_that(".parse_niml_element handles trailing >", {
  result <- neurosurf:::.parse_niml_element("INDEX_LIST ni_dimen=100 >")
  expect_equal(result$label, "INDEX_LIST")
  expect_equal(result$attr$ni_dimen, "100")
})

test_that(".parse_niml_element returns NULL attr for single label", {
  result <- neurosurf:::.parse_niml_element("AFNI_atr")
  expect_equal(result$label, "AFNI_atr")
  expect_null(result$attr)
})

test_that(".parse_niml_element handles whitespace", {
  result <- neurosurf:::.parse_niml_element("  LABEL  key=value  ")
  expect_equal(result$label, "LABEL")
  expect_equal(result$attr$key, "value")
})


# Tests for file descriptor constants --------------------------------------

test_that("file descriptor constants are defined correctly", {
  expect_s4_class(neurosurf:::GIFTI_SURFACE_DSET, "GIFTISurfaceFileDescriptor")
  expect_s4_class(neurosurf:::GIFTI_GZ_SURFACE_DSET, "GIFTISurfaceFileDescriptor")
  expect_s4_class(neurosurf:::NIML_SURFACE_DSET, "NIMLSurfaceFileDescriptor")
  expect_s4_class(neurosurf:::AFNI_SURFACE_DSET, "AFNISurfaceFileDescriptor")
  expect_s4_class(neurosurf:::FREESURFER_ASCII_SURFACE_DSET, "FreesurferAsciiSurfaceFileDescriptor")
  expect_s4_class(neurosurf:::FREESURFER_BINARY_SURFACE_DSET, "FreesurferBinarySurfaceFileDescriptor")
})

test_that("file descriptor file_format slots are set correctly", {
  expect_equal(neurosurf:::GIFTI_SURFACE_DSET@file_format, "GIFTI")
  expect_equal(neurosurf:::NIML_SURFACE_DSET@file_format, "NIML")
  expect_equal(neurosurf:::AFNI_SURFACE_DSET@file_format, "1D")
  expect_equal(neurosurf:::FREESURFER_ASCII_SURFACE_DSET@file_format, "Freesurfer_ASCII")
  expect_equal(neurosurf:::FREESURFER_BINARY_SURFACE_DSET@file_format, "Freesurfer_BINARY")
})

test_that("file descriptor header_extension slots are correct", {
  expect_equal(neurosurf:::GIFTI_SURFACE_DSET@header_extension, "gii")
  expect_equal(neurosurf:::GIFTI_GZ_SURFACE_DSET@header_extension, "gii.gz")
  expect_equal(neurosurf:::NIML_SURFACE_DSET@header_extension, "niml.dset")
  expect_equal(neurosurf:::AFNI_SURFACE_DSET@header_extension, "1D.dset")
  expect_equal(neurosurf:::FREESURFER_ASCII_SURFACE_DSET@header_extension, "asc")
})


# Tests for findSurfaceDescriptor with different extensions ----------------
# Note: findSurfaceDescriptor uses neuroim2::file_matches which checks file patterns
# not just extensions. For non-matching patterns, it defaults to FreesurferBinaryDescriptor.

test_that("findSurfaceDescriptor identifies FreeSurfer ASCII from .asc extension", {
  # Real file test - this uses the actual file matching
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  desc <- neurosurf:::findSurfaceDescriptor(surf_file)
  expect_s4_class(desc, "FreesurferAsciiSurfaceFileDescriptor")
})

test_that("findSurfaceDescriptor defaults to binary for unknown extensions", {
  desc <- neurosurf:::findSurfaceDescriptor("lh.pial")
  expect_s4_class(desc, "FreesurferBinarySurfaceFileDescriptor")

  desc2 <- neurosurf:::findSurfaceDescriptor("rh.white")
  expect_s4_class(desc2, "FreesurferBinarySurfaceFileDescriptor")
})

test_that("findSurfaceDescriptor handles FreeSurfer standard names", {
  for (name in c("lh.pial", "rh.white", "lh.inflated", "rh.sphere")) {
    desc <- neurosurf:::findSurfaceDescriptor(name)
    expect_s4_class(desc, "FreesurferBinarySurfaceFileDescriptor")
  }
})


# Tests for read_meta_info methods -----------------------------------------

test_that("read_meta_info works for FreesurferAsciiSurfaceFileDescriptor", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  desc <- neurosurf:::FREESURFER_ASCII_SURFACE_DSET
  meta <- read_meta_info(desc, surf_file)

  expect_s4_class(meta, "FreesurferSurfaceGeometryMetaInfo")
  expect_equal(meta@vertices, 642L)
  expect_equal(meta@faces, 1280L)
})

test_that("read_meta_info stores correct hemisphere", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  desc <- neurosurf:::FREESURFER_ASCII_SURFACE_DSET
  meta <- read_meta_info(desc, surf_file)

  expect_equal(meta@hemi, "lh")
})


# Tests for write_surf_data with NeuroSurfaceVector ------------------------

test_that("write_surf_data handles multi-column NeuroSurfaceVector", {
  geom <- example_surface_geometry()
  idx <- 1:10
  mat <- matrix(rnorm(30), nrow = 10, ncol = 3)
  surfv <- NeuroSurfaceVector(geometry = geom, indices = idx, mat = mat)

  outstem <- tempfile("surfvec_multi")
  fname <- paste0(outstem, ".1D.dset")
  on.exit(unlink(fname), add = TRUE)

  write_surf_data(surfv, outstem = outstem, hemi = "")
  tab <- read.table(fname, header = FALSE)

  expect_equal(nrow(tab), 10)
  expect_equal(ncol(tab), 4)  # node index + 3 data columns
})

test_that("write_surf_data preserves data values", {
  geom <- example_surface_geometry()
  idx <- c(1L, 5L, 10L)
  vals <- c(1.5, 2.5, 3.5)
  surf <- NeuroSurface(geometry = geom, indices = idx, data = vals)

  outstem <- tempfile("surfdata_vals")
  fname <- paste0(outstem, ".1D.dset")
  on.exit(unlink(fname), add = TRUE)

  write_surf_data(surf, outstem = outstem, hemi = "")
  tab <- read.table(fname, header = FALSE)

  expect_equal(tab[, 2], vals)
})


# Tests for SurfaceDataMetaInfo constructor --------------------------------

test_that("SurfaceDataMetaInfo validates required fields", {
  desc <- neurosurf:::AFNI_SURFACE_DSET
  header <- list(
    header_file = "test.1D.dset",
    data_file = "test.1D.dset",
    nodes = c(0, 1, 2),
    nels = 2,
    label = "test"
  )

  meta <- neurosurf:::SurfaceDataMetaInfo(desc, header)
  expect_s4_class(meta, "SurfaceDataMetaInfo")
})

test_that("SurfaceDataMetaInfo stores correct values", {
  desc <- neurosurf:::AFNI_SURFACE_DSET
  # Note: nodes is converted to node_count as-is (as integer vector)
  header <- list(
    header_file = "myfile.1D.dset",
    data_file = "myfile.1D.dset",
    nodes = 100,  # Single node count value
    nels = 5,
    label = "mydata"
  )

  meta <- neurosurf:::SurfaceDataMetaInfo(desc, header)

  expect_equal(meta@header_file, "myfile.1D.dset")
  expect_equal(meta@node_count, 100L)
  expect_equal(meta@nels, 5L)
  expect_equal(meta@label, "mydata")
})


# Tests for show method on SurfaceDataMetaInfo -----------------------------

test_that("show method works for SurfaceDataMetaInfo", {
  desc <- neurosurf:::AFNI_SURFACE_DSET
  header <- list(
    header_file = "test.1D.dset",
    data_file = "test.1D.dset",
    nodes = c(0, 1, 2),
    nels = 2,
    label = "test"
  )

  meta <- neurosurf:::SurfaceDataMetaInfo(desc, header)
  output <- capture.output(show(meta))

  expect_true(length(output) > 0)
  expect_true(any(grepl("node_count", output)))
})


# Tests for extract_gifti_transform ----------------------------------------

test_that(".extract_gifti_transform returns identity for empty input", {
  gii <- list(data_info = NULL)
  xform <- neurosurf:::.extract_gifti_transform(gii)

  expect_equal(dim(xform), c(4, 4))
  expect_equal(xform, diag(4))
})

test_that(".extract_gifti_transform returns identity when no transform in data_info", {
  gii <- list(data_info = list(
    list(intent = "NIFTI_INTENT_POINTSET", trans_mat = NULL)
  ))
  xform <- neurosurf:::.extract_gifti_transform(gii)

  expect_equal(xform, diag(4))
})

test_that(".extract_gifti_transform extracts valid transform", {
  custom_xform <- matrix(c(
    1, 0, 0, 10,
    0, 1, 0, 20,
    0, 0, 1, 30,
    0, 0, 0, 1
  ), nrow = 4, byrow = TRUE)

  gii <- list(data_info = list(
    list(intent = "NIFTI_INTENT_POINTSET", trans_mat = custom_xform)
  ))
  xform <- neurosurf:::.extract_gifti_transform(gii)

  expect_equal(xform, custom_xform)
})

test_that(".extract_gifti_transform handles modern gifti (data.frame data_info)", {
  # Regression for #68: gifti >= 0.8 returns data_info as a data.frame and
  # stores parsed transforms in $parsed_transformations. The old code did
  # `info$trans_mat` on each column (an atomic vector), erroring with
  # "$ operator is invalid for atomic vectors".
  custom_xform <- matrix(c(
    2, 0, 0, 5,
    0, 2, 0, 6,
    0, 0, 2, 7,
    0, 0, 0, 1
  ), nrow = 4, byrow = TRUE)

  gii <- list(
    data_info = data.frame(
      Intent = c("NIFTI_INTENT_POINTSET", "NIFTI_INTENT_TRIANGLE"),
      name = c("pointset", "triangle"),
      Dim0 = c("10242", "20480"),
      stringsAsFactors = FALSE
    ),
    # One entry per data array; pointset carries the transform, triangle none.
    parsed_transformations = list(list(custom_xform), list())
  )

  xform <- neurosurf:::.extract_gifti_transform(gii)
  expect_equal(xform, custom_xform)
})

test_that(".extract_gifti_transform prefers the POINTSET data array transform", {
  pointset_xform <- diag(c(3, 3, 3, 1))
  triangle_xform <- diag(c(9, 9, 9, 1))

  gii <- list(
    data_info = data.frame(
      Intent = c("NIFTI_INTENT_TRIANGLE", "NIFTI_INTENT_POINTSET"),
      name = c("triangle", "pointset"),
      stringsAsFactors = FALSE
    ),
    # Note: triangle is first; the loader must still pick the pointset's matrix.
    parsed_transformations = list(triangle_xform, pointset_xform)
  )

  xform <- neurosurf:::.extract_gifti_transform(gii)
  expect_equal(xform, pointset_xform)
})

test_that(".extract_gifti_transform does not error on data.frame without transform", {
  gii <- list(
    data_info = data.frame(
      Intent = c("NIFTI_INTENT_POINTSET", "NIFTI_INTENT_TRIANGLE"),
      name = c("pointset", "triangle"),
      stringsAsFactors = FALSE
    ),
    parsed_transformations = list(list(), list())
  )

  expect_silent(xform <- neurosurf:::.extract_gifti_transform(gii))
  expect_equal(xform, diag(4))
})

test_that(".extract_gifti_transform falls back to raw transformations field", {
  custom_xform <- diag(c(4, 4, 4, 1))
  gii <- list(
    data_info = NULL,
    transformations = list(list(custom_xform))
  )
  expect_equal(neurosurf:::.extract_gifti_transform(gii), custom_xform)
})

test_that(".find_4x4 locates matrices in nested structures and rejects others", {
  m <- diag(4)
  expect_equal(neurosurf:::.find_4x4(m), m)
  expect_equal(neurosurf:::.find_4x4(list(list(list(m)))), m)
  # Length-16 row-major vector is reshaped to a 4x4 matrix
  vec <- 1:16
  expect_equal(neurosurf:::.find_4x4(vec),
               matrix(vec, 4, 4, byrow = TRUE))
  expect_null(neurosurf:::.find_4x4(NULL))
  expect_null(neurosurf:::.find_4x4(matrix(0, 3, 3)))
  expect_null(neurosurf:::.find_4x4("not a matrix"))
})


# Tests for error paths in binary reader -----------------------------------

test_that("readFreesurferBinaryHeader fails on empty file", {
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  file.create(tf)  # Create empty file

  expect_error(
    neurosurf:::readFreesurferBinaryHeader(tf),
    "too small"
  )
})

test_that("readFreesurferBinaryGeometry fails on empty file", {
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  file.create(tf)  # Create empty file

  expect_error(
    neurosurf:::readFreesurferBinaryGeometry(tf),
    "too small"
  )
})

test_that("readFreesurferBinaryHeader fails on truncated file", {
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  writeBin(as.raw(c(0, 0, 0)), tf)  # Only 3 bytes

  expect_error(
    neurosurf:::readFreesurferBinaryHeader(tf),
    "too small"
  )
})


# Tests for NeuroSurfaceSource construction --------------------------------

test_that("NeuroSurfaceSource can be created with geometry object", {
  geom <- example_surface_geometry()

  # Create a temporary 1D.dset file
  idx <- 1:10
  vals <- rnorm(10)
  surf <- NeuroSurface(geometry = geom, indices = idx, data = vals)

  outstem <- tempfile("src_test")
  fname <- paste0(outstem, ".1D.dset")
  on.exit(unlink(fname), add = TRUE)
  write_surf_data(surf, outstem = outstem, hemi = "")

  # Create source with geometry object
  src <- NeuroSurfaceSource(geom, fname)
  expect_s4_class(src, "NeuroSurfaceSource")
})


# Tests for .readHeader internal function ----------------------------------

test_that(".readHeader returns correct structure", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  header <- neurosurf:::.readHeader(surf_file)

  expect_s4_class(header, "FreesurferSurfaceGeometryMetaInfo")
})

test_that(".readHeader fails on unknown file type", {
  tf <- tempfile(fileext = ".unknown")
  on.exit(unlink(tf), add = TRUE)
  writeLines("test content", tf)

  # This should fail since we can't find a reader for .unknown
  expect_error(neurosurf:::.readHeader(tf))
})


# Tests for hemisphere pattern detection -----------------------------------

test_that("readFreesurferAsciiHeader detects lh from various patterns", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  header <- neurosurf:::readFreesurferAsciiHeader(surf_file)
  expect_equal(header$hemi, "lh")
})

test_that("readFreesurferAsciiHeader detects rh from filename", {
  rh_file <- system.file("extdata", "std.8_rh.smoothwm.asc", package = "neurosurf")
  skip_if(rh_file == "", "std.8 rh surface not available")

  header <- neurosurf:::readFreesurferAsciiHeader(rh_file)
  expect_equal(header$hemi, "rh")
})

test_that("GIFTISurfaceGeometryMetaInfo detects hemisphere from BIDS naming", {
  desc <- neurosurf:::GIFTI_SURFACE_DSET
  make_header <- function(fname) {
    info <- list(data_info = data.frame(
      name = c("pointset", "triangle"),
      Dim0 = c("32492", "64980"),
      stringsAsFactors = FALSE
    ))
    class(info) <- "gifti"  # info slot is typed as the S3 "gifti" class
    list(
      header_file = fname,
      data_file = fname,
      label = "midthickness",
      info = info
    )
  }

  meta_l <- neurosurf:::GIFTISurfaceGeometryMetaInfo(
    desc, make_header("tpl-fsLR_den-32k_hemi-L_midthickness.surf.gii"))
  expect_equal(meta_l@hemi, "lh")

  meta_r <- neurosurf:::GIFTISurfaceGeometryMetaInfo(
    desc, make_header("tpl-fsLR_den-32k_hemi-R_midthickness.surf.gii"))
  expect_equal(meta_r@hemi, "rh")

  # FreeSurfer-style naming still works
  meta_fs <- neurosurf:::GIFTISurfaceGeometryMetaInfo(
    desc, make_header("lh.white.surf.gii"))
  expect_equal(meta_fs@hemi, "lh")
})


# Tests for FreesurferSurfaceGeometryMetaInfo constructor ------------------

test_that("FreesurferSurfaceGeometryMetaInfo validates numeric vertices", {
  desc <- neurosurf:::FREESURFER_ASCII_SURFACE_DSET
  header <- list(
    header_file = "test.asc",
    data_file = "test.asc",
    vertices = 100,
    faces = 200,
    embed_dimension = 3,
    label = "test",
    hemi = "lh"
  )

  meta <- neurosurf:::FreesurferSurfaceGeometryMetaInfo(desc, header)
  expect_s4_class(meta, "FreesurferSurfaceGeometryMetaInfo")
  expect_equal(meta@vertices, 100L)
  expect_equal(meta@faces, 200L)
})


# Tests for NIMLSurfaceDataMetaInfo constructor ----------------------------

test_that("NIMLSurfaceDataMetaInfo stores data correctly", {
  desc <- neurosurf:::NIML_SURFACE_DSET
  test_data <- matrix(1:12, nrow = 3, ncol = 4)
  header <- list(
    header_file = "test.niml.dset",
    data_file = "test.niml.dset",
    node_count = 3,
    nels = 4,
    label = "test_niml",
    data = test_data,
    nodes = c(0, 1, 2)
  )

  meta <- neurosurf:::NIMLSurfaceDataMetaInfo(desc, header)

  expect_s4_class(meta, "NIMLSurfaceDataMetaInfo")
  expect_equal(meta@node_count, 3L)
  expect_equal(meta@nels, 4L)
  expect_equal(dim(meta@data), c(3, 4))
})


# Tests for data encoding slots --------------------------------------------

test_that("file descriptors have consistent encoding slots", {
  # GIFTI
  expect_equal(neurosurf:::GIFTI_SURFACE_DSET@data_encoding, "gii")
  expect_equal(neurosurf:::GIFTI_SURFACE_DSET@data_extension, "gii")

  # NIML
  expect_equal(neurosurf:::NIML_SURFACE_DSET@data_encoding, "raw")

  # FreeSurfer ASCII
  expect_equal(neurosurf:::FREESURFER_ASCII_SURFACE_DSET@header_encoding, "text")
  expect_equal(neurosurf:::FREESURFER_ASCII_SURFACE_DSET@data_encoding, "raw")
})
