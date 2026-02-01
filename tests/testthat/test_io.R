test_that("write_surf_data writes expected rows/cols", {
  geom <- example_surface_geometry()
  idx <- c(1L, 3L)
  vals <- c(10, 20)
  surf <- NeuroSurface(geometry = geom, indices = idx, data = vals)

  outstem <- tempfile("surfdata")
  fname <- paste0(outstem, ".1D.dset")
  on.exit(unlink(fname), add = TRUE)

  write_surf_data(surf, outstem = outstem, hemi = "")
  tab <- read.table(fname, header = FALSE)
  expect_equal(nrow(tab), length(idx))
  expect_equal(tab[, 1], idx - 1)
  expect_equal(tab[, 2], vals)
})

test_that("write_surf_data handles NeuroSurfaceVector", {
  geom <- example_surface_geometry()
  idx <- 1:4
  mat <- matrix(1:8, nrow = 4, ncol = 2)
  surfv <- NeuroSurfaceVector(geometry = geom, indices = idx, mat = mat)

  outstem <- tempfile("surfvec")
  fname <- paste0(outstem, "_lh.1D.dset")
  on.exit(unlink(fname), add = TRUE)

  write_surf_data(surfv, outstem = outstem, hemi = "lh")
  tab <- read.table(fname, header = FALSE)
  expect_equal(nrow(tab), length(idx))
  expect_equal(tab[, 1], idx - 1)
  expect_equal(unname(as.matrix(tab[, -1])), unname(mat))
})

test_that("read_surf_geometry works with FreeSurfer ASCII format", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)

  expect_s4_class(geom, "SurfaceGeometry")
  expect_true(nrow(vertices(geom)) > 0)
  expect_true(nrow(faces(geom)) > 0)
  expect_equal(geom@hemi, "lh")
})

test_that("read_surf_geometry detects correct hemisphere from filename", {
  # Test left hemisphere
  lh_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(lh_file == "", "std.8 surface not available")
  lh_geom <- read_surf_geometry(lh_file)
  expect_equal(lh_geom@hemi, "lh")

  # Test right hemisphere
  rh_file <- system.file("extdata", "std.8_rh.smoothwm.asc", package = "neurosurf")
  skip_if(rh_file == "", "std.8 rh surface not available")
  rh_geom <- read_surf_geometry(rh_file)
  expect_equal(rh_geom@hemi, "rh")
})

test_that("SurfaceGeometrySource creates correct source", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  src <- SurfaceGeometrySource(surf_file)

  expect_s4_class(src, "SurfaceGeometrySource")
})

test_that("load_data works on SurfaceGeometrySource", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  src <- SurfaceGeometrySource(surf_file)
  geom <- load_data(src)

  expect_s4_class(geom, "SurfaceGeometry")
})

test_that("read_surf works with geometry only", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf(surf_file)

  expect_s4_class(geom, "SurfaceGeometry")
})

test_that("read_surf returns correct vertex and face counts", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf(surf_file)

  # std.8 has 642 vertices
  expect_equal(nrow(vertices(geom)), 642)
  # std.8 has 1280 faces
  expect_equal(nrow(faces(geom)), 1280)
})

test_that("geometry graph is valid igraph object", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)
  g <- graph(geom)

  expect_s3_class(g, "igraph")
  expect_equal(igraph::vcount(g), nrow(vertices(geom)))
})

test_that("vertices have correct dimensions", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)
  v <- vertices(geom)

  expect_true(is.matrix(v))
  expect_equal(ncol(v), 3)  # x, y, z
})

test_that("faces have correct dimensions", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)
  f <- faces(geom)

  expect_true(is.matrix(f))
  expect_equal(ncol(f), 3)  # triangles have 3 vertices
})

test_that("readFreesurferAsciiHeader detects hemi from filename patterns", {
  # Test that the _lh pattern is detected
  lh_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(lh_file == "", "std.8 surface not available")

  # Internal function, access via namespace
  header <- neurosurf:::readFreesurferAsciiHeader(lh_file)

  expect_equal(header$hemi, "lh")
})

test_that("findSurfaceDescriptor identifies FreeSurfer ASCII format", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  desc <- neurosurf:::findSurfaceDescriptor(surf_file)

  expect_s4_class(desc, "FreesurferAsciiSurfaceFileDescriptor")
})

test_that("findSurfaceDescriptor identifies multiple surface types", {
  # Test .asc extension
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 inflated surface not available")

  desc <- neurosurf:::findSurfaceDescriptor(surf_file)
  expect_s4_class(desc, "FreesurferAsciiSurfaceFileDescriptor")
})

test_that("read_surf_geometry returns consistent coords and vertices", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)

  # coords and vertices(geom) should return the same data
  all_coords <- coords(geom)
  all_verts <- vertices(geom)

  expect_equal(all_coords, all_verts)
})

test_that("read_surf_geometry returns valid node sequence", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)
  n <- nodes(geom)

  expect_equal(n, seq_len(nrow(vertices(geom))))
})

test_that("read_surf can load different surface types", {
  # Test with inflated surface
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 inflated surface not available")

  geom <- read_surf(surf_file)

  expect_s4_class(geom, "SurfaceGeometry")
  expect_equal(nrow(vertices(geom)), 642)  # std.8 has 642 vertices
})

test_that("FreesurferSurfaceGeometryMetaInfo stores correct info", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  desc <- neurosurf:::findSurfaceDescriptor(surf_file)
  header <- neurosurf:::readFreesurferAsciiHeader(surf_file)
  meta <- neurosurf:::FreesurferSurfaceGeometryMetaInfo(desc, header)

  expect_s4_class(meta, "FreesurferSurfaceGeometryMetaInfo")
})

test_that("write_surf_data creates correctly formatted output", {
  # Test that indices are 0-based in output
  geom <- example_surface_geometry()
  idx <- c(1L, 2L, 4L)  # 1-based in R
  vals <- c(1.5, 2.5, 4.5)
  surf <- NeuroSurface(geometry = geom, indices = idx, data = vals)

  outstem <- tempfile("surfdata_format")
  fname <- paste0(outstem, ".1D.dset")
  on.exit(unlink(fname), add = TRUE)

  write_surf_data(surf, outstem = outstem, hemi = "")
  tab <- read.table(fname, header = FALSE)

  # Indices in file should be 0-based (0, 1, 3)
  expect_equal(tab[, 1], c(0, 1, 3))
  expect_equal(tab[, 2], vals)
})


# Tests for NIML parsing ---------------------------------------------------

test_that(".parse_niml_element parses simple element", {
  result <- neurosurf:::.parse_niml_element("SPARSE_DATA")
  expect_equal(result$label, "SPARSE_DATA")
  expect_null(result$attr)
})

test_that(".parse_niml_element parses element with attributes", {
  result <- neurosurf:::.parse_niml_element("SPARSE_DATA ni_type=float ni_dimen=100")
  expect_equal(result$label, "SPARSE_DATA")
  expect_equal(result$attr$ni_type, "float")
  expect_equal(result$attr$ni_dimen, "100")
})

test_that(".parse_niml_element handles empty input", {
  result <- neurosurf:::.parse_niml_element("")
  expect_null(result$label)
  expect_null(result$attr)
})


# Tests for SurfaceGeometry transforms -------------------------------------

test_that("SurfaceGeometry has identity surf_to_world by default", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)

  # Check surf_to_world is a 4x4 identity matrix by default
  xform <- surf_to_world(geom)
  expect_true(is.matrix(xform))
  expect_equal(dim(xform), c(4, 4))
  expect_equal(xform, diag(4))
})

test_that("surf_to_world setter works", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)

  # Set a custom transform
  new_xform <- matrix(c(1, 0, 0, 0,
                        0, 1, 0, 0,
                        0, 0, 1, 0,
                        10, 20, 30, 1), nrow = 4, byrow = TRUE)
  surf_to_world(geom) <- new_xform

  expect_equal(surf_to_world(geom), new_xform)
})


# Tests for show methods ---------------------------------------------------

test_that("show method works for SurfaceGeometryMetaInfo", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  desc <- neurosurf:::findSurfaceDescriptor(surf_file)
  header <- neurosurf:::readFreesurferAsciiHeader(surf_file)
  meta <- neurosurf:::FreesurferSurfaceGeometryMetaInfo(desc, header)

  output <- capture.output(show(meta))
  expect_true(length(output) > 0)
  expect_true(any(grepl("vertices", output)))
  expect_true(any(grepl("faces", output)))
})


# Tests for loadFSSurface --------------------------------------------------

test_that("loadFSSurface loads FreeSurfer ASCII surface", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  desc <- neurosurf:::findSurfaceDescriptor(surf_file)
  header <- neurosurf:::readFreesurferAsciiHeader(surf_file)
  meta <- neurosurf:::FreesurferSurfaceGeometryMetaInfo(desc, header)

  geom <- loadFSSurface(meta)

  expect_s4_class(geom, "SurfaceGeometry")
  expect_equal(nrow(vertices(geom)), 642)
})

test_that("loadFSSurface rejects invalid surf_to_world", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  desc <- neurosurf:::findSurfaceDescriptor(surf_file)
  header <- neurosurf:::readFreesurferAsciiHeader(surf_file)
  meta <- neurosurf:::FreesurferSurfaceGeometryMetaInfo(desc, header)

  # Wrong dimensions
  bad_xform <- matrix(1:9, nrow = 3, ncol = 3)

  expect_error(
    loadFSSurface(meta, surf_to_world = bad_xform),
    "4x4 matrix"
  )
})


# Tests for reading different surface types --------------------------------

test_that("all std.8 surface types can be read", {
  surf_types <- c("smoothwm", "pial", "inflated", "white", "sphere")

  for (surf_type in surf_types) {
    surf_file <- system.file("extdata", paste0("std.8_lh.", surf_type, ".asc"),
                             package = "neurosurf")
    if (surf_file != "" && file.exists(surf_file)) {
      geom <- read_surf_geometry(surf_file)
      expect_s4_class(geom, "SurfaceGeometry")
      expect_equal(nrow(vertices(geom)), 642)
    }
  }
})


# Tests for write_surf_data edge cases -------------------------------------

test_that("write_surf_data rejects invalid input", {
  expect_error(
    write_surf_data("not_a_surface", "output"),
    "inherit"
  )
})

test_that("write_surf_data handles empty hemi string", {
  geom <- example_surface_geometry()
  idx <- c(1L, 2L)
  vals <- c(1.0, 2.0)
  surf <- NeuroSurface(geometry = geom, indices = idx, data = vals)

  outstem <- tempfile("surfdata_nohemi")
  fname <- paste0(outstem, ".1D.dset")
  on.exit(unlink(fname), add = TRUE)

  write_surf_data(surf, outstem = outstem, hemi = "")

  expect_true(file.exists(fname))
})


# Tests for example_surface_geometry --------------------------------------

test_that("example_surface_geometry returns valid geometry", {
  geom <- example_surface_geometry()

  expect_s4_class(geom, "SurfaceGeometry")
  expect_gt(nrow(vertices(geom)), 0)
  expect_gt(nrow(faces(geom)), 0)
  expect_s3_class(graph(geom), "igraph")
})


# Tests for load_data methods ----------------------------------------------

test_that("load_data on FreesurferSurfaceGeometryMetaInfo returns SurfaceGeometry", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  desc <- neurosurf:::findSurfaceDescriptor(surf_file)
  header <- neurosurf:::readFreesurferAsciiHeader(surf_file)
  meta <- neurosurf:::FreesurferSurfaceGeometryMetaInfo(desc, header)

  geom <- load_data(meta)

  expect_s4_class(geom, "SurfaceGeometry")
})


# Tests for readFreesurferAsciiGeometry ------------------------------------

test_that("readFreesurferAsciiGeometry returns vertices and faces", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  result <- neurosurf:::readFreesurferAsciiGeometry(surf_file)

  expect_type(result, "list")
  expect_true("vertices" %in% names(result))
  expect_true("faces" %in% names(result))
  expect_equal(ncol(result$vertices), 3)
  expect_equal(ncol(result$faces), 3)
})


# Tests for FreesurferSurfaceGeometryMetaInfo ------------------------------

test_that("FreesurferSurfaceGeometryMetaInfo stores header info correctly", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  desc <- neurosurf:::findSurfaceDescriptor(surf_file)
  header <- neurosurf:::readFreesurferAsciiHeader(surf_file)
  meta <- neurosurf:::FreesurferSurfaceGeometryMetaInfo(desc, header)

  expect_s4_class(meta, "FreesurferSurfaceGeometryMetaInfo")
  expect_equal(meta@nvertices, 642)
  expect_equal(meta@nfaces, 1280)
})


# Tests for SurfaceGeometrySource load_data --------------------------------

test_that("SurfaceGeometrySource can load multiple surface types", {
  for (surf_type in c("smoothwm", "pial", "inflated")) {
    surf_file <- system.file("extdata", paste0("std.8_lh.", surf_type, ".asc"),
                             package = "neurosurf")
    if (surf_file != "" && file.exists(surf_file)) {
      src <- SurfaceGeometrySource(surf_file)
      geom <- load_data(src)
      expect_s4_class(geom, "SurfaceGeometry")
    }
  }
})


# Tests for SurfaceGeometry graph properties -------------------------------

test_that("graph edges have dist attribute", {
  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 surface not available")

  geom <- read_surf_geometry(surf_file)
  g <- graph(geom)

  # Check that edges have a dist attribute
  edge_attrs <- igraph::edge_attr_names(g)
  expect_true("dist" %in% edge_attrs)

  # Check that all distances are non-negative
  dists <- igraph::E(g)$dist
  expect_true(all(dists >= 0))
})


# Tests for readFreesurferBinaryHeader errors ------------------------------

test_that("readFreesurferBinaryHeader errors on invalid file", {
  # Create a file with invalid magic bytes
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  writeBin(as.raw(c(0, 0, 0, 1, 2, 3)), tf)

  expect_error(
    neurosurf:::readFreesurferBinaryHeader(tf),
    ignore.case = TRUE
  )
})

test_that("readFreesurferBinaryGeometry errors on invalid file", {
  tf <- tempfile()
  on.exit(unlink(tf), add = TRUE)
  writeBin(as.raw(c(0, 0, 0, 1, 2, 3)), tf)

  expect_error(
    neurosurf:::readFreesurferBinaryGeometry(tf),
    ignore.case = TRUE
  )
})


# Tests for findSurfaceDescriptor edge cases -------------------------------

test_that("findSurfaceDescriptor handles various FreeSurfer extensions", {
  # Test with different surface types
  for (surf_type in c("smoothwm", "pial", "inflated")) {
    surf_file <- system.file("extdata", paste0("std.8_lh.", surf_type, ".asc"),
                             package = "neurosurf")
    if (surf_file != "" && file.exists(surf_file)) {
      desc <- neurosurf:::findSurfaceDescriptor(surf_file)
      expect_s4_class(desc, "FreesurferAsciiSurfaceFileDescriptor")
    }
  }
})


# Tests for hemisphere detection -------------------------------------------

test_that("readFreesurferAsciiHeader detects rh from filename", {
  rh_file <- system.file("extdata", "std.8_rh.smoothwm.asc", package = "neurosurf")
  skip_if(rh_file == "", "std.8 rh surface not available")

  header <- neurosurf:::readFreesurferAsciiHeader(rh_file)

  expect_equal(header$hemi, "rh")
})


# Tests for write_surf_data with hemi prefix -------------------------------

test_that("write_surf_data adds hemi prefix to filename", {
  geom <- example_surface_geometry()
  idx <- c(1L, 2L)
  vals <- c(1.0, 2.0)
  surf <- NeuroSurface(geometry = geom, indices = idx, data = vals)

  outstem <- tempfile("surfdata_hemi")
  fname_lh <- paste0(outstem, "_lh.1D.dset")
  on.exit(unlink(fname_lh), add = TRUE)

  write_surf_data(surf, outstem = outstem, hemi = "lh")

  expect_true(file.exists(fname_lh))
})

test_that("write_surf_data works with rh hemi", {
  geom <- example_surface_geometry()
  idx <- c(1L, 2L)
  vals <- c(1.0, 2.0)
  surf <- NeuroSurface(geometry = geom, indices = idx, data = vals)

  outstem <- tempfile("surfdata_rh")
  fname_rh <- paste0(outstem, "_rh.1D.dset")
  on.exit(unlink(fname_rh), add = TRUE)

  write_surf_data(surf, outstem = outstem, hemi = "rh")

  expect_true(file.exists(fname_rh))
})


# Tests for multiple surface files -----------------------------------------

test_that("read_surf works for sphere surface", {
  surf_file <- system.file("extdata", "std.8_lh.sphere.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 sphere surface not available")

  geom <- read_surf(surf_file)

  expect_s4_class(geom, "SurfaceGeometry")
  expect_equal(nrow(vertices(geom)), 642)
})

test_that("read_surf works for sphere.reg surface", {
  surf_file <- system.file("extdata", "std.8_lh.sphere.reg.asc", package = "neurosurf")
  skip_if(surf_file == "", "std.8 sphere.reg surface not available")

  geom <- read_surf(surf_file)

  expect_s4_class(geom, "SurfaceGeometry")
})
