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
