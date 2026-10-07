test_that("NIML synthetic data supports ColumnReader indices and values", {
  dest <- tempfile()
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE))
  file <- neurosurf_generate_testdata("rscan01_lh.niml.dset", dest,
                                     quiet = TRUE)
  header <- readNIMLSurfaceHeader(file)
  meta <- NIMLSurfaceDataMetaInfo(NIML_SURFACE_DSET, header)
  reader <- data_reader(meta)
  nodes <- neuroim2::read_columns(reader, 0L)
  expect_identical(as.integer(nodes), 0:15)
  columns <- neuroim2::read_columns(reader, c(1L, 4L))
  expect_equal(dim(columns), c(16L, 2L))
  expect_equal(columns[1, ], sin(1 / 3) + cos(c(1, 4) / 5),
               tolerance = 1e-6)
})
