test_that("data_reader works for SurfaceDataMetaInfo", {
  niml_file <- system.file("extdata", "rscan01_lh.niml.dset", package = "neurosurf")
  header <- readNIMLSurfaceHeader(niml_file)
  meta <- NIMLSurfaceDataMetaInfo(NIML_SURFACE_DSET, header)

  reader <- data_reader(meta)
  nodes <- neuroim2::read_columns(reader, as.integer(0))
  expect_equal(as.integer(nodes), meta@node_indices)
})
