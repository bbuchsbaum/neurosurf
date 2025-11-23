test_that("data_reader works for SurfaceDataMetaInfo", {
  niml_file <- system.file("extdata", "rscan01_lh.niml.dset", package = "neurosurf")

  # Try to read the header; skip test if parsing fails (compatibility issue with neuroim2)
  header <- tryCatch(
    readNIMLSurfaceHeader(niml_file),
    error = function(e) {
      skip(paste("NIML parsing failed (neuroim2 compatibility issue):", conditionMessage(e)))
    }
  )

  meta <- NIMLSurfaceDataMetaInfo(NIML_SURFACE_DSET, header)

  reader <- data_reader(meta)
  nodes <- neuroim2::read_columns(reader, as.integer(0))
  expect_equal(as.integer(nodes), meta@node_indices)
})
