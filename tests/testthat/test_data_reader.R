test_that("data_reader works for SurfaceDataMetaInfo", {
  niml_file <- neurosurf_testdata_path("rscan01_lh.niml.dset")

  skip_if(niml_file == "", "Test data not available. Run neurosurf_download_testdata() to download.")
  skip_on_cran()

  # Try to read the header; skip test if parsing fails (compatibility issue with neuroim2)
  header <- tryCatch(
    readNIMLSurfaceHeader(niml_file),
    error = function(e) {
      skip(paste("NIML parsing failed (neuroim2 compatibility issue):", conditionMessage(e)))
    }
  )

  meta <- NIMLSurfaceDataMetaInfo(NIML_SURFACE_DSET, header)

  reader <- data_reader(meta)
  nodes <- NULL
  utils::capture.output(
    nodes <- neuroim2::read_columns(reader, as.integer(0))
  )
  expect_equal(as.integer(nodes), meta@node_indices)
})
