test_that("read_freesurfer_annot closes connection on error", {
  f <- neurosurf_testdata_path("rscan01_lh.gii")

  skip_if(f == "", "Test data not available. Run neurosurf_download_testdata() to download.")
  skip_on_cran()

  before <- showConnections(all = TRUE)
  expect_error(read_freesurfer_annot(f, NULL))
  after <- showConnections(all = TRUE)
  expect_equal(nrow(after), nrow(before))
})

test_that("readFreesurferBinaryHeader closes connection on error", {
  tf <- tempfile()
  writeBin(as.raw(1:10), tf)
  before <- showConnections(all = TRUE)
  expect_error(readFreesurferBinaryHeader(tf))
  after <- showConnections(all = TRUE)
  expect_equal(nrow(after), nrow(before))
})

test_that("readFreesurferBinaryGeometry closes connection on error", {
  tf <- tempfile()
  writeBin(as.raw(1:10), tf)
  before <- showConnections(all = TRUE)
  expect_error(readFreesurferBinaryGeometry(tf))
  after <- showConnections(all = TRUE)
  expect_equal(nrow(after), nrow(before))
})
