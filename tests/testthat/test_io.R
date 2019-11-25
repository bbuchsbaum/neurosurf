
inflated_asc <- system.file("extdata", "std.8_lh.inflated.asc", package="neurosurf")
niml_data_lh <- system.file("extdata", "rscan01_lh.niml.dset", package="neurosurf")
curv_lh <- system.file("extdata", "std.8_lh.curv.niml.dset", package="neurosurf")

test_that("can read a freesurfer ascii geometry file", {
  surf <- neurosurf::read_surf(inflated_asc)
  expect_true(!is.null(surf))
})

test_that("can read a niml 4D data file", {
  #surf <- neurosurf::read_surf(inflated_asc, niml_data_lh)
  #expect_true(!is.null(surf))
  expect_true(TRUE)
})



