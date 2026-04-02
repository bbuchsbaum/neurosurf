# Tests for snapshot_surface helpers

test_that(".ns_snapshot_is_blank detects uniform placeholder frames", {
  black_file <- tempfile(fileext = ".png")
  white_file <- tempfile(fileext = ".png")
  gray_file <- tempfile(fileext = ".png")
  valid_file <- tempfile(fileext = ".png")

  png::writePNG(array(0, dim = c(30, 30, 3)), black_file)
  png::writePNG(array(1, dim = c(30, 30, 3)), white_file)
  png::writePNG(array(0.5, dim = c(30, 30, 3)), gray_file)

  valid_img <- array(1, dim = c(40, 40, 3))
  valid_img[10:30, 12:28, ] <- 0.45
  png::writePNG(valid_img, valid_file)

  expect_true(neurosurf:::.ns_snapshot_is_blank(black_file))
  expect_true(neurosurf:::.ns_snapshot_is_blank(white_file))
  expect_true(neurosurf:::.ns_snapshot_is_blank(gray_file))
  expect_false(neurosurf:::.ns_snapshot_is_blank(valid_file))
})

test_that(".ns_snapshot_is_blank handles missing files", {
  expect_true(neurosurf:::.ns_snapshot_is_blank(""))
  expect_true(neurosurf:::.ns_snapshot_is_blank(tempfile(fileext = ".png")))
})
