# Tests for testdata.R - optional test data download functionality

test_that("neurosurf_cache_dir returns a string", {
  cache_dir <- neurosurf_cache_dir()
  expect_type(cache_dir, "character")
  expect_true(nchar(cache_dir) > 0)
})

test_that("neurosurf_cache_dir returns path containing neurosurf", {
  cache_dir <- neurosurf_cache_dir()
  expect_true(grepl("neurosurf", cache_dir))
})

test_that("neurosurf_has_testdata returns logical", {
  result <- neurosurf_has_testdata("nonexistent_file.xyz")
  expect_type(result, "logical")
  expect_false(result)
})

test_that("neurosurf_has_testdata finds package extdata files", {
  # std.8 surfaces are included in the package
  result <- neurosurf_has_testdata("std.8_lh.smoothwm.asc")
  expect_true(result)
})

test_that("neurosurf_testdata_path returns string", {
  path <- neurosurf_testdata_path("nonexistent_file.xyz")
  expect_type(path, "character")
  expect_equal(path, "")
})

test_that("neurosurf_testdata_path finds package extdata files", {
  path <- neurosurf_testdata_path("std.8_lh.smoothwm.asc")
  expect_true(nchar(path) > 0)
  expect_true(file.exists(path))
})

test_that("neurosurf_download_testdata validates file names", {
  expect_error(
    neurosurf_download_testdata("invalid_file.xyz"),
    "Unknown files"
  )
})

test_that("neurosurf_download_testdata lists available files in error", {
  expect_error(
    neurosurf_download_testdata("invalid_file.xyz"),
    "rscan01_lh.gii"
  )
})

test_that("neurosurf_download_testdata validates destdir", {
  expect_error(
    neurosurf_download_testdata("rscan01_lh.gii", destdir = "/nonexistent/path/xyz123"),
    "does not exist"
  )
})

test_that("neurosurf_download_testdata accepts valid file names", {
  # This test checks that valid file names don't error on the validation step
  # We use a temp dir that exists but skip actual download
  skip_if_offline()

  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # The download will fail because testdata-v1 release may not exist
  # but the file validation should pass
  tryCatch({
    neurosurf_download_testdata("rscan01_lh.gii", destdir = temp_dir, quiet = TRUE)
  }, error = function(e) {
    # Download failure is expected, but we should not see "Unknown files" error
    expect_false(grepl("Unknown files", conditionMessage(e)))
  }, warning = function(w) {
    # Warnings about download failure are acceptable
    expect_true(grepl("Failed to download", conditionMessage(w)))
  })
})

test_that("neurosurf_download_testdata skips existing files by default", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create a dummy file
  dummy_file <- file.path(temp_dir, "rscan01_lh.gii")
  writeLines("dummy content", dummy_file)

  # Should report file exists and not try to download
  expect_message(
    neurosurf_download_testdata("rscan01_lh.gii", destdir = temp_dir, quiet = FALSE),
    "already exists"
  )
})

test_that("neurosurf_download_testdata returns paths invisibly", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create a dummy file
  dummy_file <- file.path(temp_dir, "rscan01_lh.gii")
  writeLines("dummy content", dummy_file)

  # Should return the path to existing file
  result <- neurosurf_download_testdata("rscan01_lh.gii", destdir = temp_dir, quiet = TRUE)
  expect_equal(result, dummy_file)
})

test_that("neurosurf_download_testdata handles 'all' files argument", {
  # Test that 'all' is properly expanded (validation only, no download)
  expect_error(
    neurosurf_download_testdata("all", destdir = "/nonexistent/path"),
    "does not exist"
  )
  # If we got this error, it means 'all' was valid and we proceeded to destdir check
})
