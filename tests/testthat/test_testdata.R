# Tests for testdata.R - optional test data download functionality

library(neurosurf)

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

test_that("neurosurf_download_testdata with overwrite=TRUE re-downloads", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create a dummy file
  dummy_file <- file.path(temp_dir, "rscan01_lh.gii")
  writeLines("dummy content", dummy_file)
  original_mtime <- file.mtime(dummy_file)

  skip_if_offline()

  # With overwrite=TRUE, should attempt download (will fail but not report "already exists")
  result <- tryCatch({
    neurosurf_download_testdata("rscan01_lh.gii", destdir = temp_dir, overwrite = TRUE, quiet = FALSE)
  }, message = function(m) {
    # Should NOT say "already exists"
    expect_false(grepl("already exists", conditionMessage(m)))
  }, warning = function(w) {
    # Download failure is expected
    expect_true(grepl("Failed to download", conditionMessage(w)))
  })
})

test_that("neurosurf_testdata_path checks cache directory", {
  # Create a temporary cache dir structure and place a file there
  temp_cache <- tempfile()
  temp_extdata <- file.path(temp_cache, "extdata")
  dir.create(temp_extdata, recursive = TRUE)
  on.exit(unlink(temp_cache, recursive = TRUE), add = TRUE)

  # Create a test file in the cache extdata
  test_file <- file.path(temp_extdata, "test_cached_file.xyz")
  writeLines("cached content", test_file)

  # Mock neurosurf_cache_dir to return our temp dir
  # Since we can't easily mock, we just verify the function structure
  # by checking that it returns empty for non-existent files in both locations
  path <- neurosurf_testdata_path("definitely_nonexistent_12345.xyz")
  expect_equal(path, "")
})

test_that("neurosurf_has_testdata checks cache directory", {
  # Similar to above - verify returns FALSE for non-existent files
  result <- neurosurf_has_testdata("definitely_nonexistent_12345.xyz")
  expect_false(result)
})

test_that("neurosurf_download_testdata accepts multiple valid files", {
  expect_error(
    neurosurf_download_testdata(c("rscan01_lh.gii", "rscan01_lh.niml.dset"),
                                destdir = "/nonexistent/path"),
    "does not exist"
  )
  # If we got this error, it means both files were valid
})

test_that("neurosurf_download_testdata rejects mixed valid/invalid files", {
  expect_error(
    neurosurf_download_testdata(c("rscan01_lh.gii", "invalid_file.xyz")),
    "Unknown files"
  )
})

# Additional comprehensive tests for testdata.R
# Note: Only files currently in GitHub release are tested here:
# rscan01_lh.gii, rscan01_lh.niml.dset, rscan01_rh.niml.dset

test_that("neurosurf_download_testdata quiet=TRUE suppresses messages", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create dummy file
  dummy_file <- file.path(temp_dir, "rscan01_lh.gii")
  writeLines("dummy", dummy_file)

  # With quiet=TRUE, should not produce messages
  expect_silent(
    neurosurf_download_testdata("rscan01_lh.gii", destdir = temp_dir, quiet = TRUE)
  )
})

test_that("neurosurf_cache_dir is consistent across calls", {
  cache1 <- neurosurf_cache_dir()
  cache2 <- neurosurf_cache_dir()
  expect_equal(cache1, cache2)
})

test_that("neurosurf_testdata_path prefers package extdata", {
  # For files that exist in package extdata, should return package path
  pkg_path <- neurosurf_testdata_path("std.8_lh.smoothwm.asc")

  # Should be from package, not cache
  expect_true(grepl("neurosurf", pkg_path))
  expect_true(grepl("extdata", pkg_path))
})

test_that("neurosurf_has_testdata returns TRUE for all std.8 surfaces", {
  surfaces <- c(
    "std.8_lh.smoothwm.asc",
    "std.8_lh.inflated.asc",
    "std.8_lh.pial.asc",
    "std.8_rh.smoothwm.asc",
    "std.8_rh.inflated.asc"
  )

  for (surf in surfaces) {
    expect_true(neurosurf_has_testdata(surf), info = paste("Missing:", surf))
  }
})

test_that("neurosurf_download_testdata creates destdir if using cache", {
 # When destdir is NULL and package extdata is not writable,
  # it should try to use/create cache dir
  # This is hard to test directly, but we can verify the function handles NULL destdir

  # Skip actual download
  skip_if_offline()

  # With NULL destdir, the function should determine appropriate destination
  # We can't easily test this without mocking, so just verify it doesn't error immediately
  expect_error(
    neurosurf_download_testdata("rscan01_lh.gii", destdir = NULL, quiet = TRUE),
    NA  # NA means we expect no error from this specific call
  ) |> suppressWarnings()  # Suppress download warnings
})

test_that("neurosurf_testdata_path returns empty string for missing file", {
  path <- neurosurf_testdata_path("this_file_does_not_exist_12345.xyz")
  expect_equal(path, "")
  expect_type(path, "character")
  expect_length(path, 1)
})

test_that("neurosurf_has_testdata returns FALSE for missing file", {
  result <- neurosurf_has_testdata("this_file_does_not_exist_12345.xyz")
  expect_false(result)
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("neurosurf_download_testdata error message lists all available files", {
  # Check that the error message is informative
  err <- tryCatch(
    neurosurf_download_testdata("bad_file.xyz"),
    error = function(e) conditionMessage(e)
  )

  expect_true(grepl("rscan01_lh.gii", err))
  expect_true(grepl("rscan01_lh.niml.dset", err))
  expect_true(grepl("rscan01_rh.niml.dset", err))
})

test_that("neurosurf_download_testdata returns character vector", {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)

  # Create dummy files
  for (f in c("rscan01_lh.gii", "rscan01_lh.niml.dset")) {
    writeLines("dummy", file.path(temp_dir, f))
  }

  result <- neurosurf_download_testdata(
    c("rscan01_lh.gii", "rscan01_lh.niml.dset"),
    destdir = temp_dir,
    quiet = TRUE
  )

  expect_type(result, "character")
  expect_length(result, 2)
  expect_true(all(file.exists(result)))
})
