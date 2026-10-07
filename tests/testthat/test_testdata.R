test_that("bundled and missing test data paths are identified", {
  expect_identical(neurosurf_cache_dir(), file.path(
    tools::R_user_dir("neurosurf", which = "cache"), "synthetic-v1"
  ))
  expect_true(neurosurf_has_testdata("std.8_lh.smoothwm.asc"))
  expect_true(file.exists(neurosurf_testdata_path("std.8_lh.smoothwm.asc")))
  expect_false(neurosurf_has_testdata("missing.xyz"))
  expect_identical(neurosurf_testdata_path("missing.xyz"), "")
})

test_that("file names and explicit destinations are validated before writing", {
  local_mocked_bindings(.ns_write_testdata = function(...) stop("writer"))
  expect_error(neurosurf_generate_testdata("invalid.xyz"), "Unknown files")
  expect_error(neurosurf_generate_testdata(c("rscan01_lh.gii", "bad")),
               "Unknown files")
  expect_error(neurosurf_generate_testdata("all", destdir = tempfile()),
               "does not exist")
})

test_that("existing files are reused invisibly without writing", {
  dest <- tempfile()
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE))
  file <- file.path(dest, "rscan01_lh.gii")
  writeLines("existing", file)
  local_mocked_bindings(.ns_write_testdata = function(...) stop("writer"))
  expect_message(neurosurf_generate_testdata("rscan01_lh.gii", dest),
                 "already exists")
  result <- withVisible(neurosurf_download_testdata(
    "rscan01_lh.gii", dest, quiet = TRUE
  ))
  expect_identical(result$value, file)
  expect_false(result$visible)
  expect_identical(readLines(file), "existing")
})

test_that("synthetic files round-trip through GIFTI and NIML readers", {
  dest <- tempfile()
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE))
  expect_silent(paths <- neurosurf_generate_testdata(destdir = dest,
                                                    quiet = TRUE))
  expect_length(paths, 3L)
  expect_true(all(file.info(paths)$size < 10000))
  data <- gifti::readgii(paths[1])$data[[1]]
  expect_equal(dim(data), c(16L, 4L))
  expect_equal(data[1, 1], sin(1 / 3) + cos(1 / 5), tolerance = 1e-7)
  left <- readNIMLSurfaceHeader(paths[2])
  right <- readNIMLSurfaceHeader(paths[3])
  expect_equal(left$nodes, 0:15)
  expect_equal(left$node_count, 16L)
  expect_equal(left$nels, 4L)
  expect_equal(left$data, data, tolerance = 1e-6)
  expect_equal(right$data, -data, tolerance = 1e-6)
  expect_match(paste(readLines(paths[1]), collapse = ""), "Synthetic")
  expect_identical(list.files(dest), sort(basename(paths)))
})

test_that("the compatibility wrapper generates identical files in a cache", {
  cache <- tempfile()
  on.exit(unlink(cache, recursive = TRUE))
  local_mocked_bindings(neurosurf_cache_dir = function() cache)
  old_timeout <- getOption("timeout")
  old_seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  expect_silent(paths <- neurosurf_download_testdata("all", quiet = TRUE))
  expect_true(all(dirname(paths) == file.path(cache, "extdata")))
  expect_true(all(file.exists(paths)))
  expect_identical(getOption("timeout"), old_timeout)
  expect_identical(get0(".Random.seed", envir = .GlobalEnv,
                        inherits = FALSE), old_seed)
  expect_identical(neurosurf_testdata_path("rscan01_lh.gii"), paths[1])
  hashes <- vapply(paths, digest::digest, character(1), file = TRUE)
  neurosurf_generate_testdata("all", overwrite = TRUE, quiet = TRUE)
  expect_identical(vapply(paths, digest::digest, character(1), file = TRUE),
                   hashes)
})

test_that("failed fixture writes preserve existing files and clean staging", {
  dest <- tempfile()
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE))
  file <- file.path(dest, "rscan01_lh.gii")
  writeLines("old", file)
  local_mocked_bindings(.ns_write_testdata = function(path, name, values) {
    writeLines("partial", path)
    stop("writer fixture error")
  })
  expect_error(neurosurf_generate_testdata(
    "rscan01_lh.gii", dest, overwrite = TRUE, quiet = TRUE
  ), "writer fixture error")
  expect_identical(readLines(file), "old")
  expect_identical(list.files(dest, all.files = TRUE, no.. = TRUE),
                   "rscan01_lh.gii")
})
