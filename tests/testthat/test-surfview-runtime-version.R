test_that("the declared surfview version matches the bundled runtime", {
  yaml_path <- system.file("htmlwidgets", "surfwidget.yaml", package = "neurosurf")
  commit_path <- system.file("htmlwidgets", "lib", "neurosurface",
                             "surfview.embed.commit", package = "neurosurf")
  skip_if(!nzchar(yaml_path) || !nzchar(commit_path),
          "Installed widget metadata unavailable.")
  # The version line of the `surfview` dependency entry in surfwidget.yaml.
  spec <- readLines(yaml_path, warn = FALSE)
  at <- grep("^\\s*-\\s*name:\\s*surfview\\s*$", spec)
  expect_length(at, 1L)
  version_line <- grep("^\\s*version:", spec[seq.int(at + 1L, length(spec))],
                       value = TRUE)[[1L]]
  declared <- trimws(sub("^\\s*version:\\s*", "", version_line))
  record <- readLines(commit_path, warn = FALSE)
  shipped <- sub("^version=", "", grep("^version=", record, value = TRUE))
  # Consumers (e.g. neuromosaic provenance) report the declared field as the
  # runtime version, so it must name the bundle actually shipped.
  expect_identical(declared, shipped)
})
