test_that("load_fsaverage_std8 returns correct structure", {
  # Load the default smoothwm surface
  fs <- load_fsaverage_std8()

  expect_type(fs, "list")
  expect_named(fs, c("lh", "rh"))
  expect_s4_class(fs$lh, "SurfaceGeometry")
  expect_s4_class(fs$rh, "SurfaceGeometry")
})

test_that("load_fsaverage_std8 loads all surface types", {
  surf_types <- c("smoothwm", "pial", "inflated", "white", "sphere")

  for (surf in surf_types) {
    fs <- load_fsaverage_std8(surf)
    expect_type(fs, "list")
    expect_named(fs, c("lh", "rh"))
    expect_s4_class(fs$lh, "SurfaceGeometry")
    expect_s4_class(fs$rh, "SurfaceGeometry")
  }
})

test_that("load_fsaverage_std8 errors on invalid surface type", {
  expect_error(load_fsaverage_std8("invalid_surface"))
})

test_that("load_fsaverage works as a wrapper for std.8", {
  # Default call

fs <- load_fsaverage()

  expect_type(fs, "list")
  expect_named(fs, c("lh", "rh"))
  expect_s4_class(fs$lh, "SurfaceGeometry")
  expect_s4_class(fs$rh, "SurfaceGeometry")
})

test_that("load_fsaverage accepts density and surf arguments", {
  fs <- load_fsaverage(density = "std.8", surf = "inflated")

  expect_type(fs, "list")
  expect_s4_class(fs$lh, "SurfaceGeometry")
  expect_s4_class(fs$rh, "SurfaceGeometry")
})

test_that("load_fsaverage errors on unsupported density", {
  expect_error(load_fsaverage(density = "fsaverage6"))
})

test_that("load_fsaverage_bundle returns SurfaceSet objects", {
  bundle <- load_fsaverage_bundle()

  expect_type(bundle, "list")
  expect_named(bundle, c("lh", "rh"))
  expect_s4_class(bundle$lh, "SurfaceSet")
  expect_s4_class(bundle$rh, "SurfaceSet")
})

test_that("load_fsaverage_bundle includes all requested surfaces", {
  surfs <- c("smoothwm", "pial", "inflated")
  bundle <- load_fsaverage_bundle(surfs = surfs)

  expect_type(bundle, "list")
  expect_s4_class(bundle$lh, "SurfaceSet")
  expect_s4_class(bundle$rh, "SurfaceSet")

  # Check that we can get surfaces by label
  lh_labels <- surface_labels(bundle$lh)
  expect_true("smoothwm" %in% lh_labels)
  expect_true("pial" %in% lh_labels)
  expect_true("inflated" %in% lh_labels)
})

test_that("load_fsaverage_bundle with single surface", {
  bundle <- load_fsaverage_bundle(surfs = "pial", default_label = "pial")

  expect_type(bundle, "list")
  expect_s4_class(bundle$lh, "SurfaceSet")
  expect_s4_class(bundle$rh, "SurfaceSet")
})

test_that("load_fsaverage_bundle errors on empty surfs", {
  expect_error(load_fsaverage_bundle(surfs = character(0)))
})

test_that("load_fsaverage_bundle errors on invalid default_label", {
  expect_error(load_fsaverage_bundle(surfs = c("smoothwm", "pial"),
                                     default_label = "inflated"))
})

test_that("loaded fsaverage surfaces have expected properties", {
  fs <- load_fsaverage_std8("smoothwm")

  # Check that vertices and faces are non-empty
  expect_gt(nrow(vertices(fs$lh)), 0)
  expect_gt(nrow(faces(fs$lh)), 0)
  expect_gt(nrow(vertices(fs$rh)), 0)
  expect_gt(nrow(faces(fs$rh)), 0)

  # LH and RH should have the same number of vertices (std.8 decimation)
  expect_equal(nrow(vertices(fs$lh)), nrow(vertices(fs$rh)))
})
