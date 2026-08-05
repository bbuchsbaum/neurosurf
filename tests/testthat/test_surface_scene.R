test_that("SurfaceScene validates unilateral and bilateral scenes", {
  left <- scene_test_geometry("lh")
  right <- scene_test_geometry("rh", 3)
  scene <- surface_scene(
    left = left,
    right = right,
    layers = list(
      surface_layer(
        "task",
        values = list(left = c(NA, 1e-12, 2, 3), right = 4:7),
        colormap = c("#0000ff", "#ff0000"),
        limits = c(0, 7),
        units = "z",
        metadata = list(method = "GLM")
      ),
      surface_layer(
        "atlas",
        values = list(lh = c(1, 3), rh = c(2, 4)),
        indices = list(lh = c(1, 4), rh = c(2, 3)),
        limits = c(1, 4)
      )
    ),
    curvature = list(left = -1:2, right = 2:-1),
    selected_layer = "task",
    metadata = list(subject = "sub-01", missing = NA),
    provenance = list(command = "fit_surface()"),
    fallback = "Two cortical hemispheres showing task and atlas maps.",
    alt_text = "Lateral views of two cortical hemispheres."
  )

  expect_s4_class(scene, "SurfaceScene")
  expect_equal(names(scene@geometries), c("left", "right"))
  expect_equal(names(scene@layers), c("task", "atlas"))
  expect_equal(scene@layers$task$values$left[2], 1e-12)
  expect_true(is.na(scene@layers$task$values$left[1]))
})

test_that("SurfaceScene rejects ambiguous or unsafe input", {
  left <- scene_test_geometry("lh")
  right <- scene_test_geometry("rh")
  base_layer <- surface_layer("signal", 1:4)
  args <- list(left = left, layers = base_layer,
               fallback = "Surface fallback.", alt_text = "Surface alt text.")

  expect_error(do.call(surface_scene, c(args, right = list(left))),
               "declares hemisphere")
  expect_error(surface_layer("bad name", 1:4), "layer name")
  expect_error(surface_layer("signal", 1:4, opacity = 2), "opacity")
  expect_error(surface_scene(left = left,
                             layers = surface_layer("signal", 1:4,
                                                    limits = c(2, 1)),
                             fallback = "Fallback.", alt_text = "Alt."),
               "limits")
  expect_error(surface_scene(left = left, layers = surface_layer("signal", 1:3),
                             fallback = "Fallback.", alt_text = "Alt."),
               "3 values for 4")
  expect_error(surface_scene(left = left,
                             layers = surface_layer("signal", 1:2,
                                                    indices = c(1, 5)),
                             fallback = "Fallback.", alt_text = "Alt."),
               "outside")
  expect_error(surface_scene(left = left,
                             layers = list(base_layer, base_layer),
                             fallback = "Fallback.", alt_text = "Alt."),
               "unique")
  expect_error(surface_scene(left = left, layers = base_layer,
                             fallback = "", alt_text = "Alt."), "fallback")
  expect_error(surface_scene(left = left, layers = base_layer,
                             metadata = list("unnamed"),
                             fallback = "Fallback.", alt_text = "Alt."),
               "uniquely named")
})

test_that("manifest modes carry equivalent content-addressed bytes", {
  left <- scene_test_geometry("lh")
  scene <- surface_scene(
    left = left,
    layers = surface_layer(
      "signal", c(NA, 1e-20, 2, 3), limits = c(0, 3),
      metadata = list(scientific = 1e-20, missing = NA)
    ),
    fallback = "Surface fallback.",
    alt_text = "Surface alt text."
  )
  inline <- surface_scene_manifest(scene, "inline")
  directory <- tempfile("surface-scene-assets-")
  dir.create(directory)
  adjacent <- surface_scene_manifest(scene, "directory", directory)

  expect_identical(inline$schemaVersion, "surfview.scene.v1")
  expect_identical(names(inline$assets), names(adjacent$assets))
  expect_equal(inline$layers$signal$metadata$scientific, 1e-20)
  expect_true(is.na(inline$layers$signal$metadata$missing))

  for (asset_id in names(inline$assets)) {
    inline_asset <- inline$assets[[asset_id]]
    adjacent_asset <- adjacent$assets[[asset_id]]
    inline_bytes <- base64enc::base64decode(inline_asset$data)
    adjacent_bytes <- readBin(file.path(directory, adjacent_asset$uri), "raw",
                              n = adjacent_asset$byteLength)
    expect_identical(inline_bytes, adjacent_bytes)
    expect_identical(
      digest::digest(inline_bytes, algo = "sha256", serialize = FALSE),
      inline_asset$sha256
    )
  }

  value_asset <- Filter(function(x) identical(x$role, "values"), inline$assets)[[1]]
  decoded <- readBin(base64enc::base64decode(value_asset$data), "numeric",
                     n = 4, size = 4, endian = "little")
  expect_true(is.na(decoded[1]))
  expect_equal(decoded[2], 1e-20, tolerance = 1e-26)
})

test_that("standalone reports support inline and adjacent assets", {
  scene <- surface_scene(
    left = scene_test_geometry("lh"),
    layers = surface_layer("signal", 1:4),
    fallback = "Static surface description.",
    alt_text = "Interactive test surface."
  )
  inline_dir <- tempfile("surface-scene-inline-")
  adjacent_dir <- tempfile("surface-scene-adjacent-")
  inline <- write_surface_scene(scene, inline_dir, self_contained = TRUE)
  adjacent <- write_surface_scene(scene, adjacent_dir, self_contained = FALSE)

  expect_true(file.exists(inline))
  expect_true(file.exists(adjacent))
  expect_match(paste(readLines(inline), collapse = "\n"),
               "Static surface description.")
  expect_true(file.exists(file.path(adjacent_dir, "surfview.embed.iife.js")))
  expect_true(length(list.files(adjacent_dir, pattern = "\\.bin$")) >= 3)
})
