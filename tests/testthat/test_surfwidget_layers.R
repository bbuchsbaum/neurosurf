test_that("legacy surfwidget calls adapt to SurfaceScene", {
  geom <- scene_test_geometry("lh")
  surface <- ColorMappedNeuroSurface(
    geom, 1:4, c(-1.5, -0.25, 0.4, 1.8),
    grDevices::colorRampPalette(c("blue", "white", "red"))(8),
    c(-2, 2), c(-1, 1)
  )
  widget <- surfwidget(
    surface,
    layers = list(list(
      label = "curvature",
      data = rep(0.5, 4),
      cmap = c("#707070", "#e0e0e0"),
      color_range = c(0, 1)
    ))
  )

  expect_s3_class(widget, "htmlwidget")
  expect_identical(widget$x$scene$schemaVersion, "surfview.scene.v1")
  expect_equal(names(widget$x$scene$layers), c("data", "curvature"))
  expect_equal(widget$x$scene$layers$data$threshold, c(-1, 1))
  expect_equal(widget$x$scene$layers$data$limits, c(-2, 2))
  expect_equal(widget$x$scene$layers$curvature$colorMap,
               c("#707070", "#e0e0e0"))
  expect_true(widget$x$options$controls)
  expect_identical(widget$x$options$preset, "paper-light")
  expect_match(widget$x$fallback, "left-hemisphere")
})

test_that("legacy empty layers fail instead of producing a blank map", {
  geom <- scene_test_geometry("lh")
  surface <- ColorMappedNeuroSurface(
    geom, 1:4, 1:4, c("#440154", "#fde725"), c(1, 4), c(0, 0)
  )
  expect_error(surfwidget(surface, layers = list(list(label = "empty"))),
               "must provide 'data'")
})

test_that("Tweakpane-era config is explicitly deprecated", {
  scene <- surface_scene(
    left = scene_test_geometry("lh"),
    layers = surface_layer("signal", 1:4),
    fallback = "Fallback.", alt_text = "Alt text."
  )
  expect_warning(surfwidget(scene, config = list(showControls = TRUE)),
                 "deprecated")
  expect_warning(surfwidget(scene, config = list(controlType = "pane")),
                 "Tweakpane")
})
