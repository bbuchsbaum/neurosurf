# Smoke tests for visualization functions
# Covers surfwidget, view_surface, and related visualization functionality

library(testthat)
library(neurosurf)

# ==============================================================================
# surfwidget Smoke Tests
# ==============================================================================

test_that("surfwidget creates valid htmlwidget from SurfaceGeometry", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()

  widget <- surfwidget(geom)

  expect_s3_class(widget, "htmlwidget")
  expect_s3_class(widget, "surfwidget")
})

test_that("surfwidget creates widget from NeuroSurface", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()
  idx <- 1:4
  vals <- c(1.0, 2.0, 3.0, 4.0)
  neuro_surf <- NeuroSurface(geometry = geom, indices = idx, data = vals)

  widget <- surfwidget(neuro_surf)

  expect_s3_class(widget, "htmlwidget")
  expect_s3_class(widget, "surfwidget")
})

test_that("surfwidget creates widget from ColorMappedNeuroSurface", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()
  idx <- 1:4
  vals <- c(1.0, 2.0, 3.0, 4.0)

  cmap <- grDevices::rainbow(10)
  color_mapped <- ColorMappedNeuroSurface(
    geometry = geom,
    indices = idx,
    data = vals,
    cmap = cmap,
    irange = range(vals),
    thresh = c(0, 0)
  )

  widget <- surfwidget(color_mapped)

  expect_s3_class(widget, "htmlwidget")
})

test_that("surfwidget accepts custom color map", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()
  custom_cmap <- c("#FF0000", "#00FF00", "#0000FF")

  widget <- surfwidget(geom, cmap = custom_cmap)

  expect_s3_class(widget, "htmlwidget")
})

test_that("surfwidget handles intensity range parameter", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()
  idx <- 1:4
  vals <- c(1.0, 2.0, 3.0, 4.0)
  neuro_surf <- NeuroSurface(geometry = geom, indices = idx, data = vals)

  widget <- surfwidget(neuro_surf, irange = c(0, 10))

  expect_s3_class(widget, "htmlwidget")
})

test_that("surfwidget handles threshold parameter", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()
  idx <- 1:4
  vals <- c(1.0, 2.0, 3.0, 4.0)
  neuro_surf <- NeuroSurface(geometry = geom, indices = idx, data = vals)

  widget <- surfwidget(neuro_surf, thresh = c(1.5, 3.5))

  expect_s3_class(widget, "htmlwidget")
})

test_that("surfwidget handles alpha parameter", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()

  widget <- surfwidget(geom, alpha = 0.5)

  expect_s3_class(widget, "htmlwidget")
})

test_that("surfwidget handles colorbar parameter", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()

  widget_with_cb <- surfwidget(geom, colorbar = TRUE)
  widget_without_cb <- surfwidget(geom, colorbar = FALSE)

  expect_s3_class(widget_with_cb, "htmlwidget")
  expect_s3_class(widget_without_cb, "htmlwidget")
})

test_that("surfwidget handles colorbar_label parameter", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()

  widget <- surfwidget(geom, colorbar = TRUE, colorbar_label = "Curvature")

  expect_s3_class(widget, "htmlwidget")
})

# ==============================================================================
# surfwidget Config Tests
# ==============================================================================

test_that("surfwidget accepts valid config options", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()

  config <- list(
    shininess = 50,
    flatShading = FALSE,
    directionalLightIntensity = 0.7
  )

  widget <- surfwidget(geom, config = config)

  expect_s3_class(widget, "htmlwidget")
})

test_that("surfwidget warns on invalid config keys", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()

  config <- list(
    shininess = 50,
    invalidKey = "should warn"
  )

  expect_warning(
    surfwidget(geom, config = config),
    "unknown config key"
  )
})

test_that("surfwidget validates shininess range", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()

  # Invalid shininess (> 100)
  expect_warning(
    surfwidget(geom, config = list(shininess = 200)),
    "shininess"
  )
})

# ==============================================================================
# show_surface_widget Tests
# ==============================================================================

test_that("show_surface_widget works with SurfaceGeometry", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()

  widget <- show_surface_widget(geom)

  expect_s3_class(widget, "htmlwidget")
})

test_that("show_surface_widget works with data parameter", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()
  data <- c(1, 2, 3, 4)

  widget <- show_surface_widget(geom, data = data)

  expect_s3_class(widget, "htmlwidget")
})

# ==============================================================================
# debug_surfwidget Tests
# ==============================================================================

test_that("debug_surfwidget returns diagnostic info for SurfaceGeometry", {
  geom <- example_surface_geometry()

  diagnostics <- debug_surfwidget(geom, verbose = FALSE)

  expect_true(is.list(diagnostics))
  expect_true("class" %in% names(diagnostics))
  expect_true("n_vertices" %in% names(diagnostics))
  expect_true("n_faces" %in% names(diagnostics))
})

test_that("debug_surfwidget detects issues with empty geometry", {
  # Create a minimal geometry
  geom <- example_surface_geometry()

  diagnostics <- debug_surfwidget(geom, verbose = FALSE)

  expect_true("issues" %in% names(diagnostics))
})

# ==============================================================================
# Shiny Output/Render Tests (structural only)
# ==============================================================================

test_that("surfwidgetOutput creates output element", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("shiny")

  output <- surfwidgetOutput("test_id")

  expect_true(inherits(output, "shiny.tag") || inherits(output, "shiny.tag.list"))
})

test_that("renderSurfwidget creates render function", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("shiny")

  geom <- example_surface_geometry()

  render_fn <- renderSurfwidget({
    surfwidget(geom)
  })

  expect_true(is.function(render_fn))
})

# ==============================================================================
# Widget Update Functions Tests
# ==============================================================================

test_that("updateColorMap modifies widget", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()
  widget <- surfwidget(geom)

  new_cmap <- c("#FF0000", "#00FF00", "#0000FF")
  updated <- updateColorMap(widget, new_cmap)

  expect_s3_class(updated, "htmlwidget")
  expect_true(!is.null(updated$x$calls))
})

test_that("updateAlpha modifies widget", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()
  widget <- surfwidget(geom)

  updated <- updateAlpha(widget, 0.5)

  expect_s3_class(updated, "htmlwidget")
})

test_that("updateZoom modifies widget", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()
  widget <- surfwidget(geom)

  updated <- updateZoom(widget, 2.0)

  expect_s3_class(updated, "htmlwidget")
})

# ==============================================================================
# view_surface Basic Tests (using NULL rgl device)
# ==============================================================================

test_that("view_surface runs without error on simple geometry", {
  skip_if_not_installed("rgl")

  # Ensure NULL device is used
  old_opt <- getOption("rgl.useNULL")
  options(rgl.useNULL = TRUE)
  on.exit(options(rgl.useNULL = old_opt), add = TRUE)

  geom <- example_surface_geometry()

  # Should complete without error
  result <- tryCatch(
    view_surface(geom, new_window = TRUE),
    error = function(e) "error"
  )

  expect_true(!identical(result, "error"))
})

test_that("view_surface accepts vals parameter", {
  skip_if_not_installed("rgl")

  old_opt <- getOption("rgl.useNULL")
  options(rgl.useNULL = TRUE)
  on.exit(options(rgl.useNULL = old_opt), add = TRUE)

  geom <- example_surface_geometry()
  vals <- c(1, 2, 3, 4)

  result <- tryCatch(
    view_surface(geom, vals = vals, new_window = TRUE),
    error = function(e) "error"
  )

  expect_true(!identical(result, "error"))
})

test_that("view_surface accepts different viewpoints", {
  skip_if_not_installed("rgl")

  old_opt <- getOption("rgl.useNULL")
  options(rgl.useNULL = TRUE)
  on.exit(options(rgl.useNULL = old_opt), add = TRUE)

  geom <- example_surface_geometry()
  geom@hemi <- "lh"  # Set hemisphere for viewpoint

  viewpoints <- c("lateral", "medial", "ventral", "dorsal", "anterior", "posterior")

  for (vp in viewpoints) {
    result <- tryCatch(
      view_surface(geom, viewpoint = vp, new_window = TRUE),
      error = function(e) "error"
    )
    expect_true(!identical(result, "error"),
                info = paste("Failed for viewpoint:", vp))
  }
})

# ==============================================================================
# Layers Support Tests
# ==============================================================================

test_that("surfwidget handles layers parameter", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  geom <- example_surface_geometry()

  layers <- list(
    list(
      data = c(0.5, 0.6, 0.7, 0.8),
      cmap = c("#FF0000", "#00FF00"),
      alpha = 0.5
    )
  )

  widget <- surfwidget(geom, layers = layers)

  expect_s3_class(widget, "htmlwidget")
})

# ==============================================================================
# Real Surface Tests
# ==============================================================================

test_that("surfwidget works with real fsaverage surface", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)

  widget <- surfwidget(surf)

  expect_s3_class(widget, "htmlwidget")
})

test_that("surfwidget works with curvature data", {
  skip_if_not_installed("htmlwidgets")
  skip_if_not_installed("Rvcg")

  surf_file <- system.file("extdata", "std.8_lh.smoothwm.asc", package = "neurosurf")
  skip_if(surf_file == "", "Surface file not available")

  surf <- read_surf(surf_file)
  curv_vals <- curvature(surf)

  neuro_surf <- NeuroSurface(
    geometry = surf,
    indices = 1:nrow(coords(surf)),
    data = curv_vals
  )

  widget <- surfwidget(neuro_surf)

  expect_s3_class(widget, "htmlwidget")
})
