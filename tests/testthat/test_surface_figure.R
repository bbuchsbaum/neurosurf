figure_test_geometries <- function() {
  list(
    lh = read_surf_geometry(system.file(
      "extdata", "std.8_lh.smoothwm.asc", package = "neurosurf"
    )),
    rh = read_surf_geometry(system.file(
      "extdata", "std.8_rh.smoothwm.asc", package = "neurosurf"
    ))
  )
}

figure_test_values <- function(geoms) {
  lapply(geoms, function(g) as.numeric(scale(coords(g)[, 3])))
}

test_that("surface_figure composes hemisphere-by-view panels", {
  geoms <- figure_test_geometries()
  values <- figure_test_values(geoms)

  fig <- surface_figure(
    lh = geoms$lh, rh = geoms$rh,
    values = values,
    threshold = 0.5, limits = c(-2, 2),
    legend_title = "z",
    panel_width = 120, panel_height = 80, antialias = 1L
  )

  expect_s3_class(fig, "surface_figure")
  expect_named(fig$panels,
               c("lh_lateral", "rh_lateral", "lh_medial", "rh_medial"))
  expect_identical(c(fig$nrow, fig$ncol), c(2L, 2L))
  for (panel in fig$panels) {
    expect_s3_class(panel, "surface_rgba")
    expect_identical(panel$provenance$limits, c(-2, 2))
    expect_identical(panel$provenance$threshold, 0.5)
    expect_true(any(panel$coverage))
  }
  expect_identical(fig$panels$lh_lateral$camera$view, "lateral")
  expect_identical(fig$panels$rh_medial$camera$view, "medial")
})

test_that("surface_figure accepts left/right names and bare vectors", {
  geoms <- figure_test_geometries()
  values <- figure_test_values(geoms)

  bilateral <- surface_figure(
    lh = geoms$lh, rh = geoms$rh,
    values = list(left = values$lh, right = values$rh),
    views = "lateral",
    panel_width = 100, panel_height = 60, antialias = 1L
  )
  expect_named(bilateral$panels, c("lh_lateral", "rh_lateral"))

  single <- surface_figure(
    lh = geoms$lh,
    values = values$lh,
    views = c("lateral", "medial"),
    panel_width = 100, panel_height = 60, antialias = 1L
  )
  expect_identical(c(single$nrow, single$ncol), c(2L, 1L))

  # default limits come from the finite value range
  expect_equal(single$scale$limits, range(values$lh))
})

test_that("surface_figure validates its inputs", {
  geoms <- figure_test_geometries()
  values <- figure_test_values(geoms)

  expect_error(surface_figure(values = values), "at least one")
  expect_error(
    surface_figure(lh = geoms$lh, rh = geoms$rh, values = values$lh),
    "named list"
  )
  expect_error(
    surface_figure(lh = geoms$lh, rh = geoms$rh,
                   values = list(lh = values$lh)),
    "missing element"
  )
  expect_error(
    surface_figure(lh = geoms$lh, values = list(lh = values$lh[-1])),
    "vertices"
  )
  expect_error(
    surface_figure(lh = geoms$lh,
                   values = rep(NA_real_, length(values$lh))),
    "finite"
  )
})

test_that("surface_figure draws and writes PNG output", {
  geoms <- figure_test_geometries()
  values <- figure_test_values(geoms)

  fig <- surface_figure(
    lh = geoms$lh, rh = geoms$rh,
    values = values,
    threshold = 0.5, limits = c(-2, 2),
    legend_title = "z",
    panel_width = 120, panel_height = 80, antialias = 1L
  )

  out <- write_surface_figure(fig, tempfile(fileext = ".png"))
  expect_true(file.exists(out))
  img <- png::readPNG(out)
  expect_identical(dim(img)[2], 2L * 120L)
  expect_gt(dim(img)[1], 2L * 80L) # legend strip adds height

  no_legend <- surface_figure(
    lh = geoms$lh, rh = geoms$rh,
    values = values, legend = FALSE,
    views = "lateral",
    panel_width = 120, panel_height = 80, antialias = 1L
  )
  out2 <- write_surface_figure(no_legend, tempfile(fileext = ".png"))
  expect_identical(dim(png::readPNG(out2))[1:2], c(80L, 240L))

  # plot()/print() draw on the active device without error
  device_file <- tempfile(fileext = ".png")
  grDevices::png(device_file, width = 300, height = 240)
  expect_invisible(plot(fig))
  expect_invisible(print(no_legend))
  grDevices::dev.off()
  expect_true(file.exists(device_file))

  expect_error(write_surface_figure(list(), tempfile()), "surface_figure")
  expect_error(write_surface_figure(fig, tempfile(), scale = 0), "positive")
})
