test_that("surface_palette returns anchors, ramps and the renderer's LUT", {
  expect_length(surface_palette("two_sided", n = NULL), 6L)
  ramp <- surface_palette("two_sided", n = 9)
  expect_length(ramp, 9L)
  expect_match(ramp, "^#[0-9A-F]{6}$")
  lut <- surface_palette("two_sided", n = 256, limits = c(-5, 5),
                         threshold = 2)
  ref <- neurosurf:::.ns_overlay_lut(surface_palette(n = NULL), c(-5, 5), 2,
                                     "two_sided", 256L)
  expect_identical(lut, grDevices::rgb(ref[, 1], ref[, 2], ref[, 3]))
  # Each tail starts at its lightest colour just past the threshold.
  v <- seq(-5, 5, length.out = 256)
  first <- grDevices::col2rgb(lut[which(v > 2)[1]])
  expect_lt(max(abs(first - grDevices::col2rgb(surface_palette(n = NULL)[4]))),
            4)
  expect_error(surface_palette(limits = c(1, 1)), "increasing")
  expect_error(surface_palette(n = 1), "at least 2")
})

test_that("automatic palette follows the sign of the colour range", {
  auto <- neurosurf:::.ns_auto_palette
  expect_identical(auto(NULL, c(-1, 1)), surface_palette("two_sided"))
  expect_identical(auto(NULL, c(0, 1)), surface_palette("positive"))
  expect_identical(auto(NULL, c(-3, -1)), surface_palette("negative"))
})

test_that("a masked medial wall occludes instead of revealing far faces", {
  # A near masked triangle covers a far cortex triangle with overlay.
  projected <- rbind(c(0, 0, 0), c(12, 0, 0), c(0, 12, 0),
                     c(0, 0, 1), c(12, 0, 1), c(0, 12, 1))
  faces <- rbind(c(1L, 2L, 3L), c(4L, 5L, 6L))
  values <- c(8, 8, 8, 0, 0, 0)
  mask <- c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  render <- function(policy) {
    neurosurf:::cpp_rasterize_surface_scalar(
      projected, faces, values, rep(0.5, 6), mask, 12L, 12L, 1, 0L,
      c(-10, 10), cbind(seq(0, 1, length.out = 16), 0.2,
                        seq(1, 0, length.out = 16), 1),
      1, 0, 0.75, 0.9, policy, c(1, 1, 1), 1L, TRUE
    )
  }
  masked <- render(1L)
  expect_equal(sum(as.integer(masked$overlay_alpha)), 0)
  expect_false(any(masked$coverage))
  expect_true(all(as.integer(masked$rgba[, , 1:3]) == 255L))
  shaded <- render(0L)
  expect_true(any(shaded$coverage))
})

test_that("SurfaceGeometry show() reports true area with units", {
  # A unit right triangle and its mirror form a 1 x 1 square: area 1.
  vb <- rbind(c(0, 1, 0, 1), c(0, 0, 1, 1), 0)
  geom <- SurfaceGeometry(t(vb), rbind(c(0L, 1L, 2L), c(1L, 3L, 2L)), "lh")
  out <- paste(capture.output(show(geom)), collapse = "\n")
  expect_match(out, "Surface Area:\\s+1 mm\\^2")
  expect_false(grepl("/____\\\\", out))
})

test_that("view_surface shades anatomy and picks a palette by default", {
  skip_if_not_installed("rgl")
  withr::local_options(rgl.useNULL = TRUE)
  geom <- example_surface_geometry()
  dev <- rgl::open3d(useNULL = TRUE)
  on.exit(rgl::close3d(dev), add = TRUE)
  expect_silent(suppressWarnings(
    view_surface(geom, new_window = FALSE)
  ))
  vals <- seq(-1, 1, length.out = nrow(coords(geom)))
  expect_no_error(suppressWarnings(
    view_surface(geom, vals = vals, new_window = FALSE)
  ))
})

test_that("view_surface fades colour in just past the threshold", {
  blend <- neurosurf:::.ns_fade_blend
  fg <- rep("#FF0000", 5)
  bg <- rep("#000000", 5)
  out <- blend(fg, bg, c(0, 1, 1.125, 1.25, NA), thresh = c(-1, 1))
  red <- grDevices::col2rgb(out)[1, ]
  expect_equal(red[1:2], c(0, 0))           # inside the band: base colour
  expect_equal(red[3], 128, tolerance = 1)  # halfway through the fade
  expect_equal(red[4], 255)                 # fully coloured
  expect_equal(red[5], 0)                   # missing values stay base
})

test_that("surface_colorbar_grob builds a key from a panel or a scale", {
  geom <- example_surface_geometry()
  vals <- seq(-4, 4, length.out = nrow(coords(geom)))
  panel <- render_surface_rgba(geom, vals, threshold = 1.5,
                               limits = c(-4, 4), alpha_ramp = 0.25,
                               width = 40, height = 40, antialias = 1)
  expect_identical(panel$provenance$alpha_ramp, 0.25)
  expect_length(panel$provenance$palette, 6L)
  key <- surface_colorbar_grob(panel, title = "z")
  expect_s3_class(key, "grob")
  labels <- unlist(lapply(key$children, function(g) g$label))
  expect_true(all(c("z", "1.5", "4") %in% labels))
  explicit <- surface_colorbar_grob(limits = c(0, 3), tail = "positive")
  expect_s3_class(explicit, "grob")
  expect_error(surface_colorbar_grob(limits = c(2, 1)), "increasing")
  expect_error(surface_colorbar_grob("x"), "surface_rgba")
})
