test_that("anatomy contrast is independent of positive affine metric scaling", {
  x <- c(-5, -2, -1, 0, 1, 2, 5)
  for (style in c("continuous", "binary")) {
    a <- normalize_surface_anatomy(x, style)
    expect_equal(a, normalize_surface_anatomy(100 + 7*x, style))
    expect_equal(normalize_surface_anatomy(x, style, invert = TRUE), -a)
    expect_equal(a[4], 0)
    expect_equal(range(a), c(-0.5, 0.5))
  }
  expect_equal(normalize_surface_anatomy(c(-1, 0, 1), "binary", 0), c(-.5, 0, .5))
  expect_equal(normalize_surface_anatomy(rep(1, 4), "binary", 0), rep(0, 4))
  expect_error(normalize_surface_anatomy(c(1, NA)), "finite")
  expect_error(normalize_surface_anatomy(1:4, midpoint = Inf), "midpoint")
  expect_error(normalize_surface_anatomy(1:4, invert = NA), "invert")
})

test_that("anatomy contrast cannot alter raster scalar values or coverage", {
  geometry <- SurfaceGeometry(matrix(c(0,0,0, 0,2,0, 0,0,2), 3, 3, byrow=TRUE),
                              matrix(c(0L,1L,2L), 1, 3), hemi="left")
  render <- function(style, gray) render_surface_rgba(
    geometry, c(0, 2, 4), anatomy_metric=c(-1, 0, 1),
    width=40, height=40, antialias=1, outer_contour=FALSE,
    threshold=1, return_buffers=TRUE, anatomy_style=style,
    anatomy_range=gray)
  old <- render("publication", c(.72,.90))
  new <- render("binary", c(.25,.75))
  expect_equal(new$coverage, old$coverage)
  expect_equal(new$scalar, old$scalar)
  expect_equal(new$overlay_alpha, old$overlay_alpha)
  expect_false(identical(new$rgba, old$rgba))
})

test_that("derived anatomy smoothing preserves geometry and constants", {
  g <- load_fsaverage_std8("white")$lh
  before <- coords(g)
  raw <- anatomical_curvature(g, 0)
  smooth <- anatomical_curvature(g)
  expect_equal(coords(g), before)
  expect_length(smooth, length(raw))
  expect_true(all(is.finite(smooth)))
  f <- faces(g)
  variation <- function(v) mean(abs(v[f[,1]] - v[f[,2]]))
  expect_lt(variation(smooth), variation(raw))
  expect_error(anatomical_curvature(g, -1), "iterations")
  expect_length(surface_heat_colors(), 256)
  expect_length(surface_heat_colors(FALSE), 128)
})
