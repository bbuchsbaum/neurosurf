test_that(".ns_norm_panel handles objects, paths, and per-panel arg lists", {
  geom <- example_surface_geometry()

  # Bare S4 object -> obj with shared args only
  p1 <- neurosurf:::.ns_norm_panel(geom, shared = list(cmap = "x"))
  expect_true(methods::is(p1$obj, "SurfaceGeometry"))
  expect_equal(p1$args$cmap, "x")

  # list(obj, overrides) -> per-panel args override shared
  p2 <- neurosurf:::.ns_norm_panel(
    list(geom, thresh = c(-1, 1), cmap = "y"),
    shared = list(cmap = "x", irange = c(-2, 2))
  )
  expect_true(methods::is(p2$obj, "SurfaceGeometry"))
  expect_equal(p2$args$cmap, "y")            # overridden
  expect_equal(p2$args$irange, c(-2, 2))     # inherited
  expect_equal(p2$args$thresh, c(-1, 1))     # panel-specific

  # A character path is accepted as the object (read lazily at draw time)
  p3 <- neurosurf:::.ns_norm_panel("lh.pial", shared = list())
  expect_identical(p3$obj, "lh.pial")

  # A list with no unnamed element is an error
  expect_error(
    neurosurf:::.ns_norm_panel(list(thresh = c(-1, 1)), shared = list()),
    "unnamed"
  )
})

test_that(".ns_to_rgb normalises grey, RGB, and RGBA arrays to RGB", {
  grey <- matrix(0.5, nrow = 4, ncol = 6)
  rgb_arr <- neurosurf:::.ns_to_rgb(grey)
  expect_equal(dim(rgb_arr), c(4L, 6L, 3L))
  expect_true(all(rgb_arr == 0.5))

  rgb3 <- array(0.3, dim = c(4, 6, 3))
  expect_equal(dim(neurosurf:::.ns_to_rgb(rgb3)), c(4L, 6L, 3L))

  # RGBA with alpha = 0 composites to white
  rgba <- array(0, dim = c(2, 2, 4))   # colour 0, alpha 0
  out <- neurosurf:::.ns_to_rgb(rgba)
  expect_equal(dim(out), c(2L, 2L, 3L))
  expect_true(all(out == 1))           # fully transparent over white
})

test_that(".ns_compose_grid tiles panels with consistent cell sizes", {
  a <- array(0.2, dim = c(10, 20, 3))
  b <- array(0.8, dim = c(8, 16, 3))   # smaller -> padded/centred on white
  canvas <- neurosurf:::.ns_compose_grid(list(a, b), nrow = 1L, ncol = 2L)

  # cell size is the max of each dimension; one row, two columns
  expect_equal(dim(canvas), c(10L, 40L, 3L))

  # second cell is centred on white, so its corners are white (== 1)
  expect_equal(canvas[1, 21, 1], 1)
})

test_that("surface_montage errors on an empty or non-list panels argument", {
  expect_error(surface_montage(list()), "non-empty")
  expect_error(surface_montage("not a list"), "non-empty")
})

test_that("surface_montage composes a static montage when rgl can render", {
  skip_on_cran()
  skip_if_not_installed("rgl")
  skip_if(rgl::rgl.useNULL() && !requireNamespace("webshot2", quietly = TRUE),
          "No static rgl snapshot backend available")
  # Some headless CI machines cannot open any rgl device at all.
  can_open <- tryCatch({ rgl::open3d(); rgl::close3d(); TRUE },
                       error = function(e) FALSE)
  skip_if_not(can_open, "Cannot open an rgl device in this session")

  geom <- example_surface_geometry()
  set.seed(1)
  vals <- rnorm(nrow(coords(geom)))
  ns <- NeuroSurface(geom, indices = seq_along(vals), data = vals)

  out <- tempfile(fileext = ".png")
  res <- surface_montage(
    list(ns, list(ns, thresh = c(-0.5, 0.5))),
    cmap = grDevices::rainbow(50), irange = c(-2, 2),
    ncol = 2, file = out, width = 240, height = 180
  )

  expect_identical(res, out)
  expect_true(file.exists(out))
  img <- png::readPNG(out)
  expect_equal(length(dim(img)), 3L)
  expect_equal(dim(img)[2], 480L)   # two 240-wide panels in one row
})
