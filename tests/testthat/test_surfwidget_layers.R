test_that("surfwidget keeps root overlay settings separate from explicit layers", {
  verts <- matrix(
    c(
      0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      0, 0, 1
    ),
    ncol = 3,
    byrow = TRUE
  )
  faces <- matrix(
    c(
      0L, 1L, 2L,
      0L, 1L, 3L,
      0L, 2L, 3L,
      1L, 2L, 3L
    ),
    ncol = 3,
    byrow = TRUE
  )
  geom <- SurfaceGeometry(verts, faces, hemi = "lh")
  n_verts <- nrow(coords(geom))

  surface <- ColorMappedNeuroSurface(
    geom,
    seq_len(n_verts),
    c(-1.5, -0.25, 0.4, 1.8),
    colorRampPalette(c("blue", "white", "red"))(8),
    c(-2, 2),
    c(-1, 1)
  )

  widget <- surfwidget(
    surface,
    layers = list(
      list(
        label = "curvature",
        data = rep(0.5, n_verts),
        cmap = c("#707070", "#e0e0e0"),
        color_range = c(0, 1),
        alpha = 1
      ),
      list(label = "data")
    )
  )

  expect_equal(widget$x$thresh, c(-1, 1))
  expect_equal(widget$x$irange, c(-2, 2))

  expect_equal(widget$x$layers[[1]]$id, "curvature")
  expect_equal(widget$x$layers[[1]]$data, rep(0.5, n_verts))
  expect_equal(widget$x$layers[[1]]$cmap, c("#707070", "#e0e0e0"))
  expect_equal(widget$x$layers[[1]]$color_range, c(0, 1))
  expect_null(widget$x$layers[[1]]$thresh)
  expect_null(widget$x$layers[[1]]$indices)

  expect_equal(widget$x$layers[[2]]$id, "data")
  expect_null(widget$x$layers[[2]]$data)
  expect_null(widget$x$layers[[2]]$cmap)
  expect_null(widget$x$layers[[2]]$color_range)
  expect_null(widget$x$layers[[2]]$thresh)
  expect_null(widget$x$layers[[2]]$indices)
})
