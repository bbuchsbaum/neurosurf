test_that("sparse scalar data fills and preserves seeds", {
  options(rgl.useNULL = TRUE)
  verts <- matrix(c(0, 0, 0,
                    2, 0, 0,
                    2, 2, 0,
                    0, 2, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0, 1, 2,
                    0, 2, 3), ncol = 3, byrow = TRUE)
  surf <- SurfaceGeometry(verts, faces, hemi = "lh")

  values <- c(10, 20)
  verts_idx <- c(1, 2)

  filled <- neurosurf:::.ns_fill_sparse_data(
    surf = surf,
    values = values,
    vertices = verts_idx,
    smoothing = "nearest",
    smoothing_steps = 0
  )

  expect_length(filled, nrow(coords(surf)))
  expect_false(any(is.na(filled)))
  expect_equal(filled[1], 10)
  expect_equal(filled[2], 20)
  expect_equal(filled[3], 20)
  expect_equal(filled[4], 10)

  filled_smooth <- neurosurf:::.ns_fill_sparse_data(
    surf = surf,
    values = values,
    vertices = verts_idx,
    smoothing = "auto",
    smoothing_steps = 1
  )
  expect_equal(filled_smooth[verts_idx], values)
})

test_that("vector layers attach cleanly", {
  options(rgl.useNULL = TRUE)
  verts <- matrix(c(0, 0, 0,
                    1, 0, 0,
                    1, 1, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0, 1, 2), ncol = 3, byrow = TRUE)
  surf <- SurfaceGeometry(verts, faces, hemi = "lh")

  p <- surface_plot(lh = surf, views = "lateral")
  vecs <- matrix(c(1, 0, 0,
                   0, 1, 0,
                   0, 0, 1), ncol = 3, byrow = TRUE)

  p2 <- add_vector_layer(p, vectors = vecs, hemi = "left")

  expect_length(p2$vector_layers, 1L)
  expect_true(!is.null(p2$vector_layers[[1]]$data$left))
})
