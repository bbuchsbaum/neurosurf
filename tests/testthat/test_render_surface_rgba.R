cpu_render <- function(projected, faces, values, width = 12L, height = 12L,
                       threshold = 0, tail = 0L, mask = NULL,
                       return_buffers = TRUE) {
  if (is.null(mask)) mask <- rep(TRUE, nrow(projected))
  neurosurf:::cpp_rasterize_surface_scalar(
    projected, faces, values, rep(0.5, nrow(projected)), mask,
    width, height, threshold, tail, c(-10, 10),
    cbind(seq(0, 1, length.out = 16), 0.2, seq(1, 0, length.out = 16), 1),
    1, 0, 0.75, 0.9, 0L, c(1, 1, 1), 1L, return_buffers
  )
}

test_that("one triangle has the analytic threshold crossing", {
  projected <- rbind(c(0, 0, 1), c(10, 0, 1), c(0, 10, 1))
  out <- cpu_render(projected, matrix(c(1L, 2L, 3L), nrow = 1),
                    c(0, 10, 0), threshold = 5)
  active <- which(out$overlay_alpha > 0, arr.ind = TRUE)
  expect_true(nrow(active) > 0)
  # x pixel centres cross v=5 at x=5 exactly; the first active column has
  # centre 5.5 and therefore column index 6.
  expect_equal(min(active[, "col"]), 6L)
})

test_that("marching triangles returns exact signed crossing coordinates", {
  vertices <- rbind(c(0, 0), c(10, 0), c(0, 10))
  faces <- matrix(c(1L, 2L, 3L), nrow = 1)
  positive <- surface_threshold_segments(
    vertices, faces, c(0, 10, 0), threshold = 5, tail = "positive"
  )
  expect_equal(positive[, c("x", "y")],
               data.frame(x = c(5, 5), y = c(0, 5)), tolerance = 1e-12)

  signed <- surface_threshold_segments(
    vertices, faces, c(-10, 10, 0), threshold = 5, tail = "two_sided"
  )
  expect_setequal(unique(signed$level), c(-5, 5))
  expect_equal(as.integer(table(signed$level)), c(2L, 2L))
})

test_that("optimized scalar buffer matches a slow barycentric reference", {
  projected <- rbind(c(0, 0, 1), c(8, 0, 1), c(0, 8, 1))
  values <- c(-2, 6, 2)
  out <- cpu_render(projected, matrix(c(1L, 2L, 3L), nrow = 1), values,
                    width = 9, height = 9)
  reference <- matrix(NA_real_, 9, 9)
  for (row in seq_len(9)) for (col in seq_len(9)) {
    x <- col - 0.5
    y <- row - 0.5
    w1 <- x / 8
    w2 <- y / 8
    w0 <- 1 - w1 - w2
    if (min(w0, w1, w2) >= -1e-10) {
      reference[row, col] <- sum(c(w0, w1, w2) * values)
    }
  }
  expect_equal(out$scalar, reference, tolerance = 1e-7)
})

test_that("positive and negative thresholds are independent", {
  projected <- rbind(
    c(0, 0, 1), c(5, 0, 1), c(0, 5, 1),
    c(6, 0, 1), c(11, 0, 1), c(11, 5, 1)
  )
  faces <- rbind(c(1L, 2L, 3L), c(4L, 5L, 6L))
  values <- c(4, 4, 4, -4, -4, -4)
  pos <- cpu_render(projected, faces, values, threshold = 3, tail = 1L)
  neg <- cpu_render(projected, faces, values, threshold = 3, tail = -1L)
  expect_true(all(which(pos$overlay_alpha > 0, arr.ind = TRUE)[, "col"] <= 5))
  expect_true(all(which(neg$overlay_alpha > 0, arr.ind = TRUE)[, "col"] >= 7))
})

test_that("z-buffer and face order choose the nearer fragment", {
  xy <- rbind(c(1, 1), c(10, 1), c(1, 10))
  projected <- rbind(cbind(xy, 1), cbind(xy, 2))
  faces <- rbind(c(1L, 2L, 3L), c(4L, 5L, 6L))
  values <- c(rep(-8, 3), rep(8, 3))
  a <- cpu_render(projected, faces, values)
  b <- cpu_render(projected, faces[2:1, , drop = FALSE], values)
  expect_identical(a$rgba, b$rgba)
  expect_gt(a$scalar[3, 3], 0)
})

test_that("vertex and face permutation leave raster buffers unchanged", {
  projected <- rbind(c(1, 1, 1), c(10, 1, 1), c(10, 10, 1), c(1, 10, 1))
  faces <- rbind(c(1L, 2L, 3L), c(1L, 3L, 4L))
  values <- c(-4, -1, 4, 1)
  ref <- cpu_render(projected, faces, values)
  perm <- c(3L, 1L, 4L, 2L)
  inverse <- match(seq_along(perm), perm)
  got <- cpu_render(projected[perm, ], matrix(inverse[faces], ncol = 3),
                    values[perm])
  expect_identical(ref$rgba, got$rgba)
  expect_identical(ref$overlay_alpha, got$overlay_alpha)
})

test_that("shared edges have no cracks and activation area is monotone", {
  projected <- rbind(c(1, 1, 1), c(11, 1, 1), c(11, 11, 1), c(1, 11, 1))
  faces <- rbind(c(1L, 2L, 3L), c(1L, 3L, 4L))
  values <- c(0, 10, 10, 0)
  low <- cpu_render(projected, faces, values, threshold = 2)
  high <- cpu_render(projected, faces, values, threshold = 7)
  expect_true(all(low$coverage[2:10, 2:10]))
  expect_lte(sum(high$overlay_alpha > 0), sum(low$overlay_alpha > 0))
  shifted <- cpu_render(projected, faces, values, threshold = 2.5)
  edge_low <- min(which(low$overlay_alpha > 0, arr.ind = TRUE)[, "col"])
  edge_shifted <- min(which(shifted$overlay_alpha > 0, arr.ind = TRUE)[, "col"])
  expect_lte(abs(edge_shifted - edge_low), 1L)
})

test_that("masked, NA, and degenerate inputs have explicit behavior", {
  projected <- rbind(c(1, 1, 1), c(10, 1, 1), c(1, 10, 1), c(2, 2, 3))
  faces <- rbind(c(1L, 2L, 3L), c(4L, 4L, 4L))
  masked <- cpu_render(projected, faces, c(8, 8, 8, 100), mask = c(TRUE, FALSE,
                                                                    TRUE, TRUE))
  expect_equal(sum(as.integer(masked$overlay_alpha)), 0)
  na_value <- cpu_render(projected, faces, c(NA, 8, 8, 100))
  expect_equal(sum(as.integer(na_value$overlay_alpha)), 0)
  expect_true(any(na_value$coverage))
})

test_that("public renderer is headless and writes a nonblank PNG", {
  skip_if_not_installed("rgl")
  vertices <- rbind(c(0, 0, 0), c(1, 0, 0), c(1, 1, 0), c(0, 1, 0))
  faces <- rbind(c(0L, 1L, 2L), c(0L, 2L, 3L))
  geom <- SurfaceGeometry(vertices, faces, hemi = "lh")
  out <- render_surface_rgba(
    geom, c(-4, -1, 4, 1), width = 80, height = 60,
    camera = "dorsal", threshold = 1, antialias = 2,
    return_buffers = TRUE
  )
  file <- tempfile(fileext = ".png")
  expect_silent(write_surface_rgba(out, file))
  expect_true(file.exists(file))
  expect_gt(file.info(file)$size, 100)
  expect_identical(out$provenance$backend, "cpu_barycentric")
  expect_true(any(out$coverage))
})

test_that("outer contour ignores enclosed holes", {
  coverage <- matrix(FALSE, 9, 9)
  coverage[2:8, 2:8] <- TRUE
  coverage[5, 5] <- FALSE
  edge <- neurosurf:::.ns_outer_contour_mask(coverage)
  expect_true(any(edge[2, 2:8]))
  expect_false(any(edge[4:6, 4:6]))
})

test_that("camera modes are explicit and medial-wall outline is independent", {
  skip_if_not_installed("rgl")
  vertices <- rbind(c(0, 0, 0), c(1, 0, 0), c(1, 1, 0), c(0, 1, 0))
  faces <- rbind(c(0L, 1L, 2L), c(0L, 2L, 3L))
  geom <- SurfaceGeometry(vertices, faces, hemi = "lh")
  canonical <- render_surface_rgba(
    geom, rep(4, 4), cortex_mask = c(TRUE, TRUE, TRUE, FALSE),
    camera = "dorsal", camera_mode = "canonical", medial_wall = "outline",
    width = 60, height = 60, antialias = 1
  )
  expect_identical(canonical$camera$projection, "canonical_orthographic")
  expect_true(any(canonical$coverage & !canonical$cortex_coverage))
  expect_equal(sum(as.integer(canonical$overlay_alpha[
    canonical$coverage & !canonical$cortex_coverage
  ])), 0)
})

test_that("canonical camera landmarks preserve anterior-posterior orientation", {
  landmarks <- rbind(anterior = c(0, 10, 0), posterior = c(0, -10, 0),
                     superior = c(0, 0, 5))
  left <- neurosurf:::.ns_project_surface_camera(
    landmarks, "lateral", "lh", width = 100, height = 80
  )
  right <- neurosurf:::.ns_project_surface_camera(
    landmarks, "lateral", "rh", width = 100, height = 80
  )
  expect_lt(left["anterior", 1], left["posterior", 1])
  expect_gt(right["anterior", 1], right["posterior", 1])
  expect_lt(left["superior", 2], left["anterior", 2])

  presentation <- neurosurf:::.ns_project_surface_camera(
    landmarks, "lateral", "lh", width = 100, height = 80,
    presentation_obliquity = 7
  )
  expect_false(isTRUE(all.equal(left, presentation)))
})

test_that("constant limits and extreme values fail or clamp explicitly", {
  skip_if_not_installed("rgl")
  vertices <- rbind(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0))
  geom <- SurfaceGeometry(vertices, matrix(c(0L, 1L, 2L), nrow = 1),
                          hemi = "lh")
  expect_error(
    render_surface_rgba(geom, rep(1, 3), camera = "dorsal",
                        limits = c(1, 1)),
    "increasing"
  )
  extreme <- render_surface_rgba(
    geom, c(-1e100, 0, 1e100), camera = "dorsal",
    limits = c(-10, 10), width = 30, height = 30, antialias = 1
  )
  expect_true(any(extreme$coverage))
  expect_false(anyNA(extreme$rgba))
})
