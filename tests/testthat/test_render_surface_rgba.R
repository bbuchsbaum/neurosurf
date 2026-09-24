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

test_that("every camera basis is a proper rotation and medial views are not mirrored", {
  for (camera in c("lateral", "medial", "dorsal", "ventral")) {
    for (hemi in c("lh", "rh")) {
      b <- neurosurf:::.ns_camera_basis(camera, hemi)
      expect_equal(crossprod(b), diag(3), tolerance = 1e-12)
      expect_equal(det(b), 1, tolerance = 1e-12)
    }
  }
  landmarks <- rbind(anterior = c(0, 10, 0), posterior = c(0, -10, 0))
  lh_medial <- neurosurf:::.ns_project_surface_camera(
    landmarks, "medial", "lh", width = 100, height = 80
  )
  rh_medial <- neurosurf:::.ns_project_surface_camera(
    landmarks, "medial", "rh", width = 100, height = 80
  )
  # Viewed from the midline, the left hemisphere's anterior pole is on the
  # right of the image and the right hemisphere's is on the left.
  expect_gt(lh_medial["anterior", 1], lh_medial["posterior", 1])
  expect_lt(rh_medial["anterior", 1], rh_medial["posterior", 1])
})

test_that("butterfly subdivision interpolates and adds no scalar extrema", {
  geom <- load_fsaverage_std8("inflated")$lh
  v <- coords(geom)
  f <- t(geom@mesh$it)
  values <- sin(v[, 2] / 9) * 3
  res <- neurosurf:::cpp_butterfly_subdivide(f, cbind(v, values), 3L)
  n <- nrow(v)
  expect_identical(nrow(res$faces), 4L * nrow(f))
  expect_equal(res$attrs[seq_len(n), ], unname(cbind(v, values)))
  new <- seq.int(n + 1L, nrow(res$attrs))
  p <- res$parents[new, ]
  lo <- pmin(values[p[, 1]], values[p[, 2]])
  hi <- pmax(values[p[, 1]], values[p[, 2]])
  expect_true(all(res$attrs[new, 4] >= lo - 1e-12 &
                    res$attrs[new, 4] <= hi + 1e-12))
  # New vertices lie near, not exactly on, the chord between their parents.
  mid <- (v[p[, 1], ] + v[p[, 2], ]) / 2
  offset <- sqrt(rowSums((res$attrs[new, 1:3] - mid)^2))
  edge <- sqrt(rowSums((v[p[, 1], ] - v[p[, 2], ])^2))
  expect_true(all(offset < 0.5 * edge))
  expect_gt(stats::median(offset), 0)
})

test_that("butterfly subdivision reproduces linear fields on a regular grid", {
  g <- expand.grid(x = 0:6, y = 0:6)
  idx <- function(i, j) i * 7 + j + 1
  faces <- NULL
  for (i in 0:5) for (j in 0:5) {
    faces <- rbind(faces, c(idx(i, j), idx(i + 1, j), idx(i + 1, j + 1)),
                   c(idx(i, j), idx(i + 1, j + 1), idx(i, j + 1)))
  }
  attrs <- cbind(g$x, g$y, 0, 2 * g$x - g$y)
  res <- neurosurf:::cpp_butterfly_subdivide(faces, attrs, 3L)
  expect_equal(res$attrs[, 4], 2 * res$attrs[, 1] - res$attrs[, 2],
               tolerance = 1e-12)
})

test_that("display subdivision is automatic for coarse meshes only", {
  geom <- load_fsaverage_std8("inflated")$lh
  values <- coords(geom)[, 3]
  coarse <- render_surface_rgba(geom, values, width = 400, height = 250,
                                antialias = 1)
  expect_gt(coarse$provenance$subdivision_levels, 0L)
  small <- render_surface_rgba(geom, values, width = 40, height = 25,
                               antialias = 1)
  expect_identical(small$provenance$subdivision_levels, 0L)
  off <- render_surface_rgba(geom, values, width = 400, height = 250,
                             antialias = 1, subdivide = FALSE)
  expect_identical(off$provenance$subdivision_levels, 0L)
  expect_error(render_surface_rgba(geom, values, subdivide = "yes"),
               "subdivide")
})

test_that("overlay lookup spans only the suprathreshold range", {
  pal <- c("#0000FF", "#00FFFF", "#FFFF00", "#FF0000")
  lut <- neurosurf:::.ns_overlay_lut(pal, c(-4, 4), 2, "two_sided", 257L)
  at <- function(v) lut[round((v + 4) / 8 * 256) + 1L, 1:3]
  expect_equal(at(2), c(1, 1, 0), tolerance = 0.02)   # weakest positive
  expect_equal(at(4), c(1, 0, 0), tolerance = 0.02)   # strongest positive
  expect_equal(at(-2), c(0, 1, 1), tolerance = 0.02)  # weakest negative
  expect_equal(at(-4), c(0, 0, 1), tolerance = 0.02)  # strongest negative
  pos <- neurosurf:::.ns_overlay_lut(c("#FFFF00", "#FF0000"), c(0, 4), 2,
                                     "positive", 257L)
  expect_equal(pos[129L, 1:3], c(1, 1, 0), tolerance = 0.02)
  flat <- neurosurf:::.ns_overlay_lut(c("#000000", "#FFFFFF"), c(0, 1), 0,
                                      "two_sided", 3L)
  # Without a threshold the palette spans the limits (interpolated in Lab,
  # so perceptual mid-grey is slightly below 0.5 in sRGB).
  expect_equal(flat[c(1, 3), 1], c(0, 1), tolerance = 0.02)
  expect_true(flat[2, 1] > 0.4 && flat[2, 1] < 0.5)
})

test_that("lighting shades the surface and validates its parameters", {
  geom <- load_fsaverage_std8("inflated")$lh
  values <- rep(0, nrow(coords(geom)))
  lit <- render_surface_rgba(geom, values, width = 120, height = 80,
                             antialias = 1, subdivide = FALSE)
  flat <- render_surface_rgba(geom, values, width = 120, height = 80,
                              antialias = 1, subdivide = FALSE,
                              lighting = FALSE)
  grey <- function(x) as.integer(x$rgba[, , 1])[as.vector(x$coverage)]
  expect_gt(stats::sd(grey(lit)), stats::sd(grey(flat)))
  expect_identical(lit$coverage, flat$coverage)
  expect_error(render_surface_rgba(geom, values, lighting = list(glow = 1)),
               "lighting")
  expect_error(render_surface_rgba(geom, values, lighting = "on"),
               "lighting")
})
