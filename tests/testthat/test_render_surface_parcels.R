interior_mask <- function(m) {
  s <- function(dy, dx) neurosurf:::.ns_shift(m, dy, dx, FALSE)
  s(0L, 2L) & s(0L, -2L) & s(2L, 0L) & s(-2L, 0L)
}

uv_sphere <- function(n_lat = 24L, n_lon = 48L, radius = 50, bump = 0) {
  theta <- seq(0, pi, length.out = n_lat + 2L)[-c(1L, n_lat + 2L)]
  phi <- seq(0, 2 * pi, length.out = n_lon + 1L)[-(n_lon + 1L)]
  grid <- expand.grid(phi = phi, theta = theta)
  r <- radius * (1 + bump * cos(6 * grid$phi) * sin(grid$theta))
  body <- cbind(r * sin(grid$theta) * cos(grid$phi),
                r * sin(grid$theta) * sin(grid$phi),
                r * cos(grid$theta))
  vertices <- rbind(body, c(0, 0, radius), c(0, 0, -radius))
  north <- nrow(body) + 1L
  south <- nrow(body) + 2L
  idx <- function(i, j) (i - 1L) * n_lon + ((j - 1L) %% n_lon) + 1L
  faces <- list()
  for (i in seq_len(n_lat - 1L)) for (j in seq_len(n_lon)) {
    faces[[length(faces) + 1L]] <- c(idx(i, j), idx(i + 1L, j), idx(i, j + 1L))
    faces[[length(faces) + 1L]] <- c(idx(i, j + 1L), idx(i + 1L, j), idx(i + 1L, j + 1L))
  }
  for (j in seq_len(n_lon)) {
    faces[[length(faces) + 1L]] <- c(north, idx(1L, j), idx(1L, j + 1L))
    faces[[length(faces) + 1L]] <- c(south, idx(n_lat, j + 1L), idx(n_lat, j))
  }
  SurfaceGeometry(vertices, do.call(rbind, faces) - 1L, hemi = "lh")
}

# Four longitude sectors (labels 1-4); the +x cap (medial side for a left
# hemisphere viewed medially) is medial wall (label 0).
sphere_labels <- function(geometry) {
  v <- t(geometry@mesh$vb[1:3, ])
  angle <- atan2(v[, 3], v[, 2])
  labels <- as.integer(cut(angle, c(-pi, -pi / 2, 0, pi / 2, pi + 1e-9),
                           include.lowest = TRUE))
  labels[v[, 1] > 40] <- 0L
  labels
}

test_that("gbuffer returns nearest faces with exact barycentric weights", {
  projected <- rbind(c(0, 0, 1), c(8, 0, 1), c(0, 8, 1),
                     c(0, 0, 5), c(8, 0, 5), c(0, 8, 5))
  faces <- rbind(c(1L, 2L, 3L), c(4L, 5L, 6L))
  gb <- neurosurf:::cpp_rasterize_surface_gbuffer(projected, faces, 9L, 9L)
  covered <- gb$face > 0L
  expect_true(all(gb$face[covered] == 2L))
  # Pixel (row 2, col 3) has centre (2.5, 1.5): w0 = 1 - (x + y) / 8.
  expect_equal(gb$w0[2, 3], 1 - (2.5 + 1.5) / 8, tolerance = 1e-12)
  expect_equal(gb$w1[2, 3], 2.5 / 8, tolerance = 1e-12)
  expect_false(covered[9, 9])
  expect_true(is.na(gb$w0[9, 9]))
})

test_that("prepare_surface_parcels validates inputs and orients normals outward", {
  g <- uv_sphere()
  labels <- sphere_labels(g)
  expect_error(prepare_surface_parcels(g, labels[-1]), "one non-missing integer")
  expect_error(prepare_surface_parcels(g, labels, anatomy_metric = 1:3),
               "one finite value")
  expect_error(prepare_surface_parcels(g, labels, boundary_smooth = -1),
               "non-negative")
  prep <- prepare_surface_parcels(g, labels)
  v <- prep$vertices
  expect_gt(min(rowSums(prep$normals * v)), 0)
  expect_equal(dim(prep$face_scores), c(nrow(prep$faces), 9L))
  masked <- prepare_surface_parcels(g, labels, cortex_mask = labels != 1L)
  expect_true(all(masked$labels[labels == 1L] == 0L))
})

test_that("render_surface_parcels fills valued parcels and leaves others grey", {
  g <- uv_sphere()
  prep <- prepare_surface_parcels(g, sphere_labels(g))
  out <- render_surface_parcels(
    prep, values = c(`1` = 2, `3` = -2), camera = "lateral",
    width = 60L, height = 60L, antialias = 2L,
    palette = c("#0000FF", "#FFFFFF", "#FF0000"), limits = c(-2, 2),
    style = surface_parcel_style(fill_light = 0, fill_anatomy = 0)
  )
  expect_identical(dim(out$rgba), c(60L, 60L, 4L))
  alpha <- matrix(as.integer(out$rgba[, , 4]), 60, 60)
  expect_identical(alpha[1, 1], 0L)
  expect_identical(alpha[30, 30], 255L)
  labs <- out$labels
  expect_true(all(c(1L, 2L, 3L, 4L) %in% labs))
  channel <- function(k) matrix(as.integer(out$rgba[, , k]), 60, 60)
  r <- channel(1); g2 <- channel(2); b <- channel(3)
  interior <- function(label) {
    m <- labs == label & !is.na(labs)
    m & interior_mask(m)
  }
  red <- interior(1L)
  expect_true(all(r[red] > 200 & g2[red] < 60 & b[red] < 60))
  blue <- interior(3L)
  expect_true(all(b[blue] > 200 & r[blue] < 60))
  grey <- interior(2L)
  expect_true(all(abs(r[grey] - g2[grey]) <= 1 & abs(g2[grey] - b[grey]) <= 1))
  expect_identical(out$provenance$n_filled, 2L)
})

test_that("medial wall renders flat and categorical colours are supported", {
  g <- uv_sphere()
  prep <- prepare_surface_parcels(g, sphere_labels(g))
  out <- render_surface_parcels(
    prep, colors = c(`2` = "#00AA00"), camera = "medial",
    width = 50L, height = 50L, antialias = 2L
  )
  labs <- out$labels
  expect_true(any(labs == 0L, na.rm = TRUE))
  green <- labs == 2L & !is.na(labs)
  g_ch <- matrix(as.integer(out$rgba[, , 2]), 50, 50)
  r_ch <- matrix(as.integer(out$rgba[, , 1]), 50, 50)
  expect_gt(stats::median(g_ch[green] - r_ch[green]), 60)
  expect_error(
    render_surface_parcels(prep, values = c(`1` = 1), colors = c(`1` = "red")),
    "not both"
  )
  expect_error(render_surface_parcels(prep, values = c(1, 2)),
               "one entry per non-zero label")
  expect_error(render_surface_parcels(prep, values = c(`1` = 1),
                                      limits = c(1, 1)),
               "increasing")
})

test_that("empty and all-NA value maps render the underlay only", {
  g <- uv_sphere()
  prep <- prepare_surface_parcels(g, sphere_labels(g))
  out <- render_surface_parcels(prep, values = c(`1` = NA_real_),
                                width = 30L, height = 30L)
  expect_identical(out$provenance$n_filled, 0L)
  out2 <- render_surface_parcels(prep, width = 30L, height = 30L)
  expect_identical(out2$provenance$n_filled, 0L)
})

test_that("boundary smoothing yields a smoother parcel edge than midpoints", {
  g <- uv_sphere(n_lat = 16L, n_lon = 24L)
  labels <- sphere_labels(g)
  rough <- render_surface_parcels(prepare_surface_parcels(g, labels, boundary_smooth = 0L),
                                  width = 80L, height = 80L, antialias = 1L)
  smooth <- render_surface_parcels(prepare_surface_parcels(g, labels, boundary_smooth = 3L),
                                   width = 80L, height = 80L, antialias = 1L)
  # Both partition the visible surface into the same parcels.
  expect_setequal(stats::na.omit(unique(as.vector(rough$labels))),
                  stats::na.omit(unique(as.vector(smooth$labels))))
  expect_identical(smooth$provenance$boundary_smooth, 3L)
})

test_that("surface_parcel_style validates settings", {
  expect_s3_class(surface_parcel_style(), "surface_parcel_style")
  expect_error(surface_parcel_style(gyrus = 2), "in \\[0, 1\\]")
  expect_error(surface_parcel_style(light = c(0, 0, 0)), "non-zero")
  expect_error(surface_parcel_style(nonsense = 1), "Unknown")
  expect_error(surface_parcel_style(contour = list(alpha = 3)), "alpha")
  st <- surface_parcel_style(parcel_line = list(col = "#000000", alpha = 0))
  expect_identical(st$parcel_line$radius, 0L)
  # A plain list is promoted to a style.
  g <- uv_sphere(n_lat = 8L, n_lon = 12L)
  prep <- prepare_surface_parcels(g, sphere_labels(g))
  expect_silent(render_surface_parcels(prep, width = 20L, height = 20L,
                                       style = list(gyrus = 0.95)))
})

test_that("surface_sulcal_proxy is positive on crowns and negative in folds", {
  inflated <- uv_sphere()
  folded <- uv_sphere(bump = 0.08)
  depth <- surface_sulcal_proxy(folded, inflated, iterations = 0L)
  v <- t(inflated@mesh$vb[1:3, ])
  phi <- atan2(v[, 2], v[, 1])
  equator <- abs(v[, 3]) < 10
  crown <- equator & cos(6 * phi) > 0.9
  fold <- equator & cos(6 * phi) < -0.9
  expect_gt(mean(depth[crown]), 0)
  expect_lt(mean(depth[fold]), 0)
  expect_error(surface_sulcal_proxy(folded, uv_sphere(n_lat = 10L)), "topology")
  expect_error(surface_sulcal_proxy(1, inflated), "SurfaceGeometry")
})
