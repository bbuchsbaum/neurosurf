# Tests for surface_plot.R

test_that("surface_plot creates neurosurf_plot object", {
  geom <- example_surface_geometry()

  p <- surface_plot(geom)

  expect_s3_class(p, "neurosurf_plot")
  expect_true(!is.null(p$surfaces$left) || !is.null(p$surfaces$right))
  expect_equal(p$layout$layout, "grid")
  expect_equal(p$zoom, 2)
  expect_equal(p$background, "white")
  expect_equal(p$brightness, 0.5)
  expect_type(p$layers, "list")
  expect_length(p$layers, 0)
})

test_that("surface_plot with different layouts", {
  geom <- example_surface_geometry()

  p_grid <- surface_plot(geom, layout = "grid")
  expect_equal(p_grid$layout$layout, "grid")

  p_row <- surface_plot(geom, layout = "row")
  expect_equal(p_row$layout$layout, "row")

  p_col <- surface_plot(geom, layout = "column")
  expect_equal(p_col$layout$layout, "column")
})

test_that("surface_plot with different views", {
  geom <- example_surface_geometry()

  p <- surface_plot(geom, views = c("lateral", "medial", "dorsal"))
  expect_true(length(p$layout$views) >= 3)
})

test_that("surface_plot rejects invalid views", {
  geom <- example_surface_geometry()

  expect_error(
    surface_plot(geom, views = c("invalid_view")),
    "Invalid view"
  )
})

test_that("surface_plot requires at least one hemisphere", {
  expect_error(
    surface_plot(lh = NULL, rh = NULL),
    "At least one"
  )
})

test_that("add_surface_layer adds layer to plot", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- rnorm(n_verts)

  p <- surface_plot(geom)
  p <- add_surface_layer(p, data = data)

  expect_length(p$layers, 1)
  expect_false(p$layers[[1]]$as_outline)
  expect_equal(p$layers[[1]]$cmap, "viridis")
  expect_equal(p$layers[[1]]$alpha, 1)
})

test_that("add_surface_layer with custom colormap", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- rnorm(n_verts)

  p <- surface_plot(geom)
  p <- add_surface_layer(p, data = data, cmap = c("blue", "white", "red"))

  expect_equal(p$layers[[1]]$cmap, c("blue", "white", "red"))
})

test_that("add_surface_layer with color_range", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- rnorm(n_verts)

  p <- surface_plot(geom)
  p <- add_surface_layer(p, data = data, color_range = c(-2, 2))

  expect_equal(p$layers[[1]]$color_range, c(-2, 2))
})

test_that("add_surface_layer as outline", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  labels <- sample(1:5, n_verts, replace = TRUE)

  p <- surface_plot(geom)
  p <- add_surface_layer(p, data = labels, as_outline = TRUE, show_colorbar = FALSE)

  expect_true(p$layers[[1]]$as_outline)
  expect_false(p$layers[[1]]$show_colorbar)
})

test_that("add_surface_layer validates input", {
  geom <- example_surface_geometry()
  p <- surface_plot(geom)

  expect_error(
    add_surface_layer(list(), data = 1:10),
    "neurosurf_plot"
  )
})

test_that("add_vector_layer adds vector overlay", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  vectors <- matrix(rnorm(n_verts * 3), ncol = 3)

  p <- surface_plot(geom)
  p <- add_vector_layer(p, vectors = vectors)

  expect_length(p$vector_layers, 1)
  expect_equal(p$vector_layers[[1]]$color, "red")
  expect_equal(p$vector_layers[[1]]$alpha, 0.8)
})

test_that("add_vector_layer validates vector dimensions", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  bad_vectors <- matrix(rnorm(n_verts * 2), ncol = 2)

  p <- surface_plot(geom)

  expect_error(
    add_vector_layer(p, vectors = bad_vectors),
    "3 columns"
  )
})

test_that("add_atlas_outline adds outline layer with defaults", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  labels <- sample(1:5, n_verts, replace = TRUE)

  p <- surface_plot(geom)
  p <- add_atlas_outline(p, labels = labels)

  expect_length(p$layers, 1)
  expect_true(p$layers[[1]]$as_outline)
  expect_equal(p$layers[[1]]$outline_col, "black")
  expect_equal(p$layers[[1]]$outline_lwd, 1.5)
})

test_that("add_atlas_outline validates input", {
  geom <- example_surface_geometry()

  expect_error(
    add_atlas_outline(list(), labels = 1:10),
    "neurosurf_plot"
  )
})

test_that(".ns_autocrop removes whitespace", {
  # Create test image with white border
  img <- array(1, dim = c(100, 100, 3))  # All white
  img[40:60, 40:60, ] <- 0.5  # Gray center

  cropped <- neurosurf:::.ns_autocrop(img, border = 5)

  expect_true(nrow(cropped) < nrow(img))
  expect_true(ncol(cropped) < ncol(img))
})

test_that(".ns_autocrop handles empty image", {
  img <- array(1, dim = c(100, 100, 3))  # All white
  cropped <- neurosurf:::.ns_autocrop(img)

  # Should return original if no content found

  expect_true(is.array(cropped))
})

test_that(".ns_resize_img returns image unchanged (placeholder)", {
  img <- array(0.5, dim = c(100, 100, 3))
  resized <- neurosurf:::.ns_resize_img(img)

  expect_equal(dim(resized), dim(img))
})

test_that(".ns_cmap_to_colors generates color vectors", {
  cols <- neurosurf:::.ns_cmap_to_colors("viridis", n = 10)
  expect_length(cols, 10)
  expect_true(all(grepl("^#", cols)))

  cols2 <- neurosurf:::.ns_cmap_to_colors(c("blue", "white", "red"), n = 5)
  expect_length(cols2, 5)
})

test_that(".ns_check_views validates view names", {
  expect_silent(neurosurf:::.ns_check_views(c("lateral", "medial")))
  expect_error(
    neurosurf:::.ns_check_views(c("invalid")),
    "Invalid view"
  )
})

test_that(".ns_split_layer_data handles list input", {
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))

  surfaces <- list(left = geom, right = NULL)
  data_list <- list(left = rnorm(n), right = NULL)

  result <- neurosurf:::.ns_split_layer_data(surfaces, data_list, "both")

  expect_equal(length(result$left), n)
  expect_null(result$right)
})

test_that(".ns_split_layer_data handles numeric input", {
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))

  surfaces <- list(left = geom, right = NULL)
  data_vec <- rnorm(n)

  result <- neurosurf:::.ns_split_layer_data(surfaces, data_vec, "both")

  expect_equal(length(result$left), n)
})

test_that("multiple layers can be added", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))

  p <- surface_plot(geom)
  p <- add_surface_layer(p, data = rnorm(n_verts), label = "layer1")
  p <- add_surface_layer(p, data = rnorm(n_verts), label = "layer2")

  expect_length(p$layers, 2)
  expect_equal(p$layers[[1]]$label, "layer1")
  expect_equal(p$layers[[2]]$label, "layer2")
})

test_that("plot.neurosurf_plot exists", {
  expect_true(is.function(plot.neurosurf_plot))
})

# Tests for .ns_normalize_surface
test_that(".ns_normalize_surface returns NULL for NULL input", {
  result <- neurosurf:::.ns_normalize_surface(NULL, "left")
  expect_null(result)
})

test_that(".ns_normalize_surface returns SurfaceGeometry unchanged", {
  geom <- example_surface_geometry()
  result <- neurosurf:::.ns_normalize_surface(geom, "left")
  expect_s4_class(result, "SurfaceGeometry")
})

test_that(".ns_normalize_surface errors on invalid input", {
  expect_error(
    neurosurf:::.ns_normalize_surface(123, "left"),
    "Unsupported surface"
  )
})

# Tests for .ns_set_layout
test_that(".ns_set_layout creates grid layout", {
  geom <- example_surface_geometry()
  result <- neurosurf:::.ns_set_layout(
    lh = geom, rh = NULL,
    layout = "grid",
    views = c("lateral", "medial"),
    mirror_views = FALSE,
    flip = FALSE
  )
  expect_equal(result$layout, "grid")
  expect_equal(length(result$views), 2)
  expect_equal(result$hemis, c("left", "left"))
})

test_that(".ns_set_layout creates row layout", {
  geom <- example_surface_geometry()
  result <- neurosurf:::.ns_set_layout(
    lh = geom, rh = geom,
    layout = "row",
    views = c("lateral"),
    mirror_views = FALSE,
    flip = FALSE
  )
  expect_equal(result$layout, "row")
  expect_equal(result$dims, c(1L, 2L))
})

test_that(".ns_set_layout creates column layout", {
  geom <- example_surface_geometry()
  result <- neurosurf:::.ns_set_layout(
    lh = geom, rh = NULL,
    layout = "column",
    views = c("lateral", "medial"),
    mirror_views = FALSE,
    flip = FALSE
  )
  expect_equal(result$layout, "column")
  expect_equal(result$dims, c(2L, 1L))
})

test_that(".ns_set_layout handles flip for bilateral", {
  geom <- example_surface_geometry()
  result <- neurosurf:::.ns_set_layout(
    lh = geom, rh = geom,
    layout = "grid",
    views = c("lateral"),
    mirror_views = FALSE,
    flip = TRUE
  )
  # With flip, right hemisphere should come first

  expect_equal(result$hemis[1], "right")
})

# Tests for .ns_split_vertices
test_that(".ns_split_vertices handles list input", {
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))
  surfaces <- list(left = geom, right = NULL)
  verts <- list(left = 1:10, right = NULL)

  result <- neurosurf:::.ns_split_vertices(surfaces, verts, "both")
  expect_equal(result$left, 1:10)
  expect_null(result$right)
})

test_that(".ns_split_vertices handles numeric input for single hemi", {
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))
  surfaces <- list(left = geom, right = NULL)
  verts <- 1:10

  # When hemi is "left", numeric input is assigned to left
  result <- neurosurf:::.ns_split_vertices(surfaces, verts, "left")
  expect_equal(result$left, 1:10)
  expect_null(result$right)
})

test_that(".ns_split_vertices returns NULL for NULL input", {
  geom <- example_surface_geometry()
  surfaces <- list(left = geom, right = NULL)

  result <- neurosurf:::.ns_split_vertices(surfaces, NULL, "both")
  expect_null(result$left)
  expect_null(result$right)
})

# Tests for .ns_compute_vertex_colors
test_that(".ns_compute_vertex_colors returns gray for empty layers", {
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))

  result <- neurosurf:::.ns_compute_vertex_colors(
    layers = list(),
    surf = geom,
    hemi = "left",
    brightness = 0.5
  )
  expect_length(result, n)
  expect_true(all(grepl("^#", result)))
})

test_that(".ns_compute_vertex_colors applies layer colors", {
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))
  data <- rep(1, n)

  layer <- list(
    data = list(left = data, right = NULL),
    cmap = "viridis",
    color_range = c(0, 2),
    alpha = 1,
    as_outline = FALSE,
    zero_transparent = FALSE
  )

  result <- neurosurf:::.ns_compute_vertex_colors(
    layers = list(layer),
    surf = geom,
    hemi = "left",
    brightness = 0.5
  )
  expect_length(result, n)
  expect_true(all(grepl("^#", result)))
})

# Tests for .ns_vertex_normals
test_that(".ns_vertex_normals computes vertex normals", {
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))

  result <- neurosurf:::.ns_vertex_normals(geom)
  expect_equal(nrow(result), n)
  expect_equal(ncol(result), 3)
})

# Tests for .ns_build_colorbars
test_that(".ns_build_colorbars returns nullGrob for empty layers", {
  result <- neurosurf:::.ns_build_colorbars(list())
  expect_s3_class(result, "grob")
})

test_that(".ns_build_colorbars builds grob for layers", {
  layer <- list(
    show_colorbar = TRUE,
    as_outline = FALSE,
    cmap = "viridis",
    color_range = c(-1, 1),
    label = "Test"
  )
  result <- neurosurf:::.ns_build_colorbars(list(layer))
  # Returns a gTree or grob-like object
  expect_true(inherits(result, "grob") || inherits(result, "gTree"))
})

# Tests for .ns_split_vector_data
test_that(".ns_split_vector_data handles matrix input", {
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))
  surfaces <- list(left = geom, right = NULL)
  vectors <- matrix(rnorm(n * 3), ncol = 3)

  result <- neurosurf:::.ns_split_vector_data(surfaces, vectors, "both")
  expect_equal(nrow(result$left), n)
  expect_null(result$right)
})

# Tests for show_surface_plot
test_that("show_surface_plot creates plot object", {
  skip_if_not(interactive() || rgl::rgl.useNULL(), "Requires RGL device")
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))

  # Should not error and return a neurosurf_plot invisibly
  old_null <- getOption("rgl.useNULL")
  options(rgl.useNULL = TRUE)
  on.exit(options(rgl.useNULL = old_null), add = TRUE)

  result <- show_surface_plot(geom, data = rnorm(n))
  expect_s3_class(result, "neurosurf_plot")
  expect_length(result$layers, 1)
})

# Tests for surface_plot bilateral
test_that("surface_plot with bilateral hemispheres", {
  geom <- example_surface_geometry()

  p <- surface_plot(lh = geom, rh = geom)
  expect_s3_class(p, "neurosurf_plot")
  expect_true(!is.null(p$surfaces$left))
  expect_true(!is.null(p$surfaces$right))
})

# Tests for add_surface_layer with vertices parameter
test_that("add_surface_layer with specific vertices", {
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))

  p <- surface_plot(geom)
  # Need to specify hemi when using sparse data with vertices
  p <- add_surface_layer(p, data = rep(1, 10), vertices = 1:10, hemi = "left")

  expect_length(p$layers, 1)
  expect_equal(p$layers[[1]]$vertices$left, 1:10)
})
