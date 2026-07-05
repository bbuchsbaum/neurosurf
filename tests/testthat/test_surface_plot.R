# Tests for surface_plot.R

# ---- Figure assembly / framing (GH #74) -----------------------------------

test_that(".ns_autocrop is background-aware (crops on non-white backgrounds)", {
  mk <- function(bg) {
    img <- array(0, dim = c(200, 300, 3))
    for (k in 1:3) img[, , k] <- bg[k]
    img[80:120, 130:170, 1] <- 0.9
    img[80:120, 130:170, 2] <- 0.2
    img[80:120, 130:170, 3] <- 0.2
    img
  }
  # brain blob is 41 x 41
  dark <- neurosurf:::.ns_autocrop(mk(c(0.133, 0.133, 0.133)), border = 0,
                                   bg = "#222222")
  expect_equal(dim(dark)[1:2], c(41L, 41L))
  black <- neurosurf:::.ns_autocrop(mk(c(0, 0, 0)), border = 0, bg = "black")
  expect_equal(dim(black)[1:2], c(41L, 41L))
  # white still works, and the legacy (bg = NULL) path is unchanged
  white <- neurosurf:::.ns_autocrop(mk(c(1, 1, 1)), border = 0, bg = "white")
  expect_equal(dim(white)[1:2], c(41L, 41L))
  legacy <- neurosurf:::.ns_autocrop(mk(c(1, 1, 1)), border = 0)
  expect_equal(dim(legacy)[1:2], c(41L, 41L))
})

test_that(".ns_autocrop margin keeps a proportional border", {
  img <- array(1, dim = c(200, 200, 3))
  img[81:120, 81:120, 1] <- 0.1 # 40x40 content, non-white
  img[81:120, 81:120, 2] <- 0.1
  img[81:120, 81:120, 3] <- 0.1
  # margin = 0.25 of the 40px content extent -> 10px border each side -> 60x60
  m <- neurosurf:::.ns_autocrop(img, bg = "white", margin = 0.25)
  expect_equal(dim(m)[1:2], c(60L, 60L))
  # margin = 0 crops flush
  m0 <- neurosurf:::.ns_autocrop(img, bg = "white", margin = 0)
  expect_equal(dim(m0)[1:2], c(40L, 40L))
})

test_that(".ns_assemble_grid_layout sizes cells to panel pixel dimensions", {
  # 2x2, column-major: k1->(1,1) k2->(2,1) k3->(1,2) k4->(2,2)
  panels <- list(
    list(image = array(0, dim = c(100, 130, 3))), # r1 c1
    list(image = array(0, dim = c(110, 120, 3))), # r2 c1
    list(image = array(0, dim = c(105, 140, 3))), # r1 c2
    list(image = array(0, dim = c(115, 125, 3)))  # r2 c2
  )
  lay <- neurosurf:::.ns_assemble_grid_layout(panels, nrow = 2, ncol = 2)
  expect_equal(lay$rows, c(1, 2, 1, 2))
  expect_equal(lay$cols, c(1, 1, 2, 2))
  # column widths = max panel width per column
  expect_equal(lay$col_w, c(max(130, 120), max(140, 125)))
  # row heights = max panel height per row
  expect_equal(lay$row_h, c(max(100, 105), max(110, 115)))
  # a panel that is the col/row max fills its cell (npc == 1)
  expect_equal(lay$pw[3] / lay$col_w[2], 1) # panel 3 is widest in col 2
})

test_that(".ns_trim_png crops a rendered figure to its content", {
  skip_if_not_installed("png")
  img <- array(0, dim = c(200, 300, 3)) # black background
  img[80:120, 130:170, 1] <- 0.9        # bright blob, 41x41
  img[80:120, 130:170, 2] <- 0.2
  img[80:120, 130:170, 3] <- 0.2
  f <- tempfile(fileext = ".png")
  png::writePNG(img, f)
  dims <- neurosurf:::.ns_trim_png(f, bg = "black")
  expect_equal(dims[1:2], c(41L, 41L))
  expect_equal(dim(png::readPNG(f))[1:2], c(41L, 41L))
  unlink(f)
})

test_that(".ns_autocrop ignores a fully-opaque alpha channel and uses bg", {
  # 4-channel image, alpha all opaque, dark background with a bright blob.
  img <- array(0, dim = c(200, 300, 4))
  img[, , 1:3] <- 0.133           # dark bg
  img[, , 4] <- 1                 # fully opaque everywhere
  img[80:120, 130:170, 1] <- 0.9  # blob 41x41
  img[80:120, 130:170, 2:3] <- 0.2
  # alpha branch would keep the whole image; bg detection must crop to the blob
  cropped <- neurosurf:::.ns_autocrop(img, bg = "#222222")
  expect_equal(dim(cropped)[1:2], c(41L, 41L))
})

test_that(".ns_autocrop bg='auto' detects background from image corners", {
  # background is 0.102 (a device-shifted #222222), not the nominal 0.133
  img <- array(0, dim = c(200, 300, 3))
  img[, , 1:3] <- 0.102
  img[80:120, 130:170, 1] <- 0.9
  img[80:120, 130:170, 2:3] <- 0.2
  # nominal "#222222" with tight fuzz fails to match the shifted bg ...
  nominal <- neurosurf:::.ns_autocrop(img, bg = "#222222", fuzz = 0.02)
  expect_gt(nrow(nominal), 41L)
  # ... but corner auto-detection nails it
  auto <- neurosurf:::.ns_autocrop(img, bg = "auto", fuzz = 0.02)
  expect_equal(dim(auto)[1:2], c(41L, 41L))
})

test_that("surface_plot stores margin and validates it", {
  geom <- example_surface_geometry()
  p <- surface_plot(geom, margin = 0.05)
  expect_equal(p$margin, 0.05)
  expect_error(surface_plot(geom, margin = -1), "non-negative")
  expect_error(surface_plot(geom, margin = c(0.1, 0.2)), "single")
})

test_that("show_surface_plot accepts background/zoom/margin without error", {
  # These plumb into surface_plot(); we can build the object without rendering.
  geom <- example_surface_geometry()
  # surface_plot is the part that must accept the args (rendering needs rgl).
  p <- surface_plot(geom, background = "#222222", zoom = 4, margin = 0.02)
  expect_equal(p$background, "#222222")
  expect_equal(p$zoom, 4)
  expect_equal(p$margin, 0.02)
})

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

test_that("add_surface_layer accepts irange alias and threshold band", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- rnorm(n_verts)

  p <- surface_plot(geom)
  p <- add_surface_layer(p, data = data, irange = c(-3, 3), thresh = 2)

  expect_equal(p$layers[[1]]$color_range, c(-3, 3))
  expect_equal(p$layers[[1]]$thresh, c(-2, 2))
})

test_that("add_surface_layer accepts NeuroSurface data directly", {
  geom <- example_surface_geometry()
  n_verts <- nrow(coords(geom))
  data <- rnorm(n_verts)
  ns <- NeuroSurface(geom, indices = seq_len(n_verts), data = data)

  p <- surface_plot(geom)
  p <- add_surface_layer(p, data = ns)

  expect_equal(p$layers[[1]]$data$left, data)
  expect_equal(p$layers[[1]]$vertices$left, seq_len(n_verts))
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

test_that("draw_surface_plot includes an explicit background grob", {
  skip_if_not_installed("grid")

  fake_plot <- structure(
    list(
      layout = list(dims = c(1L, 1L)),
      layers = list(),
      background = "white"
    ),
    class = "neurosurf_plot"
  )

  g <- testthat::with_mocked_bindings(
    render_surface_plot = function(...) {
      list(
        panels = list(list(image = array(1, dim = c(10, 10, 3)), aspect = 1)),
        layout = list(dims = c(1L, 1L))
      )
    },
    draw_surface_plot(fake_plot, colorbar = FALSE)
  )

  expect_s3_class(g, "gTree")
  expect_s3_class(g$children[[1]], "rect")
  expect_equal(g$children[[1]]$gp$fill, "white")
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

test_that("view_surface does not warn on explicit vertex colors without vals", {
  skip_if_not_installed("rgl")

  geom <- example_surface_geometry()
  n <- nrow(coords(geom))
  old_null <- getOption("rgl.useNULL")
  options(rgl.useNULL = TRUE)
  on.exit(options(rgl.useNULL = old_null), add = TRUE)

  rgl::open3d()
  on.exit(try(rgl::close3d(), silent = TRUE), add = TRUE)

  warnings_seen <- character()
  withCallingHandlers(
    view_surface(
      geom,
      vals = NA,
      vert_clrs = rep("#FFFFFF", n),
      bgcol = NA,
      new_window = FALSE
    ),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_false(any(grepl("no non-missing arguments", warnings_seen)))
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

test_that(".ns_snapshot_file_ok rejects blank snapshot failures", {
  skip_if_not_installed("png")

  black_file <- tempfile(fileext = ".png")
  white_file <- tempfile(fileext = ".png")
  valid_file <- tempfile(fileext = ".png")
  on.exit(unlink(c(black_file, white_file, valid_file)), add = TRUE)

  png::writePNG(array(0, dim = c(20, 20, 3)), black_file)
  png::writePNG(array(1, dim = c(20, 20, 3)), white_file)

  valid <- array(1, dim = c(20, 20, 3))
  valid[6:15, 6:15, 1] <- 0.2
  valid[6:15, 6:15, 2] <- 0.6
  valid[6:15, 6:15, 3] <- 0.8
  png::writePNG(valid, valid_file)

  expect_false(neurosurf:::.ns_snapshot_file_ok(black_file))
  expect_false(neurosurf:::.ns_snapshot_file_ok(white_file))
  expect_true(neurosurf:::.ns_snapshot_file_ok(valid_file))
})

test_that(".ns_quiet_snapshot suppresses snapshot backend chatter", {
  out <- utils::capture.output({
    msg <- utils::capture.output({
      result <- neurosurf:::.ns_quiet_snapshot({
        cat("backend stdout\n")
        message("backend message")
        TRUE
      })
    }, type = "message")
  }, type = "output")

  expect_true(result)
  expect_length(out, 0L)
  expect_length(msg, 0L)
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
  skip_if_not(interactive(), "Requires interactive session for RGL rendering")
  skip_on_cran()

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

test_that("show_surface_plot can draw to a PNG file", {
  skip_if_not_installed("grid")
  skip_if_not(capabilities("png"), "PNG device unavailable")

  geom <- example_surface_geometry()
  n <- nrow(coords(geom))
  out <- tempfile(fileext = ".png")

  result <- testthat::with_mocked_bindings(
    draw_surface_plot = function(...) grid::nullGrob(),
    show_surface_plot(
      geom,
      data = rnorm(n),
      file = out,
      width = 64,
      height = 64,
      thresh = 1,
      irange = c(-2, 2)
    )
  )

  expect_s3_class(result, "neurosurf_plot")
  expect_true(file.exists(out))
  expect_equal(result$layers[[1]]$thresh, c(-1, 1))
  expect_equal(result$layers[[1]]$color_range, c(-2, 2))
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

# ---- Data-modulated alpha (GH #73) ----------------------------------------

test_that("scalar alpha is stored as a uniform mode (regression)", {
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))

  p <- add_surface_layer(surface_plot(geom), data = rnorm(n), alpha = 0.5)
  layer <- p$layers[[1]]

  expect_equal(layer$alpha, 0.5)
  expect_equal(layer$alpha_mode, "uniform")
  expect_null(layer$alpha_values)
})

test_that("per-vertex alpha vector is split and stored per hemisphere", {
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))
  av <- seq(0, 1, length.out = n)

  p <- add_surface_layer(surface_plot(geom), data = rnorm(n), alpha = av,
                         hemi = "left")
  layer <- p$layers[[1]]

  expect_equal(layer$alpha_mode, "uniform")
  expect_equal(layer$alpha, 1)
  expect_equal(layer$alpha_values$left, av)
})

test_that("per-vertex alpha is clamped to [0, 1]", {
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))

  p <- add_surface_layer(surface_plot(geom), data = rnorm(n),
                         alpha = rep(c(-1, 2), length.out = n), hemi = "left")
  vals <- p$layers[[1]]$alpha_values$left
  expect_true(all(vals >= 0 & vals <= 1))
})

test_that("alpha = 'soft' records soft mode with a default alpha_range", {
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))

  p <- add_surface_layer(surface_plot(geom), data = rnorm(n), alpha = "soft",
                         color_range = c(-2, 3))
  layer <- p$layers[[1]]

  expect_equal(layer$alpha_mode, "soft")
  # default alpha_range = c(0, max(abs(color_range)))
  expect_equal(layer$alpha_range, c(0, 3))
})

test_that("invalid alpha specifications error", {
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))
  p <- surface_plot(geom)

  expect_error(add_surface_layer(p, data = rnorm(n), alpha = 1.5),
               "\\[0, 1\\]")
  expect_error(add_surface_layer(p, data = rnorm(n), alpha = "fade"),
               "should be")
  expect_error(
    add_surface_layer(p, data = rnorm(n), alpha = "soft",
                      alpha_range = c(1, 0)),
    "alpha_range"
  )
  expect_error(
    add_surface_layer(p, data = rnorm(n), alpha = "soft",
                      alpha_gamma = -1),
    "alpha_gamma"
  )
})

test_that(".ns_soft_alpha implements clamp((|v|-lo)/(hi-lo),0,1)^gamma", {
  v <- c(0, 0.15, 0.3, 0.45, NA)
  expect_equal(neurosurf:::.ns_soft_alpha(v, c(0, 0.3), NULL),
               c(0, 0.5, 1, 1, 0))
  expect_equal(neurosurf:::.ns_soft_alpha(v, c(0, 0.3), 2),
               c(0, 0.25, 1, 1, 0))
})

test_that(".ns_resolve_layer_alpha combines scalar, per-vertex, and soft", {
  # scalar
  expect_equal(
    neurosurf:::.ns_resolve_layer_alpha(
      list(alpha = 0.5, alpha_mode = "uniform"), rep(1, 3), "left", 3),
    rep(0.5, 3))
  # per-vertex times scalar
  expect_equal(
    neurosurf:::.ns_resolve_layer_alpha(
      list(alpha = 0.5, alpha_mode = "uniform",
           alpha_values = list(left = c(0, 1), right = NULL)),
      rep(1, 2), "left", 2),
    c(0, 0.5))
  # soft
  expect_equal(
    neurosurf:::.ns_resolve_layer_alpha(
      list(alpha = 1, alpha_mode = "soft", alpha_range = c(0, 0.3),
           alpha_gamma = NULL),
      c(0, 0.15, 0.3), "left", 3),
    c(0, 0.5, 1))
})

test_that("named cmap palettes resolve via hcl.colors instead of the fallback", {
  inferno <- neurosurf:::.ns_cmap_to_colors("inferno", 8)
  viridis <- neurosurf:::.ns_cmap_to_colors("viridis", 8)
  fallback <- neurosurf:::.ns_cmap_to_colors(c("blue", "white", "red"), 8)

  expect_length(inferno, 8)
  expect_false(identical(inferno, viridis))
  expect_false(identical(inferno, fallback))
  # case/separator insensitive
  expect_equal(neurosurf:::.ns_cmap_to_colors("Inferno", 8), inferno)
})

test_that("an unknown cmap name warns and falls back", {
  expect_warning(
    neurosurf:::.ns_cmap_to_colors("not-a-real-palette", 8),
    "Unknown cmap"
  )
})

test_that("data-modulated alpha pulls vertex colours toward the base", {
  # Larger grid mesh so a per-vertex ramp is meaningful.
  k <- 8
  g <- expand.grid(x = seq_len(k), y = seq_len(k))
  verts <- cbind(g$x, g$y, 0)
  faces <- do.call(rbind, lapply(seq_len(k - 1), function(i) {
    do.call(rbind, lapply(seq_len(k - 1), function(j) {
      a <- (j - 1) * k + i
      rbind(c(a, a + 1, a + k), c(a + 1, a + k + 1, a + k))
    }))
  }))
  geom <- SurfaceGeometry(verts, faces - 1L, hemi = "lh")
  n <- nrow(coords(geom))
  d <- seq(0, 0.3, length.out = n)

  base_rgb <- as.numeric(grDevices::col2rgb(grDevices::gray(0.6)))
  dist_from_base <- function(alpha, ...) {
    p <- add_surface_layer(surface_plot(geom, views = "lateral"), data = d,
                           hemi = "left", alpha = alpha, zero_transparent = FALSE,
                           ...)
    cols <- neurosurf:::.ns_compute_vertex_colors(list(p$layers[[1]]), geom,
                                                  "left", 0.6)
    sqrt(colSums((grDevices::col2rgb(cols) - base_rgb)^2))
  }

  # alpha = 0 -> every vertex is exactly the base colour
  expect_true(max(dist_from_base(0)) < 1e-6)

  # opacity is monotone in the scalar
  d0  <- dist_from_base(0)
  d05 <- dist_from_base(0.5)
  d1  <- dist_from_base(1)
  expect_true(all(d0 <= d05 + 1e-6) && all(d05 <= d1 + 1e-6))

  # per-vertex ramp: higher alpha => further from base
  av <- seq(0, 1, length.out = n)
  expect_gt(cor(av, dist_from_base(av)), 0.8)

  # soft: colour distance from base tracks |data|; d == 0 stays at base
  ds <- dist_from_base("soft", color_range = c(0, 0.3))
  expect_gt(cor(abs(d), ds), 0.8)
  expect_lt(ds[1], 1e-6)
})

test_that(".ns_normalize_color_range expands a degenerate range", {
  # constant range -> symmetric non-zero-width band centred on the value
  r <- neurosurf:::.ns_normalize_color_range(c(5, 5))
  expect_lt(r[1], 5)
  expect_gt(r[2], 5)
  expect_equal(mean(r), 5)
  # zero-centred degenerate range
  r0 <- neurosurf:::.ns_normalize_color_range(c(0, 0))
  expect_lt(r0[1], 0)
  expect_gt(r0[2], 0)
  # non-finite -> c(0, 1)
  expect_equal(neurosurf:::.ns_normalize_color_range(c(NA, 1)), c(0, 1))
  # ordinary range untouched
  expect_equal(neurosurf:::.ns_normalize_color_range(c(-2, 3)), c(-2, 3))
})

test_that("constant non-zero data renders opaquely (degenerate range guard)", {
  geom <- example_surface_geometry()
  n <- nrow(coords(geom))

  # No explicit color_range: constant data would collapse range to c(v, v).
  p <- add_surface_layer(surface_plot(geom, views = "lateral"),
                         data = rep(0.7, n), hemi = "left",
                         zero_transparent = FALSE)
  # stored range must have positive width
  expect_gt(diff(p$layers[[1]]$color_range), 0)

  cols <- neurosurf:::.ns_compute_vertex_colors(list(p$layers[[1]]), geom,
                                                "left", 0.6)
  base_rgb <- as.numeric(grDevices::col2rgb(grDevices::gray(0.6)))
  dist <- sqrt(colSums((grDevices::col2rgb(cols) - base_rgb)^2))
  # every vertex is coloured (not transparent -> not left as the base colour)
  expect_true(all(dist > 0))
  # ... and constant data maps to a single flat colour
  expect_equal(length(unique(cols)), 1L)
})

test_that("per-vertex and soft alpha are recovered exactly from rendered colours", {
  # Compositing a layer over an opaque base gives
  #   out = fg * m + base * (1 - m)
  # so with a CONSTANT foreground colour we can invert for the effective
  # per-vertex opacity m and check it equals the requested alpha exactly
  # (to 8-bit rounding).
  k <- 8
  g <- expand.grid(x = seq_len(k), y = seq_len(k))
  verts <- cbind(g$x, g$y, 0)
  faces <- do.call(rbind, lapply(seq_len(k - 1), function(i) {
    do.call(rbind, lapply(seq_len(k - 1), function(j) {
      a <- (j - 1) * k + i
      rbind(c(a, a + 1, a + k), c(a + 1, a + k + 1, a + k))
    }))
  }))
  geom <- SurfaceGeometry(verts, faces - 1L, hemi = "lh")
  n <- nrow(coords(geom))

  fg_hex <- "#FF0000"
  base_rgb <- as.numeric(grDevices::col2rgb(grDevices::gray(0.6)))
  denom <- as.numeric(grDevices::col2rgb(fg_hex)) - base_rgb
  d <- seq(0.1, 1, length.out = n) # varying, all nonzero

  recover <- function(layer) {
    cols <- neurosurf:::.ns_compute_vertex_colors(list(layer), geom, "left", 0.6)
    out <- grDevices::col2rgb(cols)
    vapply(seq_len(n), function(i) mean((out[, i] - base_rgb) / denom), numeric(1))
  }

  # per-vertex ramp recovered to 8-bit precision
  av <- seq(0, 1, length.out = n)
  p <- add_surface_layer(surface_plot(geom, views = "lateral"), data = d,
                         hemi = "left", cmap = c(fg_hex, fg_hex),
                         color_range = c(0, 1), alpha = av,
                         zero_transparent = FALSE)
  expect_equal(recover(p$layers[[1]]), av, tolerance = 0.01)

  # soft mode recovers clamp(|d| / 1) == d
  ps <- add_surface_layer(surface_plot(geom, views = "lateral"), data = d,
                          hemi = "left", cmap = c(fg_hex, fg_hex),
                          color_range = c(0, 1), alpha = "soft",
                          alpha_range = c(0, 1), zero_transparent = FALSE)
  expect_equal(recover(ps$layers[[1]]), d, tolerance = 0.01)
})

test_that("thresh with alpha < 1 applies opacity once (no double application)", {
  k <- 6
  g <- expand.grid(x = seq_len(k), y = seq_len(k))
  verts <- cbind(g$x, g$y, 0)
  faces <- do.call(rbind, lapply(seq_len(k - 1), function(i) {
    do.call(rbind, lapply(seq_len(k - 1), function(j) {
      a <- (j - 1) * k + i
      rbind(c(a, a + 1, a + k), c(a + 1, a + k + 1, a + k))
    }))
  }))
  geom <- SurfaceGeometry(verts, faces - 1L, hemi = "lh")
  n <- nrow(coords(geom))
  d <- seq(0, 0.3, length.out = n)

  base_rgb <- as.numeric(grDevices::col2rgb(grDevices::gray(0.6)))
  colour_dist <- function(alpha, thresh = NULL) {
    p <- add_surface_layer(surface_plot(geom, views = "lateral"), data = d,
                           hemi = "left", alpha = alpha, thresh = thresh,
                           color_range = c(0, 0.3), zero_transparent = FALSE)
    cols <- neurosurf:::.ns_compute_vertex_colors(list(p$layers[[1]]), geom,
                                                  "left", 0.6)
    sqrt(colSums((grDevices::col2rgb(cols) - base_rgb)^2))
  }

  above <- d >= 0.15
  # For above-threshold vertices, distance-from-base at alpha=0.5 should be
  # ~half of that at alpha=1 (double application would make it ~a quarter).
  half <- mean(colour_dist(0.5, thresh = 0.15)[above])
  full <- mean(colour_dist(1, thresh = 0.15)[above])
  expect_equal(half / full, 0.5, tolerance = 0.05)
})
