reference_spec <- function(mask = rep(1, 4), ...) {
  modifyList(list(id = "psl", label = "PSL", mask = mask,
                  color = "#0000FF", opacity = 0.4,
                  provenance = list(source = "synthetic", mesh = "square")),
             list(...))
}

reference_square <- function() {
  SurfaceGeometry(rbind(c(0, 0, 0), c(1, 0, 0), c(1, 1, 0), c(0, 1, 0)),
                  rbind(c(0L, 1L, 2L), c(0L, 2L, 3L)), hemi = "lh")
}

reference_render <- function(reference = NULL, values = rep(0, 4), ...) {
  args <- modifyList(list(geometry = reference_square(), vertex_values = values,
                         reference = reference, width = 32, height = 32,
                         camera = "dorsal", lighting = FALSE, antialias = 1L,
                         anatomy_style = "continuous", anatomy_invert = FALSE,
                         subdivide = FALSE, outer_contour = FALSE,
                         threshold = 1, limits = c(0, 4),
                         palette = c("#FF0000", "#FF0000"),
                         anatomy_range = c(0.4, 0.6), return_buffers = TRUE),
                    list(...))
  do.call(render_surface_rgba, args)
}

reference_rgb <- function(x) as.integer(x$rgba[16, 16, 1:3])

test_that("CPU reference composes source-over between anatomy and effect", {
  # Constant anatomy = 0.5, unlit blue reference at 0.4, red effect at 0.5.
  base <- reference_render()
  ref <- reference_render(reference_spec())
  overlap <- reference_render(reference_spec(), rep(4, 4), overlay_alpha = 0.5)
  opaque <- reference_render(reference_spec(), rep(4, 4))
  expect_equal(reference_rgb(base), rep(128, 3))
  expect_lte(max(abs(reference_rgb(ref) - 255 * c(0.3, 0.3, 0.7))), 0.5)
  expect_lte(max(abs(reference_rgb(overlap) - 255 * c(0.65, 0.15, 0.35))), 0.5)
  expect_equal(reference_rgb(opaque), c(255L, 0L, 0L))
  expect_identical(opaque$rgba, reference_render(values = rep(4, 4))$rgba)
  expect_identical(ref$provenance$composition_order,
                   c("anatomy", "reference", "effect"))
  expect_identical(ref$provenance$reference$provenance$source, "synthetic")
})

test_that("reference controls preserve baseline and quantitative buffers", {
  for (subdivision in c(0L, 2L)) for (aa in c(1L, 3L)) {
    baseline <- reference_render(values = c(NA, 2, 3, 4),
                                 subdivide = subdivision, antialias = aa)
    for (spec in list(reference_spec(visible = FALSE),
                      reference_spec(opacity = 0), reference_spec(rep(0, 4)))) {
      out <- reference_render(spec, c(NA, 2, 3, 4),
                              subdivide = subdivision, antialias = aa)
      expect_identical(out$rgba, baseline$rgba)
    }
    active <- reference_render(reference_spec(c(0, 1, 1, 0)), c(NA, 2, 3, 4),
                               subdivide = subdivision, antialias = aa)
    for (field in c("scalar", "depth", "overlay_alpha", "coverage",
                    "cortex_coverage")) {
      expect_identical(active[[field]], baseline[[field]], info = field)
    }
    expect_identical(active$provenance$limits, baseline$provenance$limits)
    expect_identical(active$provenance$threshold, baseline$provenance$threshold)
    expect_true(all(active$reference_coverage %in% ((0:(aa * aa)) / (aa * aa))))
  }
})

test_that("binary membership has an analytic boundary and respects cortex", {
  p <- rbind(c(0, 0, 1), c(8, 0, 1), c(0, 8, 1))
  args <- list(projected = p, faces = matrix(1:3, nrow = 1), values = rep(0, 3),
               anatomy = rep(0.5, 3), cortex_mask = rep(TRUE, 3),
               width = 9L, height = 9L, threshold = 1, tail = 0L,
               limits = c(0, 4), palette = cbind(c(0, 1), 0, 0),
               overlay_alpha = 1, alpha_ramp = 0, base_low = 0.4,
               base_high = 0.6, medial_wall_policy = 0L, background = c(1, 1, 1),
               supersample = 1L, return_buffers = TRUE,
               reference_mask = c(0, 1, 0), reference_rgba = c(0, 0, 1, 0.4))
  out <- do.call(neurosurf:::cpp_rasterize_surface_scalar, args)
  expected <- outer(seq_len(9) - 0.5, seq_len(9) - 0.5,
                    function(y, x) x >= 4 & x + y <= 8) * 1
  expect_equal(out$reference_coverage, expected)
  for (policy in 0:1) {
    args$cortex_mask <- c(TRUE, FALSE, TRUE)
    args$medial_wall_policy <- policy
    masked <- do.call(neurosurf:::cpp_rasterize_surface_scalar, args)
    expect_equal(sum(masked$reference_coverage), 0)
  }
  display <- neurosurf:::.ns_subdivide_display_mesh(
    p, matrix(1:3, nrow = 1), rep(0, 3), rep(0.5, 3), rep(TRUE, 3), 2L,
    reference_spec(c(0, 1, 0))
  )
  # Linear field x/8 survives midpoint membership subdivision exactly.
  expect_equal(display$reference$mask, display$vertices[, 1] / 8)
})

test_that("invalid authored reference inputs fail before rasterization", {
  for (mask in list(c(0, 1), c(0, NA, 1, 0), c(0, Inf, 1, 0),
                    c(0, 0.5, 1, 0), c(-1, 0, 1, 0), letters[1:4])) {
    expect_error(reference_render(reference_spec(mask)), "binary")
  }
  for (color in list("not-a-color", "#FF000080", NA_character_, c("red", "blue"))) {
    expect_error(reference_render(reference_spec(color = color)), "opaque color")
  }
  expect_error(reference_render(reference_spec(opacity = 2)), "opacity")
  expect_error(reference_render(reference_spec(visible = NA)), "visible")
})

test_that("scene roles preserve geometry, provenance and exclusive map selection", {
  ref <- surface_layer("psl", c(0, 1, 1, 0), role = "reference", color = "blue",
                       opacity = 0.2, provenance = list(source = "synthetic"))
  make <- function(layers = list(ref, surface_layer("effect", 1:4)), ...) {
    surface_scene(left = scene_test_geometry(), layers = layers,
                  fallback = "Reference below effect", alt_text = "Synthetic", ...)
  }
  scene <- make()
  expect_identical(scene@selected_layer, "effect")
  invalid <- scene
  invalid@selected_layer <- "psl"
  expect_error(methods::validObject(invalid), "quantitative")
  expect_error(surface_scene_manifest(invalid), "quantitative")
  expect_error(make(selected_layer = "psl"), "quantitative")
  expect_error(make(layers = ref), "at least one quantitative")
  ref2 <- ref; ref2$name <- "second"
  expect_error(make(layers = list(ref, ref2, surface_layer("effect", 1:4))),
               "at most one reference")
  expect_error(make(layers = list(surface_layer("bad", c(0, 1, 2, 0),
                                                role = "reference", color = "red"),
                                  surface_layer("effect", 1:4))), "binary")
  expect_error(surface_layer("bad", 1:4, role = "reference"), "opaque color")
  expect_error(surface_layer("bad", 1:4, role = "reference", color = "red",
                              threshold = c(0, 1)), "no limits")
  manifest <- surface_scene_manifest(scene)
  expect_identical(manifest$schemaVersion, "surfview.scene.reference.v1")
  expect_identical(manifest$layers$psl$role, "reference")
  expect_identical(manifest$layers$effect$role, "map")
  expect_identical(manifest$layers$psl$color, "#0000FF")
  expect_identical(manifest$layers$psl$provenance$source, "synthetic")
  expect_length(manifest$geometries, 1L)
  expect_length(Filter(function(a) a$role == "vertices", manifest$assets), 1L)
  directory <- tempfile(); dir.create(directory)
  bundled <- surface_scene_manifest(scene, "directory", directory)
  expect_identical(names(manifest$assets), names(bundled$assets))
  for (id in names(manifest$assets)) {
    expect_identical(base64enc::base64decode(manifest$assets[[id]]$data),
                     readBin(file.path(directory, bundled$assets[[id]]$uri),
                             "raw", n = bundled$assets[[id]]$byteLength))
  }
  json <- jsonlite::fromJSON(jsonlite::toJSON(unclass(manifest),
                                             auto_unbox = TRUE))
  expect_identical(json$layers$psl$role, "reference")
  expect_identical(json$selectedLayer, "effect")
})


test_that("lit references preserve scalar ramp, depth and medial-wall policies", {
  for (policy in c("shade", "mask", "outline")) {
    args <- list(values = c(-4, -1, 2, 4), lighting = TRUE, antialias = 3L,
                 subdivide = 1L, alpha_ramp = 2, threshold = 1,
                 cortex_mask = c(TRUE, TRUE, TRUE, FALSE), medial_wall = policy)
    base <- do.call(reference_render, args)
    out <- do.call(reference_render, c(list(reference = reference_spec()), args))
    for (field in c("scalar", "depth", "overlay_alpha", "coverage")) {
      expect_identical(out[[field]], base[[field]], info = field)
    }
    expect_equal(sum(out$reference_coverage[!out$cortex_coverage]), 0)
    hidden <- do.call(reference_render,
                      c(list(reference = reference_spec(visible = FALSE)), args))
    expect_identical(hidden$rgba, base$rgba)
  }
})
