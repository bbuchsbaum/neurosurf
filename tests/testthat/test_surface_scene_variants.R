test_that("surface scenes carry alternative embeddings into the manifest", {
  geometry <- example_surface_geometry()
  xyz <- coords(geometry)
  scene <- surface_scene(
    left = geometry,
    layers = surface_layer("effect", seq_len(nrow(xyz))),
    variants = list(pial = xyz * 1.1, white = list(left = xyz * 0.9)),
    variant = "inflated",
    fallback = "Static figure.", alt_text = "Example surface."
  )
  expect_identical(scene@variant, "inflated")
  expect_identical(names(scene@variants), c("pial", "white"))
  manifest <- surface_scene_manifest(scene)
  variants <- manifest$geometries$left$variants
  expect_identical(names(variants), c("inflated", "pial", "white"))
  expect_identical(variants$inflated, manifest$geometries$left$vertices)
  for (id in unlist(variants)) {
    expect_identical(manifest$assets[[id]]$role, "vertices")
    expect_identical(as.integer(manifest$assets[[id]]$shape), c(nrow(xyz), 3L))
  }
  pial <- readBin(base64enc::base64decode(manifest$assets[[variants$pial]]$data),
                  "numeric", n = length(xyz), size = 4, endian = "little")
  expect_equal(matrix(pial, ncol = 3, byrow = TRUE), unname(xyz) * 1.1,
               tolerance = 1e-6)
})

test_that("variant input is validated", {
  geometry <- example_surface_geometry()
  xyz <- coords(geometry)
  make <- function(...) surface_scene(
    left = geometry, layers = surface_layer("effect", seq_len(nrow(xyz))),
    fallback = "Static figure.", alt_text = "Example surface.", ...
  )
  expect_error(make(variants = list(pial = xyz[-1, ])), "finite coordinates")
  expect_error(make(variants = list(default = xyz)), "must not repeat")
  expect_error(make(variants = list(`bad name` = xyz)), "variant names")
  expect_null(surface_scene_manifest(make())$geometries$left$variants)
})

test_that("magnitude opacity is validated and serialized for maps only", {
  layer <- surface_layer("effect", 1:4, magnitude_opacity = list(low = 1, gamma = 2))
  expect_identical(layer$magnitude_opacity,
                   list(low = 1, high = "range", gamma = 2, floor = 0))
  expect_error(surface_layer("effect", 1:4, magnitude_opacity = list(gamma = 0)),
               "out of range")
  expect_error(surface_layer("effect", 1:4, magnitude_opacity = list(lo = 1)),
               "unknown")
  expect_error(surface_layer("effect", 1:4, magnitude_opacity = list(low = 2, high = 1)),
               "out of range")
  geometry <- example_surface_geometry()
  n <- nrow(coords(geometry))
  scene <- surface_scene(
    left = geometry,
    layers = surface_layer("effect", seq_len(n), magnitude_opacity = list(floor = 0.6)),
    fallback = "Static figure.", alt_text = "Example surface."
  )
  json <- jsonlite::fromJSON(jsonlite::toJSON(unclass(surface_scene_manifest(scene)),
                                              auto_unbox = TRUE))
  expect_identical(json$layers$effect$magnitudeOpacity$high, "range")
  expect_identical(json$layers$effect$magnitudeOpacity$floor, 0.6)
})

test_that("scene curvature is written in the runtime's FreeSurfer polarity", {
  geometry <- example_surface_geometry()
  n <- nrow(coords(geometry))
  light <- seq(-0.5, 0.5, length.out = n)
  scene <- surface_scene(
    left = geometry, layers = surface_layer("effect", seq_len(n)),
    curvature = light, fallback = "Static figure.", alt_text = "Example surface."
  )
  manifest <- surface_scene_manifest(scene)
  asset <- manifest$assets[[manifest$geometries$left$curvature]]
  written <- readBin(base64enc::base64decode(asset$data), "numeric", n = n,
                     size = 4, endian = "little")
  expect_equal(written, -light, tolerance = 1e-6)
})

test_that("render_surface_rgba fades overlays by magnitude", {
  geometry <- example_surface_geometry()
  n <- nrow(coords(geometry))
  values <- seq(-3, 3, length.out = n)
  base <- render_surface_rgba(geometry, values, width = 120L, height = 90L,
                              limits = c(-3, 3), antialias = 1L)
  faded <- render_surface_rgba(geometry, values, width = 120L, height = 90L,
                               limits = c(-3, 3), antialias = 1L,
                               magnitude_opacity = list(low = 0))
  expect_identical(dim(base$rgba), dim(faded$rgba))
  # Fading moves overlay pixels toward the grey anatomy, never away from it.
  expect_false(identical(base$rgba, faded$rgba))
  expect_equal(.ns_magnitude_alpha(c(0, 1.5, -3, NA),
                                   .scene_magnitude_opacity(list()), c(-3, 3)),
               c(0, 0.5, 1, 0))
})
