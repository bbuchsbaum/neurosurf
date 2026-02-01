skip_if_not_installed("rgl")

make_surface_set <- function() {
  verts <- matrix(c(0, 0, 0,
                    1, 0, 0,
                    0, 1, 0,
                    0, 0, 1), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L,
                    0L, 1L, 3L,
                    0L, 2L, 3L,
                    1L, 2L, 3L), ncol = 3, byrow = TRUE)

  g1 <- SurfaceGeometry(verts, faces, hemi = "lh", label = "pial")
  g2 <- SurfaceGeometry(verts + 0.1, faces, hemi = "lh", label = "inflated")
  surface_set(pial = g1, inflated = g2, default_label = "pial")
}

test_that("resolve_surface_geometry unwraps SurfaceSet defaults and labels", {
  ss <- make_surface_set()

  expect_identical(neurosurf:::resolve_surface_geometry(ss), ss@surfaces$pial)
  expect_identical(neurosurf:::resolve_surface_geometry(ss, "inflated"), ss@surfaces$inflated)
})

test_that("NeuroSurface and NeuroSurfaceVector accept SurfaceSet geometries", {
  ss <- make_surface_set()
  idx <- as.integer(seq_len(4))

  ns <- NeuroSurface(ss, indices = idx, data = as.numeric(idx))
  expect_s4_class(ns, "NeuroSurface")
  expect_identical(geometry(ns), ss)

  nsv <- NeuroSurfaceVector(ss, indices = idx, mat = matrix(idx, ncol = 1))
  expect_s4_class(nsv, "NeuroSurfaceVector")
  expect_identical(geometry(nsv), ss)

  resolved <- neurosurf:::resolve_surface_geometry(geometry(ns))
  expect_equal(ncol(resolved@mesh$vb), length(idx))
})

test_that("NeuroSurface validation rejects non-surface-like geometry", {
  expect_error(NeuroSurface(list(), indices = 1L, data = 1), "SurfaceGeometry or SurfaceSet")
})

test_that("surface_set constructor works with named arguments", {
  verts <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3, byrow = TRUE)

  g1 <- SurfaceGeometry(verts, faces, hemi = "lh")
  g2 <- SurfaceGeometry(verts + 0.5, faces, hemi = "lh")

  ss <- surface_set(white = g1, pial = g2, default_label = "white")

  expect_s4_class(ss, "SurfaceSet")
  expect_equal(ss@default_label, "white")
  expect_equal(names(ss@surfaces), c("white", "pial"))
})

test_that("surface_set errors on no surfaces", {
  expect_error(surface_set(), "supply named geometries")
})

test_that("surface_set errors on invalid default_label", {
  verts <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3, byrow = TRUE)
  g1 <- SurfaceGeometry(verts, faces, hemi = "lh")

  expect_error(
    surface_set(white = g1, default_label = "invalid"),
    "default_label"
  )
})

test_that("get_surface retrieves surface by label", {
  ss <- make_surface_set()

  pial <- get_surface(ss, "pial")
  inflated <- get_surface(ss, "inflated")

  expect_s4_class(pial, "SurfaceGeometry")
  expect_s4_class(inflated, "SurfaceGeometry")
})

test_that("get_surface returns default when no label", {
  ss <- make_surface_set()

  default_surf <- get_surface(ss)
  pial <- get_surface(ss, "pial")

  expect_equal(vertices(default_surf), vertices(pial))
})

test_that("get_surface errors on invalid label", {
  ss <- make_surface_set()

  expect_error(get_surface(ss, "nonexistent"), "not found")
})

test_that("surface_labels returns all labels", {
  ss <- make_surface_set()

  labels <- surface_labels(ss)

  expect_type(labels, "character")
  expect_true("pial" %in% labels)
  expect_true("inflated" %in% labels)
})

test_that("geometry method returns default surface", {
  ss <- make_surface_set()

  geom <- geometry(ss)

  expect_s4_class(geom, "SurfaceGeometry")
})

test_that("vertices method delegates to default surface", {
  ss <- make_surface_set()

  v <- vertices(ss)
  v_default <- vertices(get_surface(ss))

  expect_equal(v, v_default)
})

test_that("faces method delegates to default surface", {
  ss <- make_surface_set()

  f <- faces(ss)
  f_default <- faces(get_surface(ss))

  expect_equal(f, f_default)
})

test_that("nodes method delegates to default surface", {
  ss <- make_surface_set()

  n <- nodes(ss)
  n_default <- nodes(get_surface(ss))

  expect_equal(n, n_default)
})

test_that("graph method delegates to default surface", {
  ss <- make_surface_set()

  g <- graph(ss)

  expect_s3_class(g, "igraph")
})

test_that("curvature method delegates to default surface", {
  ss <- make_surface_set()

  curv <- curvature(ss)

  expect_type(curv, "double")
  expect_equal(length(curv), 4)  # 4 vertices
})
