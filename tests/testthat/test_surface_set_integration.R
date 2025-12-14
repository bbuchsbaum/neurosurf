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
