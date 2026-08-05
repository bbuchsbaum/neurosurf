scene_test_geometry <- function(hemi = "lh", offset = 0) {
  vertices <- matrix(
    c(0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1) + offset,
    ncol = 3,
    byrow = TRUE
  )
  triangles <- matrix(
    c(0L, 1L, 2L, 0L, 1L, 3L, 0L, 2L, 3L, 1L, 2L, 3L),
    ncol = 3,
    byrow = TRUE
  )
  SurfaceGeometry(vertices, triangles, hemi = hemi)
}
