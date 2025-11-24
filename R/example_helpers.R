#' Internal helper: tiny example SurfaceGeometry
#'
#' @keywords internal
#' @return A \code{SurfaceGeometry} tetrahedron useful for examples/tests.
example_surface_geometry <- function() {
  verts <- matrix(c(
    0, 0, 0,
    1, 0, 0,
    0, 1, 0,
    0, 0, 1
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(
    0L, 1L, 2L,
    0L, 1L, 3L,
    0L, 2L, 3L,
    1L, 2L, 3L
  ), ncol = 3, byrow = TRUE)
  SurfaceGeometry(verts, faces, hemi = "lh")
}
