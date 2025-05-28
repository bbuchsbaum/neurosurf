library(neurosurf)

create_cube <- function() {
  vertices <- matrix(c(
    0, 0, 0,
    1, 0, 0,
    1, 1, 0,
    0, 1, 0,
    0, 0, 1,
    1, 0, 1,
    1, 1, 1,
    0, 1, 1
  ), ncol = 3, byrow = TRUE)

  faces <- matrix(c(
    1, 2, 3,
    1, 3, 4,
    5, 6, 7,
    5, 7, 8,
    1, 2, 6,
    1, 6, 5,
    3, 4, 8,
    3, 8, 7,
    1, 4, 8,
    1, 8, 5,
    2, 3, 7,
    2, 7, 6
  ), ncol = 3, byrow = TRUE)

  list(vertices = vertices, faces = faces)
}

sort_coords <- function(m) {
  if (is.null(m)) return(m)
  m[do.call(order, as.data.frame(m)), , drop = FALSE]
}

vertex_id <- c(1, 1, 1, 1, 2, 2, 2, 2)
mesh <- create_cube()
verts <- mesh$vertices
faces <- mesh$faces

## Faces method ---------------------------------------------------------------

test_that("faces method identifies boundary faces", {
  res <- find_roi_boundaries(verts, faces, vertex_id, boundary_method = "faces")
  expected <- c(rep(FALSE, 4), rep(TRUE, 8))
  expect_equal(res$boundary, expected)
  expect_null(res$boundary_roi_id)
  expect_null(res$roi_components)
  expect_null(res$boundary_verts)
})

## Midpoint method ------------------------------------------------------------

test_that("midpoint method returns expected coordinates", {
  res <- find_roi_boundaries(verts, faces, vertex_id, boundary_method = "midpoint")
  expect_equal(length(res$boundary), 2)
  expect_equal(res$roi_components, c(1L, 1L))

  expected_mid <- matrix(c(
    0, 0, 0.5,
    1, 0, 0.5,
    1, 1, 0.5,
    0, 1, 0.5
  ), ncol = 3, byrow = TRUE)

  bc1 <- res$boundary[[1]]
  bc1_unique <- bc1[-nrow(bc1), , drop = FALSE]
  expect_equal(sort_coords(bc1_unique), sort_coords(expected_mid))
})

## Centroid method ------------------------------------------------------------

test_that("centroid method returns boundary loops", {
  res <- find_roi_boundaries(verts, faces, vertex_id, boundary_method = "centroid")
  expect_equal(length(res$boundary), 2)
  expect_equal(res$roi_components, c(1L, 1L))

  bc1 <- res$boundary[[1]]
  bc1_unique <- bc1[-nrow(bc1), , drop = FALSE]
  expect_equal(nrow(bc1_unique), 4)
  expect_true(all(bc1_unique[,3] > 0 & bc1_unique[,3] < 1))
})

## Edge vertices method -------------------------------------------------------

test_that("edge_vertices method follows mesh vertices", {
  res <- find_roi_boundaries(verts, faces, vertex_id, boundary_method = "edge_vertices")
  expect_equal(length(res$boundary), 2)
  expect_equal(res$roi_components, c(1L, 1L))

  expected_bottom <- matrix(c(
    0, 0, 0,
    1, 0, 0,
    1, 1, 0,
    0, 1, 0
  ), ncol = 3, byrow = TRUE)

  bc1 <- res$boundary[[1]]
  bc1_unique <- bc1[-nrow(bc1), , drop = FALSE]
  expect_equal(sort_coords(bc1_unique), sort_coords(expected_bottom))
})

## Edge faces method ----------------------------------------------------------

test_that("edge_faces method produces boundaries", {
  res <- find_roi_boundaries(verts, faces, vertex_id, boundary_method = "edge_faces")
  expect_equal(length(res$boundary), 2)
  expect_equal(res$roi_components, c(1L, 1L))

  bc1 <- res$boundary[[1]]
  bc1_unique <- bc1[-nrow(bc1), , drop = FALSE]
  expect_equal(nrow(bc1_unique), 4)
})

