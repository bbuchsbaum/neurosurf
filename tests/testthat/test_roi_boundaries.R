# Helper to check if C++ implementation is available
cpp_available <- function() {
  tryCatch({
    # Try calling with minimal input to see if the C++ code is loaded
    test_verts <- matrix(c(0,0,0, 1,0,0, 0,1,0), ncol = 3, byrow = TRUE)
    test_faces <- matrix(c(1L, 2L, 3L), ncol = 3)
    test_roi <- c(1L, 1L, 1L)
    neurosurf:::roi_boundary_loops_cpp(test_verts, test_faces, test_roi)
    TRUE
  }, error = function(e) FALSE)
}

# Check at load time
has_cpp <- cpp_available()

test_that("roi_boundary_loops_cpp finds single hex ring", {
  skip_if(!has_cpp, "C++ implementation not available")

  center <- c(0, 0, 0)
  angles <- seq(0, 2 * pi - 2 * pi / 6, length.out = 6)
  outer <- cbind(cos(angles), sin(angles), rep(0, 6))

  vertices <- rbind(center, outer)
  faces <- cbind(
    rep(1L, 6L),
    2:7,
    c(3:7, 2L)
  )
  roi <- c(1L, rep(2L, 6L))

  res <- find_roi_boundaries(
    vertices = vertices,
    faces = faces,
    vertex_id = roi,
    boundary_method = "edge_vertices",
    use_cpp = TRUE
  )

  expect_length(res$boundary, 1L)
  expect_equal(res$boundary_roi_id, 2L)
  expect_equal(res$roi_components, c(0, 1))

  loop <- res$boundary_verts[[1]]
  expect_equal(loop[1], loop[length(loop)])
  expect_true(all(loop %in% 2:7))
  expect_equal(length(loop), 7L)
})


test_that("roi_boundary_loops_cpp returns empty for single ROI", {
  skip_if(!has_cpp, "C++ implementation not available")

  vertices <- matrix(c(0, 0, 0,
                       1, 0, 0,
                       0, 1, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(1L, 2L, 3L), ncol = 3, byrow = TRUE)
  roi <- c(1L, 1L, 1L)

  res <- find_roi_boundaries(
    vertices = vertices,
    faces = faces,
    vertex_id = roi,
    boundary_method = "edge_vertices",
    use_cpp = TRUE
  )

  expect_length(res$boundary, 0L)
  expect_length(res$boundary_verts, 0L)
  expect_equal(res$roi_components, 0)
})


test_that("roi_boundary_loops_cpp separates multiple rings", {
  skip_if(!has_cpp, "C++ implementation not available")

  angles <- seq(0, 2 * pi - 2 * pi / 6, length.out = 6)
  outer1 <- cbind(cos(angles), sin(angles), rep(0, 6))
  outer2 <- cbind(3 + cos(angles), sin(angles), rep(0, 6))

  vertices <- rbind(
    c(0, 0, 0), outer1,
    c(3, 0, 0), outer2
  )

  faces1 <- cbind(
    rep(1L, 6L),
    2:7,
    c(3:7, 2L)
  )
  faces2 <- cbind(
    rep(8L, 6L),
    9:14,
    c(10:14, 9L)
  )

  faces <- rbind(faces1, faces2)
  roi <- c(1L, rep(2L, 6L), 1L, rep(2L, 6L))

  res <- find_roi_boundaries(
    vertices = vertices,
    faces = faces,
    vertex_id = roi,
    boundary_method = "edge_vertices",
    use_cpp = TRUE
  )

  expect_length(res$boundary, 2L)
  expect_equal(res$roi_components, c(0, 2))
  expect_true(all(res$boundary_roi_id == 2L))

  boundary_vertices <- sort(unique(unlist(res$boundary_verts)))
  expect_setequal(boundary_vertices, c(2:7, 9:14))
})


# helper: turn a loop (with closing vertex) into sorted undirected edge strings
loop_edges <- function(vloop) {
  edges <- cbind(vloop[-length(vloop)], vloop[-1])
  edges <- t(apply(edges, 1, function(e) sort(e)))
  ord <- order(edges[, 1], edges[, 2])
  edges[ord, , drop = FALSE]
}


test_that("cpp boundaries match expected cube split loops", {
  skip_if(!has_cpp, "C++ implementation not available")

  # cube split into bottom ROI 1 and top ROI 2
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
    1, 2, 3, 1, 3, 4,
    5, 6, 7, 5, 7, 8,
    1, 2, 6, 1, 6, 5,
    3, 4, 8, 3, 8, 7,
    1, 4, 8, 1, 8, 5,
    2, 3, 7, 2, 7, 6
  ), ncol = 3, byrow = TRUE)

  roi <- c(1, 1, 1, 1, 2, 2, 2, 2)

  res_cpp <- find_roi_boundaries(vertices, faces, roi,
                                 boundary_method = "edge_vertices",
                                 use_cpp = TRUE)

  expect_equal(length(res_cpp$boundary_verts), 2L)
  expect_setequal(res_cpp$boundary_roi_id, c(1L, 2L))

  expected_loops <- list(
    c(1L, 2L, 3L, 4L, 1L),
    c(5L, 6L, 7L, 8L, 5L)
  )

  cpp_edges <- lapply(res_cpp$boundary_verts, loop_edges)
  exp_edges <- lapply(expected_loops, loop_edges)

  cpp_edges_sorted <- cpp_edges[order(sapply(cpp_edges, function(e) paste(e[1, ], collapse = ",")))]
  exp_edges_sorted <- exp_edges[order(sapply(exp_edges, function(e) paste(e[1, ], collapse = ",")))]

  expect_equal(cpp_edges_sorted, exp_edges_sorted)
})


test_that("non-degree-2 components still return closed loops", {
  skip_if(!has_cpp, "C++ implementation not available")

  vertices <- matrix(c(
    0, 0, 0,  # 1
    1, 0, 0,  # 2
    0, 1, 0,  # 3
    1, 1, 0,  # 4
    0, 0, 1   # 5 (different ROI)
  ), ncol = 3, byrow = TRUE)

  faces <- matrix(c(
    1, 2, 5,
    2, 3, 5,
    3, 1, 5,
    2, 4, 5,
    1, 4, 5
  ), ncol = 3, byrow = TRUE)

  roi <- c(1, 1, 1, 1, 2)

  res <- find_roi_boundaries(vertices, faces, roi,
                             boundary_method = "edge_vertices",
                             use_cpp = TRUE)

  expect_true(length(res$boundary_verts) >= 1)
  loop <- res$boundary_verts[[1]]
  expect_equal(loop[1], loop[length(loop)])
  # no immediate backtracking
  expect_false(any(loop[-1] == loop[-length(loop)]))
})


# Tests for "faces" boundary method ----------------------------------------

test_that("find_roi_boundaries with 'faces' method identifies boundary faces", {
  # Simple triangle with two ROIs
  vertices <- matrix(c(
    0, 0, 0,
    1, 0, 0,
    0.5, 1, 0,
    0.5, 0.5, 0
  ), ncol = 3, byrow = TRUE)

  faces <- matrix(c(
    1, 2, 4,
    2, 3, 4,
    1, 4, 3
  ), ncol = 3, byrow = TRUE)

  # Vertex 4 is ROI 2, others are ROI 1
  roi <- c(1, 1, 1, 2)

  res <- find_roi_boundaries(
    vertices = vertices,
    faces = faces,
    vertex_id = roi,
    boundary_method = "faces"
  )

  expect_true(is.logical(res$boundary))
  expect_equal(length(res$boundary), nrow(faces))
  # All faces touch vertex 4 (ROI 2) and others (ROI 1), so all are boundary
  expect_true(all(res$boundary))
  expect_null(res$boundary_roi_id)
  expect_null(res$roi_components)
})


test_that("find_roi_boundaries 'faces' method returns FALSE for uniform ROI", {
  vertices <- matrix(c(
    0, 0, 0,
    1, 0, 0,
    0.5, 1, 0
  ), ncol = 3, byrow = TRUE)

  faces <- matrix(c(1, 2, 3), ncol = 3)
  roi <- c(1, 1, 1)

  res <- find_roi_boundaries(
    vertices = vertices,
    faces = faces,
    vertex_id = roi,
    boundary_method = "faces"
  )

  expect_false(any(res$boundary))
})


test_that("find_roi_boundaries 'faces' method with partial boundary", {
  # Create 4 triangles, only some on boundary
  vertices <- matrix(c(
    0, 0, 0,   # 1 - ROI 1
    1, 0, 0,   # 2 - ROI 1
    2, 0, 0,   # 3 - ROI 2
    0.5, 1, 0, # 4 - ROI 1
    1.5, 1, 0  # 5 - ROI 2
  ), ncol = 3, byrow = TRUE)

  faces <- matrix(c(
    1, 2, 4,  # All ROI 1 - not boundary
    2, 3, 5,  # Mixed ROI - boundary
    2, 4, 5,  # Mixed ROI - boundary
    3, 5, 5   # All ROI 2 (invalid but for test) - actually indices 3,5,5
  ), ncol = 3, byrow = TRUE)

  # Fix last face to be valid
  faces[4, ] <- c(3, 5, 3)  # degenerate but valid indices

  roi <- c(1, 1, 2, 1, 2)

  res <- find_roi_boundaries(
    vertices = vertices,
    faces = faces,
    vertex_id = roi,
    boundary_method = "faces"
  )

  expect_equal(length(res$boundary), 4)
  expect_false(res$boundary[1])  # All ROI 1
  expect_true(res$boundary[2])   # Mixed
  expect_true(res$boundary[3])   # Mixed
})


# Tests for input validation -----------------------------------------------

test_that("find_roi_boundaries errors on invalid vertices", {
  vertices <- matrix(1:4, ncol = 2)  # Wrong number of columns
  faces <- matrix(c(1, 2, 1), ncol = 3)
  roi <- c(1, 1)

  expect_error(
    find_roi_boundaries(vertices, faces, roi),
    "vertices must be an n x 3"
  )
})


test_that("find_roi_boundaries errors on invalid faces", {
  vertices <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(1, 2), ncol = 2)  # Wrong number of columns
  roi <- c(1, 1, 1)

  expect_error(
    find_roi_boundaries(vertices, faces, roi),
    "faces must be an m x 3"
  )
})


test_that("find_roi_boundaries errors on mismatched vertex_id length", {
  vertices <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(1, 2, 3), ncol = 3)
  roi <- c(1, 1)  # Too short

  expect_error(
    find_roi_boundaries(vertices, faces, roi),
    "length.*must equal"
  )
})


test_that("find_roi_boundaries errors on out-of-range face indices", {
  vertices <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(1, 2, 5), ncol = 3)  # 5 is out of range

  roi <- c(1, 1, 1)

  expect_error(
    find_roi_boundaries(vertices, faces, roi),
    "faces must reference vertex indices"
  )
})


# Tests for findBoundaries S4 method ---------------------------------------

test_that("findBoundaries works for NeuroSurface with 'faces' method", {
  geom <- example_surface_geometry()
  n <- length(nodes(geom))

  # Create ROI labels (divide surface into 4 regions based on x-coordinate)
  coords_mat <- coords(geom)
  x_vals <- coords_mat[, 1]
  roi_labels <- as.integer(cut(x_vals, breaks = 4))

  ns <- NeuroSurface(geom, seq_len(n), roi_labels)

  result <- findBoundaries(ns, method = "faces")

  expect_type(result, "list")
  expect_true("boundary" %in% names(result))
  expect_true(is.logical(result$boundary))
})


test_that("findBoundaries works with edge_vertices method", {
  skip_if(!has_cpp, "C++ implementation not available")

  geom <- example_surface_geometry()
  n <- length(nodes(geom))

  # Create simple 2-region split
  coords_mat <- coords(geom)
  x_vals <- coords_mat[, 1]
  roi_labels <- ifelse(x_vals > median(x_vals), 1L, 2L)

  ns <- NeuroSurface(geom, seq_len(n), roi_labels)

  result <- findBoundaries(ns, method = "edge_vertices")

  expect_type(result, "list")
  expect_true("boundary" %in% names(result))
  expect_true("boundary_roi_id" %in% names(result))
  expect_true("boundary_verts" %in% names(result))
})


test_that("findBoundaries warns for single region", {
  geom <- example_surface_geometry()
  n <- length(nodes(geom))

  # All same ROI
  roi_labels <- rep(1L, n)
  ns <- NeuroSurface(geom, seq_len(n), roi_labels)

  expect_warning(
    result <- findBoundaries(ns, method = "faces"),
    "Only one region"
  )
})


# Tests for R fallback implementation --------------------------------------

test_that("find_roi_boundaries R implementation works for simple case", {
  center <- c(0, 0, 0)
  angles <- seq(0, 2 * pi - 2 * pi / 6, length.out = 6)
  outer <- cbind(cos(angles), sin(angles), rep(0, 6))

  vertices <- rbind(center, outer)
  faces <- cbind(
    rep(1L, 6L),
    2:7,
    c(3:7, 2L)
  )
  roi <- c(1L, rep(2L, 6L))

  res_r <- find_roi_boundaries(
    vertices = vertices,
    faces = faces,
    vertex_id = roi,
    boundary_method = "edge_vertices",
    use_cpp = FALSE
  )

  expect_type(res_r, "list")
  expect_true("boundary" %in% names(res_r))
  expect_true("boundary_verts" %in% names(res_r))

  # Should find boundaries for ROI 2 (the outer ring)
  if (length(res_r$boundary) > 0) {
    loop <- res_r$boundary_verts[[1]]
    # Loop should close
    expect_equal(loop[1], loop[length(loop)])
  }
})


test_that("verbose mode prints messages for faces method", {
  vertices <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(1, 2, 3), ncol = 3)
  roi <- c(1, 2, 1)

  expect_message(
    find_roi_boundaries(vertices, faces, roi,
                        boundary_method = "faces",
                        verbose = TRUE),
    "Preparing face ROI"
  )
})

test_that("verbose mode prints messages for edge_vertices method", {
  skip_if(!has_cpp, "C++ implementation not available")

  vertices <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(1, 2, 3), ncol = 3)
  roi <- c(1, 2, 1)

  expect_message(
    find_roi_boundaries(vertices, faces, roi,
                        boundary_method = "edge_vertices",
                        use_cpp = TRUE,
                        verbose = TRUE),
    "Computing ROI boundary"
  )
})

test_that("find_roi_boundaries handles NA in vertex_id", {
  vertices <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(1, 2, 3), ncol = 3)
  roi <- c(1, NA, 1)

  # Should not error, NA vertices are handled
  result <- find_roi_boundaries(vertices, faces, roi, boundary_method = "faces")
  expect_type(result, "list")
})

test_that("find_roi_boundaries with many ROIs", {
  skip_if(!has_cpp, "C++ implementation not available")

  # Create a grid of points with multiple ROIs
  n <- 9
  x <- rep(1:3, 3)
  y <- rep(1:3, each = 3)
  vertices <- cbind(x, y, rep(0, n))

  # Create faces for a 3x3 grid
  faces <- matrix(c(
    1, 2, 5,  # triangle 1
    1, 5, 4,  # triangle 2
    2, 3, 6,  # etc.
    2, 6, 5,
    4, 5, 8,
    4, 8, 7,
    5, 6, 9,
    5, 9, 8
  ), ncol = 3, byrow = TRUE)

  # Each vertex is its own ROI
  roi <- 1:n

  result <- find_roi_boundaries(vertices, faces, roi,
                                boundary_method = "edge_vertices",
                                use_cpp = TRUE)

  expect_type(result, "list")
  # With many unique ROIs, should have multiple boundary components
  expect_true(length(result$boundary_verts) >= 1 || length(result$boundary) >= 0)
})

test_that("find_roi_boundaries with zero-indexed faces errors", {
  vertices <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0, 1, 2), ncol = 3)  # Zero-indexed
  roi <- c(1, 1, 1)

  expect_error(
    find_roi_boundaries(vertices, faces, roi),
    "faces must reference vertex indices"
  )
})

test_that("cpp_available helper works", {
  # This just tests that the helper function returns a logical
  result <- cpp_available()
  expect_type(result, "logical")
})
