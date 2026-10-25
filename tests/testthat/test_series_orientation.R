# Regression tests for #93: series() orientation must not depend on whether
# the index is stored as integer or double.

surf_geom_std8 <- function() {
  read_surf_geometry(system.file("extdata", "std.8_lh.inflated.asc",
                                 package = "neurosurf"))
}

make_sv <- function(nt = 4) {
  geom <- surf_geom_std8()
  n <- nrow(coords(geom))
  set.seed(93)
  NeuroSurfaceVector(geom, seq_len(n), matrix(rnorm(n * nt), n, nt))
}

test_that("series() on NeuroSurfaceVector returns T x 1 for a single integer index (#93)", {
  sv <- make_sv(4)
  expect_equal(dim(series(sv, 2L)), c(4L, 1L))
  expect_equal(dim(series(sv, 2)), c(4L, 1L))
  expect_equal(as.matrix(series(sv, 2L)), as.matrix(series(sv, 2)))
  expect_equal(as.vector(as.matrix(series(sv, 2L))),
               as.vector(as.matrix(sv@data[2, , drop = FALSE])))
})

test_that("series() on NeuroSurfaceVector is T x length(i) for integer and double indices (#93)", {
  sv <- make_sv(4)
  for (idx in list(2L, 2, 1:3, c(1, 2, 3), c(5L, 1L))) {
    res <- series(sv, idx)
    expect_equal(dim(res), c(4L, length(idx)), info = paste(class(idx), deparse(idx)))
    expect_equal(as.matrix(res), as.matrix(series(sv, as.numeric(idx))),
                 info = paste(class(idx), deparse(idx)))
  }
})

test_that("series_roi() on NeuroSurfaceVector accepts a single integer index (#93)", {
  sv <- make_sv(4)
  roi_i <- series_roi(sv, 2L)
  roi_d <- series_roi(sv, 2)
  expect_s4_class(roi_i, "ROISurfaceVector")
  expect_equal(dim(roi_i@data), c(4L, 1L))
  expect_equal(roi_i@data, roi_d@data)
})

test_that("series() on NeuroSurface is 1 x length(i) for integer and double indices (#93)", {
  geom <- surf_geom_std8()
  n <- nrow(coords(geom))
  s <- NeuroSurface(geom, indices = seq_len(n), data = as.numeric(seq_len(n)))
  expect_equal(dim(series(s, 2L)), c(1L, 1L))
  expect_equal(dim(series(s, 1:3)), c(1L, 3L))
  expect_equal(series(s, 1:3), series(s, c(1, 2, 3)))
})

test_that("neighbor_graph() accepts double-stored node indices like integer ones (#93)", {
  geom <- example_surface_geometry()
  ew <- igraph::E(graph(geom))$dist
  g_int <- neighbor_graph(geom, radius = 2, nodes = 1:2)
  g_dbl <- neighbor_graph(geom, radius = 2, nodes = c(1, 2))
  expect_s3_class(g_dbl, "igraph")
  expect_equal(igraph::as_edgelist(g_dbl), igraph::as_edgelist(g_int))
  g_int_w <- neighbor_graph(geom, radius = 2, edgeWeights = ew, nodes = 1:2)
  g_dbl_w <- neighbor_graph(geom, radius = 2, edgeWeights = ew, nodes = c(1, 2))
  expect_equal(igraph::as_edgelist(g_dbl_w), igraph::as_edgelist(g_int_w))
})
