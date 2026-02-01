# Tests for all_generic.R - generic function definitions

# Test that all generics are properly defined
test_that("vertices generic is defined", {
  expect_true(isGeneric("vertices"))
})

test_that("faces generic is defined", {
  expect_true(isGeneric("faces"))
})

test_that("nodes generic is defined", {
  expect_true(isGeneric("nodes"))
})

test_that("geometry generic is defined", {
  expect_true(isGeneric("geometry"))
})

test_that("graph generic is defined", {
  expect_true(isGeneric("graph"))
})

test_that("smooth generic is defined", {
  expect_true(isGeneric("smooth"))
})

test_that("laplacian generic is defined", {
  expect_true(isGeneric("laplacian"))
})

test_that("adjacency generic is defined", {
  expect_true(isGeneric("adjacency"))
})

test_that("curvature generic is defined", {
  expect_true(isGeneric("curvature"))
})

test_that("left generic is defined", {
  expect_true(isGeneric("left"))
})

test_that("right generic is defined", {
  expect_true(isGeneric("right"))
})

test_that("cluster_threshold generic is defined", {
  expect_true(isGeneric("cluster_threshold"))
})

test_that("plot_js generic is defined", {
  expect_true(isGeneric("plot_js"))
})

test_that("surfwidget generic is defined", {
  expect_true(isGeneric("surfwidget"))
})

test_that("findBoundaries generic is defined", {
  expect_true(isGeneric("findBoundaries"))
})

test_that("surf_to_world generic is defined", {
  expect_true(isGeneric("surf_to_world"))
})

test_that("neighbor_graph generic is defined", {
  expect_true(isGeneric("neighbor_graph"))
})

# Test methods exist for SurfaceGeometry
test_that("vertices has method for SurfaceGeometry", {
  expect_true(hasMethod("vertices", "SurfaceGeometry"))
})

test_that("faces has method for SurfaceGeometry", {
  expect_true(hasMethod("faces", "SurfaceGeometry"))
})

test_that("nodes has method for SurfaceGeometry", {
  expect_true(hasMethod("nodes", "SurfaceGeometry"))
})

test_that("graph has method for SurfaceGeometry", {
  expect_true(hasMethod("graph", "SurfaceGeometry"))
})

test_that("curvature has method for SurfaceGeometry", {
  expect_true(hasMethod("curvature", "SurfaceGeometry"))
})

test_that("surf_to_world has method for SurfaceGeometry", {
  expect_true(hasMethod("surf_to_world", "SurfaceGeometry"))
})

test_that("plot_js has method for SurfaceGeometry", {
  expect_true(hasMethod("plot_js", "SurfaceGeometry"))
})

# Test methods for NeuroSurface
test_that("vertices has method for NeuroSurface", {
  expect_true(hasMethod("vertices", "NeuroSurface"))
})

test_that("faces has method for NeuroSurface", {
  expect_true(hasMethod("faces", "NeuroSurface"))
})

test_that("geometry has method for NeuroSurface", {
  expect_true(hasMethod("geometry", "NeuroSurface"))
})

test_that("nodes has method for NeuroSurface", {
  expect_true(hasMethod("nodes", "NeuroSurface"))
})

test_that("graph has method for NeuroSurface", {
  expect_true(hasMethod("graph", "NeuroSurface"))
})

test_that("cluster_threshold has method for NeuroSurface", {
  expect_true(hasMethod("cluster_threshold", "NeuroSurface"))
})

# Test methods for NeuroSurfaceVector
test_that("vertices has method for NeuroSurfaceVector", {
  expect_true(hasMethod("vertices", "NeuroSurfaceVector"))
})

test_that("faces has method for NeuroSurfaceVector", {
  expect_true(hasMethod("faces", "NeuroSurfaceVector"))
})

test_that("geometry has method for NeuroSurfaceVector", {
  expect_true(hasMethod("geometry", "NeuroSurfaceVector"))
})

# Test methods for SurfaceSet
test_that("vertices has method for SurfaceSet", {
  expect_true(hasMethod("vertices", "SurfaceSet"))
})

test_that("faces has method for SurfaceSet", {
  expect_true(hasMethod("faces", "SurfaceSet"))
})

test_that("nodes has method for SurfaceSet", {
  expect_true(hasMethod("nodes", "SurfaceSet"))
})

test_that("graph has method for SurfaceSet", {
  expect_true(hasMethod("graph", "SurfaceSet"))
})

test_that("curvature has method for SurfaceSet", {
  expect_true(hasMethod("curvature", "SurfaceSet"))
})

# Test functional behavior of generics with example objects
test_that("vertices generic works on example geometry", {
  geom <- example_surface_geometry()
  v <- vertices(geom)
  expect_true(is.matrix(v))
  expect_equal(ncol(v), 3)
})

test_that("faces generic works on example geometry", {
  geom <- example_surface_geometry()
  f <- faces(geom)
  expect_true(is.matrix(f))
  expect_equal(ncol(f), 3)
})

test_that("nodes generic works on example geometry", {
  geom <- example_surface_geometry()
  n <- nodes(geom)
  expect_type(n, "integer")
})

test_that("graph generic works on example geometry", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  expect_s3_class(g, "igraph")
})

test_that("curvature generic works on example geometry", {
  geom <- example_surface_geometry()
  c <- curvature(geom)
  expect_type(c, "double")
  expect_equal(length(c), nrow(vertices(geom)))
})

test_that("surf_to_world generic works on example geometry", {
  geom <- example_surface_geometry()
  xform <- surf_to_world(geom)
  expect_true(is.matrix(xform))
  expect_equal(dim(xform), c(4, 4))
})

test_that("neighbor_graph generic works on example geometry", {
  geom <- example_surface_geometry()
  ng <- neighbor_graph(geom, radius = 1)
  expect_s3_class(ng, "igraph")
})

test_that("laplacian generic works on SurfaceGeometry", {
  geom <- example_surface_geometry()
  L <- laplacian(geom)
  expect_true(inherits(L, "Matrix") || is.matrix(L))
})

test_that("adjacency generic works on SurfaceGeometry", {
  geom <- example_surface_geometry()
  A <- adjacency(geom)
  expect_true(inherits(A, "Matrix") || is.matrix(A))
})
