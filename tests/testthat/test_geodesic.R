test_that("geodesic distance matrix matches expected on tetrahedron", {
  geom <- example_surface_geometry()

  gdm_dense <- geodesic_distance_matrix(
    geom,
    mode = "dense",
    chunk_size = 2,
    cache = FALSE
  )

  expect_equal(dim(gdm_dense), c(4, 4))
  expect_true(all(diag(gdm_dense) == 0))
  expect_equal(gdm_dense[1, 2], 1, tolerance = 1e-6)
  expect_equal(gdm_dense[2, 3], sqrt(2), tolerance = 1e-6)
  expect_equal(gdm_dense, t(gdm_dense))

  gdm_sparse <- geodesic_distance_matrix(
    geom,
    mode = "sparse",
    chunk_size = 2,
    cache = FALSE
  )
  expect_s4_class(gdm_sparse, "dgCMatrix")
  expect_equal(gdm_sparse[1, 4], 1, tolerance = 1e-6)
})


test_that("parcel centroid distances work for contiguous parcels", {
  geom <- example_surface_geometry()
  lsurf <- new(
    "LabeledNeuroSurface",
    geometry = geom,
    indices = 1:4,
    data = c(1, 1, 2, 2),
    labels = c("A", "B"),
    cols = c("#ff0000", "#00ff00")
  )

  cents <- parcel_geodesic_centroid(lsurf, cache = FALSE)
  expect_equal(nrow(cents), 2)

  pdm <- parcel_geodesic_distance_matrix(
    lsurf,
    metric = "centroid",
    cache = FALSE
  )
  expect_equal(dim(pdm), c(2, 2))
  expect_equal(pdm["1", "2"], 1, tolerance = 1e-6)
})


test_that("fragmented parcels are handled per policy", {
  verts <- matrix(c(
    0, 0, 0,
    1, 0, 0,
    0, 1, 0,
    1, 1, 0,
    2, 1, 0
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(
    0, 1, 2,
    2, 3, 4
  ), ncol = 3, byrow = TRUE)
  geom <- SurfaceGeometry(verts, faces, hemi = "lh")
  labs <- c(1, 2, 2, 2, 1)
  lsurf <- new(
    "LabeledNeuroSurface",
    geometry = geom,
    indices = 1:5,
    data = labs,
    labels = c("A", "B"),
    cols = c("#ff0000", "#00ff00")
  )

  expect_error(parcel_geodesic_centroid(lsurf, component_policy = "error"))

  cents_each <- parcel_geodesic_centroid(
    lsurf,
    component_policy = "each",
    cache = FALSE
  )
  expect_equal(nrow(cents_each), 3)

  pdm_min <- parcel_geodesic_distance_matrix(
    lsurf,
    metric = "min",
    component_policy = "merge",
    cache = FALSE
  )
  expect_equal(pdm_min["1", "2"], pdm_min["2", "1"])
  expect_true(is.finite(pdm_min["1", "2"]))
})


test_that("boundary contact reflects parcel adjacency", {
  verts <- matrix(c(
    0, 0, 0,
    1, 0, 0,
    0, 1, 0,
    1, 1, 0,
    2, 1, 0
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(
    0, 1, 2,
    2, 3, 4
  ), ncol = 3, byrow = TRUE)
  geom <- SurfaceGeometry(verts, faces, hemi = "lh")
  labs <- c(1, 2, 2, 2, 1)
  lsurf <- new(
    "LabeledNeuroSurface",
    geometry = geom,
    indices = 1:5,
    data = labs,
    labels = c("A", "B"),
    cols = c("#ff0000", "#00ff00")
  )

  bc <- parcel_boundary_contact(
    lsurf,
    component_policy = "merge",
    counts = TRUE
  )
  expect_true(bc["1", "2"] > 0)
  expect_equal(bc["1", "2"], bc["2", "1"])
})


test_that("medoid vs euclidean parcel centroids diverge when geometry is skewed", {
  verts <- matrix(c(
    0,   0, 0,
    2.2, 0, 0,
    0,   3, 0,
    2,   3, 0
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(
    0, 1, 2,
    1, 3, 2
  ), ncol = 3, byrow = TRUE)
  geom <- SurfaceGeometry(verts, faces, hemi = "lh")
  lsurf <- new(
    "LabeledNeuroSurface",
    geometry = geom,
    indices = 1:4,
    data = rep(1L, 4),
    labels = "A",
    cols = "#ff0000"
  )

  cent_medoid <- parcel_geodesic_centroid(
    lsurf,
    method = "medoid",
    cache = FALSE
  )
  cent_euclid <- parcel_geodesic_centroid(
    lsurf,
    method = "euclidean",
    cache = FALSE
  )

  expect_equal(cent_medoid$centroid_vertex, 3L)  # graph medoid (shortest mean path)
  expect_equal(cent_euclid$centroid_vertex, 2L)  # nearest Euclidean COM
  expect_false(identical(cent_medoid$centroid_vertex,
                         cent_euclid$centroid_vertex))
})

# Additional geodesic tests for coverage

test_that("geodesic_distances returns vector for single source", {
  geom <- example_surface_geometry()
  d <- geodesic_distances(geom, sources = 1, targets = 2:4)

  expect_type(d, "double")
  expect_equal(length(d), 3)
})

test_that("geodesic_distances returns matrix for multiple sources", {
  geom <- example_surface_geometry()
  d <- geodesic_distances(geom, sources = 1:2, targets = 3:4)

  expect_true(is.matrix(d))
  expect_equal(dim(d), c(2, 2))
})

test_that("clear_geodesic_cache works", {
  geom <- example_surface_geometry()

  # Fill cache
  gdm <- geodesic_distance_matrix(geom, mode = "sparse", cache = TRUE)

  # Clear
  result <- clear_geodesic_cache()

  expect_true(result)
})

test_that("geodesic_distance_matrix errors on invalid chunk_size", {
  geom <- example_surface_geometry()

  expect_error(
    geodesic_distance_matrix(geom, chunk_size = 0),
    "positive"
  )
  expect_error(
    geodesic_distance_matrix(geom, chunk_size = -1),
    "positive"
  )
})

test_that("geodesic_distance_matrix errors on invalid vertices", {
  geom <- example_surface_geometry()

  expect_error(
    geodesic_distance_matrix(geom, vertices = c(1, 100)),
    "out of range"
  )
  expect_error(
    geodesic_distance_matrix(geom, vertices = c(1.5, 2.5)),
    "integer"
  )
})

test_that("geodesic_distance_matrix errors on invalid targets", {
  geom <- example_surface_geometry()

  expect_error(
    geodesic_distance_matrix(geom, targets = c(1, 100)),
    "out of range"
  )
})

test_that("geodesic_distance_matrix works with custom vertices subset", {
  geom <- example_surface_geometry()

  gdm <- geodesic_distance_matrix(
    geom,
    vertices = 1:2,
    targets = 3:4,
    mode = "dense",
    cache = FALSE
  )

  expect_equal(dim(gdm), c(2, 2))
})

test_that("geodesic_distance_matrix caching works", {
  geom <- example_surface_geometry()

  clear_geodesic_cache()

  # First call populates cache
  gdm1 <- geodesic_distance_matrix(geom, mode = "sparse", cache = TRUE)

  # Second call should use cache
  gdm2 <- geodesic_distance_matrix(geom, mode = "sparse", cache = TRUE)

  expect_equal(as.matrix(gdm1), as.matrix(gdm2))
})

test_that("parcel_geodesic_centroid with largest policy works", {
  verts <- matrix(c(
    0, 0, 0,
    1, 0, 0,
    0, 1, 0,
    1, 1, 0,
    2, 1, 0
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(
    0, 1, 2,
    2, 3, 4
  ), ncol = 3, byrow = TRUE)
  geom <- SurfaceGeometry(verts, faces, hemi = "lh")
  labs <- c(1, 2, 2, 2, 1)  # Parcel 1 is fragmented (verts 1 and 5)
  lsurf <- new(
    "LabeledNeuroSurface",
    geometry = geom,
    indices = 1:5,
    data = labs,
    labels = c("A", "B"),
    cols = c("#ff0000", "#00ff00")
  )

  cents <- parcel_geodesic_centroid(
    lsurf,
    component_policy = "largest",
    cache = FALSE
  )

  expect_true(nrow(cents) >= 1)
})

test_that("parcel_geodesic_distance_matrix with min metric works", {
  geom <- example_surface_geometry()
  lsurf <- new(
    "LabeledNeuroSurface",
    geometry = geom,
    indices = 1:4,
    data = c(1, 1, 2, 2),
    labels = c("A", "B"),
    cols = c("#ff0000", "#00ff00")
  )

  pdm <- parcel_geodesic_distance_matrix(
    lsurf,
    metric = "min",
    cache = FALSE
  )

  expect_equal(dim(pdm), c(2, 2))
  expect_true(all(diag(pdm) == 0))
})

test_that("parcel_boundary_contact without counts returns 0/1", {
  geom <- example_surface_geometry()
  lsurf <- new(
    "LabeledNeuroSurface",
    geometry = geom,
    indices = 1:4,
    data = c(1, 1, 2, 2),
    labels = c("A", "B"),
    cols = c("#ff0000", "#00ff00")
  )

  bc <- parcel_boundary_contact(lsurf, counts = FALSE)

  expect_true(all(bc %in% c(0, 1)))
})

# Additional geodesic tests for higher coverage

test_that("geodesic_distance_matrix with custom weights works", {
  geom <- example_surface_geometry()
  g <- graph(geom)
  custom_weights <- rep(2, igraph::ecount(g))

  gdm <- geodesic_distance_matrix(
    geom,
    weights = custom_weights,
    mode = "dense",
    cache = FALSE
  )

  expect_true(is.matrix(gdm))
  expect_equal(dim(gdm), c(4, 4))
  # With doubled weights, distances should be doubled
  expect_equal(gdm[1, 2], 2, tolerance = 1e-6)
})

test_that("geodesic_distance_matrix errors on invalid weights", {
  geom <- example_surface_geometry()

  # Wrong length weights
  expect_error(
    geodesic_distance_matrix(geom, weights = c(1, 2)),
    "weights length"
  )

  # NA in weights
  g <- graph(geom)
  bad_weights <- igraph::E(g)$dist
  bad_weights[1] <- NA
  expect_error(
    geodesic_distance_matrix(geom, weights = bad_weights),
    "non-missing"
  )
})

test_that("parcel_geodesic_centroid with single-vertex parcel", {
  geom <- example_surface_geometry()
  # Create parcels where one has only 1 vertex
  lsurf <- new(
    "LabeledNeuroSurface",
    geometry = geom,
    indices = 1:4,
    data = c(1, 2, 2, 2),
    labels = c("A", "B"),
    cols = c("#ff0000", "#00ff00")
  )

  cents <- parcel_geodesic_centroid(lsurf, cache = FALSE)

  expect_equal(nrow(cents), 2)
  # Single vertex parcel should have that vertex as centroid
  expect_equal(cents$centroid_vertex[cents$parcel_id == 1], 1)
})

test_that("geodesic_distances with single source and single target", {
  geom <- example_surface_geometry()

  d <- geodesic_distances(geom, sources = 1, targets = 2)

  expect_type(d, "double")
  expect_length(d, 1)
  expect_equal(d, 1, tolerance = 1e-6)
})

test_that("parcel_boundary_contact with isolated parcels returns zeros", {
  # Create a simple geometry where two parcels don't touch
  verts <- matrix(c(
    0, 0, 0,
    1, 0, 0,
    0, 1, 0,
    5, 5, 0,  # Far away vertex
    6, 5, 0
  ), ncol = 3, byrow = TRUE)
  faces <- matrix(c(
    0, 1, 2,
    3, 4, 3  # Degenerate but forms an edge
  ), ncol = 3, byrow = TRUE)
  geom <- SurfaceGeometry(verts, faces, hemi = "lh")

  # Labels: 1,2,3 in parcel 1, 4,5 in parcel 2
  lsurf <- new(
    "LabeledNeuroSurface",
    geometry = geom,
    indices = 1:5,
    data = c(1, 1, 1, 2, 2),
    labels = c("A", "B"),
    cols = c("#ff0000", "#00ff00")
  )

  bc <- parcel_boundary_contact(lsurf, counts = TRUE)

  # These two parcels don't share edges
  expect_equal(bc["1", "2"], 0)
})

test_that("geodesic_distance_matrix dense mode diagonal is zero", {
  geom <- example_surface_geometry()

  gdm <- geodesic_distance_matrix(geom, mode = "dense", cache = FALSE)

  expect_true(all(diag(gdm) == 0))
})

test_that("geodesic_distance_matrix is symmetric", {
  geom <- example_surface_geometry()

  gdm <- geodesic_distance_matrix(geom, mode = "dense", cache = FALSE)

  expect_equal(gdm, t(gdm))
})

test_that("parcel_geodesic_distance_matrix diagonal is zero", {
  geom <- example_surface_geometry()
  lsurf <- new(
    "LabeledNeuroSurface",
    geometry = geom,
    indices = 1:4,
    data = c(1, 1, 2, 2),
    labels = c("A", "B"),
    cols = c("#ff0000", "#00ff00")
  )

  pdm <- parcel_geodesic_distance_matrix(lsurf, metric = "centroid", cache = FALSE)

  expect_true(all(diag(pdm) == 0))
})

test_that("parcel_geodesic_distance_matrix is symmetric", {
  geom <- example_surface_geometry()
  lsurf <- new(
    "LabeledNeuroSurface",
    geometry = geom,
    indices = 1:4,
    data = c(1, 1, 2, 2),
    labels = c("A", "B"),
    cols = c("#ff0000", "#00ff00")
  )

  pdm <- parcel_geodesic_distance_matrix(lsurf, metric = "centroid", cache = FALSE)

  expect_equal(pdm["1", "2"], pdm["2", "1"])
})

test_that("clear_geodesic_cache returns TRUE", {
  result <- clear_geodesic_cache()

  expect_true(result)
  expect_type(result, "logical")
})
