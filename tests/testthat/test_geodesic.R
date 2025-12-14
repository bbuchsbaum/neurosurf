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
