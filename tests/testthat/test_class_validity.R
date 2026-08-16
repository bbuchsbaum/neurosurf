validity_geometry <- function(hemi = "lh") {
  vertices <- matrix(
    c(
      0, 0, 0,
      1, 0, 0,
      0, 1, 0,
      0, 0, 1
    ),
    ncol = 3,
    byrow = TRUE
  )
  faces <- matrix(
    c(
      0, 1, 2,
      0, 1, 3,
      0, 2, 3,
      1, 2, 3
    ),
    ncol = 3,
    byrow = TRUE
  )
  SurfaceGeometry(vertices, faces, hemi = hemi)
}

test_that("SurfaceGeometry validates mesh, graph, and affine invariants", {
  geometry <- validity_geometry()
  expect_true(validObject(geometry))

  bad_affine <- geometry
  bad_affine@surf_to_world <- diag(3)
  expect_error(validObject(bad_affine), "surf_to_world")

  bad_graph <- geometry
  bad_graph@graph <- igraph::delete_vertices(bad_graph@graph, 4L)
  expect_error(validObject(bad_graph), "vertex count")

  bad_mesh <- geometry
  mesh <- bad_mesh@mesh
  mesh$it[1, 1] <- 1.5
  bad_mesh@mesh <- mesh
  expect_error(validObject(bad_mesh), "mesh\\$it")
})

test_that("SurfaceGeometry constructor rejects lossy or non-finite inputs", {
  vertices <- matrix(
    c(0, 0, 0, 1, 0, 0, 0, 1, 0),
    ncol = 3,
    byrow = TRUE
  )
  faces <- matrix(c(0, 1, 2), nrow = 1)

  fractional_faces <- faces
  fractional_faces[1, 1] <- 0.5
  expect_error(
    SurfaceGeometry(vertices, fractional_faces, "lh"),
    "integer-valued"
  )

  non_finite_vertices <- vertices
  non_finite_vertices[1, 1] <- Inf
  expect_error(
    SurfaceGeometry(non_finite_vertices, faces, "lh"),
    "finite numeric coordinates"
  )
  expect_error(SurfaceGeometry(vertices, faces, character()), "hemi")
  expect_error(SurfaceGeometry(vertices, faces, "lh", label = c("a", "b")),
               "label")
})

test_that("surface data constructors preserve integer index semantics", {
  geometry <- validity_geometry()
  values <- seq_len(4)

  expect_error(NeuroSurface(geometry, c(1, 2.5), values[1:2]),
               "integer-valued")
  expect_error(NeuroSurface(geometry, c(1, NA), values[1:2]),
               "missing or non-finite")
  expect_error(NeuroSurface(geometry, c(0, 1), values[1:2]),
               "positive indices")
  expect_error(NeuroSurface(geometry, c(1, 5), values[1:2]),
               "outside the geometry")
  expect_error(NeuroSurface(geometry, c(1, 1), values[1:2]),
               "duplicates")

  empty <- NeuroSurface(geometry, integer(), numeric())
  expect_true(validObject(empty))
})

test_that("NeuroSurfaceVector data rows follow the full geometry", {
  geometry <- validity_geometry()
  full_data <- matrix(seq_len(8), nrow = 4, ncol = 2)

  vector <- NeuroSurfaceVector(geometry, c(1L, 3L), full_data)
  expect_true(validObject(vector))
  expect_error(
    NeuroSurfaceVector(geometry, c(1L, 3L), full_data[-1, ]),
    "nrow\\(data\\)"
  )
})

test_that("derived surface classes validate color contracts", {
  geometry <- validity_geometry()
  indices <- seq_len(4)
  values <- c(-2, -1, 1, 2)
  colors <- c("#0000FF", "#FFFFFF", "#FF0000")

  expect_error(
    ColorMappedNeuroSurface(
      geometry, indices, values, colors, c(-2, 2), c(1, -1)
    ),
    "thresh\\[1\\]"
  )
  expect_error(
    ColorMappedNeuroSurface(
      geometry, indices, values, c("red", "blue"), c(-2, 2), c(-1, 1)
    ),
    "valid hex colors"
  )
  expect_no_error(
    ColorMappedNeuroSurface(
      geometry, indices, values, colors, c(-2, 2), c(-Inf, Inf)
    )
  )

  expect_error(
    methods::new(
      "LabeledNeuroSurface",
      geometry = geometry,
      indices = indices,
      data = c(1, 2, 1, 2),
      labels = c("region-a", "region-b"),
      cols = c("#FF0000", "not-a-color")
    ),
    "valid hex colors"
  )
})

test_that("ROI validity protects parallel data and cached coordinates", {
  geometry <- validity_geometry()

  expect_error(ROISurface(geometry, c(1L, 2L), 1),
               "length of 'data'")
  expect_error(ROISurface(geometry, c(1L, 1L), c(1, 2)), "duplicates")
  expect_error(
    ROISurfaceVector(geometry, c(1L, 2L), matrix(1:3, nrow = 1)),
    "ncol\\(data\\)"
  )

  roi <- ROISurface(geometry, c(1L, 2L), c(1, 2))
  roi@coords[1, 1] <- roi@coords[1, 1] + 1
  expect_error(validObject(roi), "must match geometry")
})

test_that("file descriptor prototypes are usable and self-consistent", {
  descriptors <- list(
    NIML = methods::new("NIMLSurfaceFileDescriptor"),
    `1D` = methods::new("AFNISurfaceFileDescriptor"),
    GIFTI = methods::new("GIFTISurfaceFileDescriptor"),
    Freesurfer_ASCII = methods::new("FreesurferAsciiSurfaceFileDescriptor"),
    Freesurfer_BINARY = methods::new("FreesurferBinarySurfaceFileDescriptor")
  )

  expect_equal(
    unname(vapply(descriptors, slot, character(1), name = "file_format")),
    unname(names(descriptors))
  )
  expect_true(all(vapply(descriptors, validObject, logical(1))))

  invalid <- descriptors$GIFTI
  invalid@data_extension <- ""
  expect_error(validObject(invalid), "data_extension")
})

test_that("metadata validity cross-checks declared dimensions", {
  descriptor <- methods::new("NIMLSurfaceFileDescriptor")
  metadata <- methods::new(
    "NIMLSurfaceDataMetaInfo",
    header_file = "data.niml.dset",
    data_file = "data.niml.dset",
    file_descriptor = descriptor,
    node_count = 4L,
    nels = 2L,
    label = "thickness",
    data = matrix(seq_len(8), nrow = 4, ncol = 2),
    node_indices = 0:3
  )
  expect_true(validObject(metadata))

  metadata@data <- matrix(seq_len(6), nrow = 3, ncol = 2)
  expect_error(validObject(metadata), "nrow\\(data\\)")
  empty_metadata <- methods::new("SurfaceDataMetaInfo")
  expect_error(validObject(empty_metadata), "header_file")

  geometry_metadata <- methods::new(
    "SurfaceGeometryMetaInfo",
    header_file = "lh.pial.asc",
    data_file = "lh.pial.asc",
    file_descriptor = methods::new("FreesurferAsciiSurfaceFileDescriptor"),
    vertices = 4L,
    faces = 4L,
    label = "pial",
    hemi = "lh",
    embed_dimension = 3L
  )
  expect_true(validObject(geometry_metadata))
  geometry_metadata@faces <- 0L
  expect_error(validObject(geometry_metadata), "positive integer")
})

test_that("surface sources validate requested node and column ranges", {
  geometry <- validity_geometry()
  descriptor <- methods::new("AFNISurfaceFileDescriptor")
  metadata <- methods::new(
    "SurfaceDataMetaInfo",
    header_file = "data.1D.dset",
    data_file = "data.1D.dset",
    file_descriptor = descriptor,
    node_count = 4L,
    nels = 2L,
    label = "thickness"
  )
  source <- methods::new(
    "NeuroSurfaceSource",
    geometry = geometry,
    data_meta_info = metadata,
    colind = 1L,
    nodeind = seq_len(4)
  )
  expect_true(validObject(source))

  source@colind <- 3L
  expect_error(validObject(source), "outside 'data_meta_info'")
  source@colind <- 1L
  source@nodeind <- c(1L, 2L, 3L, 5L)
  expect_error(validObject(source), "outside the geometry")
})

test_that("bilateral vectors enforce hemisphere and column contracts", {
  left_geometry <- validity_geometry("left")
  right_geometry <- validity_geometry("right")
  left <- NeuroSurfaceVector(left_geometry, seq_len(4), matrix(1:8, 4, 2))
  right <- NeuroSurfaceVector(right_geometry, seq_len(4), matrix(9:16, 4, 2))

  bilateral <- methods::new(
    "BilatNeuroSurfaceVector",
    left = left,
    right = right
  )
  expect_true(validObject(bilateral))

  right@data <- Matrix::Matrix(matrix(1:12, 4, 3))
  expect_error(
    methods::new("BilatNeuroSurfaceVector", left = left, right = right),
    "same number of columns"
  )

  right <- NeuroSurfaceVector(left_geometry, seq_len(4), matrix(9:16, 4, 2))
  expect_error(
    methods::new("BilatNeuroSurfaceVector", left = left, right = right),
    "right hemisphere"
  )
})
