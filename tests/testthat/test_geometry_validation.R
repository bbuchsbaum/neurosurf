test_that("SurfaceGeometry validates shapes and indices", {
  verts <- matrix(c(0, 0, 0,
                    1, 0, 0,
                    0, 1, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3, byrow = TRUE)

  expect_no_error(SurfaceGeometry(verts, faces, hemi = "lh"))
  bad_faces <- matrix(c(0L, 1L, 5L), ncol = 3, byrow = TRUE)
  expect_error(SurfaceGeometry(verts, bad_faces, hemi = "lh"),
               "beyond available rows")
  bad_verts <- matrix(1:8, ncol = 2)
  expect_error(SurfaceGeometry(bad_verts, faces, hemi = "lh"),
               "3 columns")
})

test_that("meshToGraph rejects invalid inputs", {
  verts <- matrix(c(0, 0, 0,
                    1, 0, 0,
                    0, 1, 0), ncol = 3, byrow = TRUE)
  faces <- matrix(c(0L, 1L, 2L), ncol = 3, byrow = TRUE)

  expect_no_error(meshToGraph(verts, faces))
  neg_faces <- matrix(c(-1L, 0L, 1L), ncol = 3, byrow = TRUE)
  expect_error(meshToGraph(verts, neg_faces), "negative indices")
})
