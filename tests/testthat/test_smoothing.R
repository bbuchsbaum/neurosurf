library(neurosurf)

test_that("smoothing radius influences output", {
  surf_file <- system.file("extdata", "std.8_lh.inflated.asc", package = "neurosurf")
  surf <- read_surf_geometry(surf_file)
  n_vertices <- nrow(coords(surf))
  set.seed(42)
  data_vals <- rnorm(n_vertices)
  nsurf <- NeuroSurface(geometry = surf, indices = 1:n_vertices, data = data_vals)

  sm_small <- smooth(nsurf, sigma = 2)
  sm_large <- smooth(nsurf, sigma = 6)

  sd_small <- sd(as.vector(series(sm_small, indices(sm_small))))
  sd_large <- sd(as.vector(series(sm_large, indices(sm_large))))

  expect_true(sd_large < sd_small)
  expect_false(isTRUE(all.equal(as.vector(series(sm_small, indices(sm_small))),
                                as.vector(series(sm_large, indices(sm_large))))))
})
