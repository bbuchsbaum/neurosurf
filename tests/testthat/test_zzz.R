test_that(".onLoad sets rgl.useNULL in non-interactive mode", {
  # In test mode (non-interactive), rgl.useNULL should be TRUE
  # The package is already loaded by testthat, so we check the option
  expect_true(getOption("rgl.useNULL", FALSE))
})
