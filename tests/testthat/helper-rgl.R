# Helper file to ensure rgl uses NULL device during tests
# This prevents segfaults when running tests in headless environments

if (requireNamespace("rgl", quietly = TRUE)) {
  options(rgl.useNULL = TRUE)
}