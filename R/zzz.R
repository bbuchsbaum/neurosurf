#' @useDynLib neurosurf, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

.onLoad <- function(libname, pkgname) {
  # Force NULL device on headless systems (including CRAN)
  # This prevents segfaults when rgl tries to access OpenGL in environments
  # without a display server
  if (!interactive() || identical(Sys.getenv("RGL_USE_NULL"), "TRUE")) {
    # Set the rgl.useNULL option to TRUE
    options(rgl.useNULL = TRUE)
  }
}