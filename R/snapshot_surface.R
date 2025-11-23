#' Snapshot a surface to a PNG
#'
#' Convenience helper for vignettes and reports: renders a surface with
#' `view_surface()` onto an off-screen rgl device and saves a PNG.
#' When `rgl.useNULL()` is `TRUE` (headless builds), a proper snapshot
#' requires the `webshot2` package; otherwise a blank image is likely and an
#' empty path is returned.
#'
#' @param surfgeom A \code{\linkS4class{SurfaceGeometry}} object.
#' @param file Output path for the PNG. Defaults to the current knitr figure
#'   path when knitting, otherwise a temporary file.
#' @param width,height Device size in pixels (controls render resolution).
#' @param ... Additional arguments passed to \code{\link{view_surface}}.
#'
#' @return The file path (invisibly). Callers can use
#'   \code{knitr::include_graphics()} or read the image via \code{png::readPNG()}.
#'   In headless mode without `webshot2`, an empty character vector is returned.
#'
#' @examples
#' \donttest{
#' fs <- load_fsaverage_std8("inflated")
#' img <- snapshot_surface(fs$lh, viewpoint = "lateral", specular = "black")
#' # knitr::include_graphics(img)
#' }
#' @export
snapshot_surface <- function(surfgeom,
                             file = NULL,
                             width = 1200,
                             height = 900,
                             ...) {
  stopifnot(inherits(surfgeom, "SurfaceGeometry"))

  if (is.null(file)) {
    if (isTRUE(getOption("knitr.in.progress")) &&
        requireNamespace("knitr", quietly = TRUE)) {
      file <- knitr::fig_path(".png")
    } else {
      file <- tempfile(fileext = ".png")
    }
  }

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

  use_null <- rgl::rgl.useNULL()

  rgl::open3d()
  rgl::par3d(windowRect = c(0, 0, width, height))
  rgl::bg3d(color = "white")

  view_surface(surfgeom, new_window = FALSE, ...)

  if (use_null && requireNamespace("webshot2", quietly = TRUE)) {
    rgl::snapshot3d(file, webshot = TRUE)
  } else {
    rgl::rgl.snapshot(file)
    if (use_null && !requireNamespace("webshot2", quietly = TRUE)) {
      warning("rgl.useNULL=TRUE and webshot2 not installed; snapshot may be blank in headless builds.")
    }
  }

  rgl::close3d()  # close the device we opened

  if (use_null && !requireNamespace("webshot2", quietly = TRUE)) {
    sz <- file.info(file)$size
    if (is.finite(sz) && sz < 2000) {
      warning("Snapshot appears blank (size < 2KB) when using null device; returning empty path.")
      return(invisible(character()))
    }
  }

  invisible(file)
}
