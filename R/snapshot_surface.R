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
#' \dontrun{
#' fs <- load_fsaverage_std8("inflated")
#' img <- snapshot_surface(fs$lh, viewpoint = "lateral", specular = "black")
#' # knitr::include_graphics(img)
#' }
#' @noRd
.ns_snapshot_is_blank <- function(file,
                                  uniform_fraction = 0.995,
                                  near_black = 0.02,
                                  near_white = 0.98,
                                  low_variation_sd = 0.005,
                                  low_variation_span = 0.025) {
  if (!is.character(file) || length(file) != 1L || !nzchar(file) ||
      !file.exists(file)) {
    return(TRUE)
  }

  img <- try(png::readPNG(file), silent = TRUE)
  if (inherits(img, "try-error")) {
    return(FALSE)
  }

  if (length(dim(img)) < 2L) {
    return(TRUE)
  }

  rgb <- if (length(dim(img)) >= 3L) {
    img[, , seq_len(min(3L, dim(img)[3L])), drop = FALSE]
  } else {
    img
  }

  vals <- as.numeric(rgb)
  vals <- vals[is.finite(vals)]
  if (!length(vals)) {
    return(TRUE)
  }

  if (mean(vals <= near_black) >= uniform_fraction ||
      mean(vals >= near_white) >= uniform_fraction) {
    return(TRUE)
  }

  stats::sd(vals) < low_variation_sd &&
    diff(range(vals)) < low_variation_span
}

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
  has_webshot <- requireNamespace("webshot2", quietly = TRUE)

  rgl::open3d()
  on.exit(try(rgl::close3d(), silent = TRUE), add = TRUE)
  rgl::par3d(windowRect = c(0, 0, width, height))
  rgl::bg3d(color = "white")

  view_surface(surfgeom, new_window = FALSE, ...)

  if (use_null && !has_webshot) {
    warning(
      "rgl.useNULL=TRUE and webshot2 not installed; snapshot unavailable in ",
      "headless builds."
    )
    return(invisible(character()))
  }

  if (use_null && has_webshot) {
    rgl::snapshot3d(file, webshot = TRUE)
  } else {
    rgl::rgl.snapshot(file)
  }

  if (.ns_snapshot_is_blank(file)) {
    warning("Snapshot appears blank; returning empty path.")
    unlink(file)
    return(invisible(character()))
  }

  invisible(file)
}
