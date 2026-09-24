#' Load fsaverage std.8 surfaces packaged with neurosurf
#'
#' This convenience helper loads the FreeSurfer fsaverage surfaces that ship
#' with neurosurf (the \code{std.8} decimated variant) and returns them as
#' \code{\linkS4class{SurfaceGeometry}} objects.
#'
#' @param surf Character string specifying which surface to load. One of
#'   \code{"smoothwm"}, \code{"pial"}, \code{"inflated"}, \code{"white"},
#'   or \code{"sphere"}. Defaults to \code{"smoothwm"}.
#'
#' @return A named list with elements \code{"lh"} and \code{"rh"}, each a
#'   \code{SurfaceGeometry} instance for the requested surface type.
#'
#' @examples
#' \donttest{
#' fs <- load_fsaverage_std8("smoothwm")
#' if (interactive()) {
#'   show_surface_plot(fs$lh, fs$rh, views = c("lateral", "medial"))
#' }
#' }
#'
#' @export
load_fsaverage_std8 <- function(surf = c("smoothwm", "pial", "inflated", "white", "sphere")) {
  surf <- match.arg(surf)
  hemis <- c("lh", "rh")

  geoms <- lapply(hemis, function(h) {
    fname <- sprintf("std.8_%s.%s.asc", h, surf)
    path <- system.file("extdata", fname, package = "neurosurf")
    if (path == "") {
      stop("Could not find fsaverage std.8 surface file '", fname,
           "' in the package extdata directory.")
    }
    read_surf_geometry(path)
  })

  names(geoms) <- hemis
  geoms
}


#' Load packaged fsaverage surfaces
#'
#' Loads a FreeSurfer fsaverage template that ships with neurosurf and returns
#' it as \code{\linkS4class{SurfaceGeometry}} objects. Two densities are
#' bundled: \code{"fsaverage5"} (10,242 vertices per hemisphere; inflated and
#' white surfaces) and the legacy \code{"std.8"} mesh (642 vertices per
#' hemisphere; smoothwm, pial, inflated, white, and sphere surfaces). Use
#' fsaverage5 for figures and examples; std.8 is kept for small tests.
#'
#' @param density Surface density: \code{"std.8"} (default, for backward
#'   compatibility) or \code{"fsaverage5"}.
#' @param surf Character string specifying which surface to load. One of
#'   \code{"smoothwm"}, \code{"pial"}, \code{"inflated"}, \code{"white"},
#'   or \code{"sphere"} for std.8; \code{"inflated"} or \code{"white"} for
#'   fsaverage5. Defaults to \code{"smoothwm"} for std.8 and
#'   \code{"inflated"} for fsaverage5.
#'
#' @return A named list with elements \code{"lh"} and \code{"rh"}, each a
#'   \code{SurfaceGeometry} instance.
#'
#' @details The fsaverage5 files are the FreeSurfer fsaverage5 template as
#'   redistributed by nilearn, under the FreeSurfer license.
#'
#' @seealso [load_fsaverage_sulc()] for the matching sulcal depth.
#'
#' @examples
#' \donttest{
#' fs <- load_fsaverage("fsaverage5", "inflated")
#' nrow(coords(fs$lh))
#' }
#'
#' @export
load_fsaverage <- function(density = c("std.8", "fsaverage5"), surf = NULL) {
  density <- match.arg(density)
  if (density == "std.8") {
    return(load_fsaverage_std8(surf = surf %||% "smoothwm"))
  }
  surf <- match.arg(surf %||% "inflated", c("inflated", "white"))
  prefix <- c(inflated = "infl", white = "white")[[surf]]
  geoms <- lapply(c(lh = "left", rh = "right"), function(side) {
    path <- .ns_fsaverage5_file(paste0(prefix, "_", side, ".gii.gz"))
    g <- read_surf_geometry(path)
    SurfaceGeometry(coords(g), faces(g) - 1L,
                    hemi = if (side == "left") "lh" else "rh",
                    label = paste0("fsaverage5_", surf))
  })
  geoms
}

#' Sulcal depth for a packaged fsaverage template
#'
#' Returns FreeSurfer sulcal depth (\code{sulc}) for each hemisphere of a
#' bundled fsaverage template, suitable as the anatomy underlay of
#' [surface_figure()] or [render_surface_rgba()]. Positive values are deeper
#' (sulcal); the renderers detect this polarity automatically.
#'
#' @param density Template density; currently \code{"fsaverage5"}.
#' @return A named list with numeric vectors \code{"lh"} and \code{"rh"},
#'   one value per vertex of [load_fsaverage()] for the same density.
#' @examples
#' \donttest{
#' sulc <- load_fsaverage_sulc("fsaverage5")
#' range(sulc$lh)
#' }
#' @export
load_fsaverage_sulc <- function(density = "fsaverage5") {
  density <- match.arg(density, "fsaverage5")
  lapply(c(lh = "left", rh = "right"), function(side) {
    path <- .ns_fsaverage5_file(paste0("sulc_", side, ".gii.gz"))
    as.numeric(gifti::readgii(path)$data[[1]])
  })
}

.ns_fsaverage5_file <- function(name) {
  path <- system.file("extdata", "fsaverage5", name, package = "neurosurf")
  if (!nzchar(path)) {
    stop("Could not find fsaverage5 file '", name,
         "' in the package extdata directory.", call. = FALSE)
  }
  path
}


#' Load a bundle of fsaverage surface variants as a SurfaceSet
#'
#' @param density Surface density; currently only \code{\"std.8\"} is supported.
#' @param surfs Character vector of surface labels to include (e.g.,
#'   \code{c(\"smoothwm\",\"pial\",\"inflated\",\"white\",\"sphere\")}).
#' @param default_label Which label to treat as default when none is specified.
#'
#' @return A named list with elements \code{\"lh\"} and \code{\"rh\"}, each a
#'   \code{\linkS4class{SurfaceSet}} containing the requested variants.
#'
#' @examples
#' \donttest{
#' bundle <- load_fsaverage_bundle()
#' lh_set <- bundle$lh
#' rh_set <- bundle$rh
#' }
#'
#' @export
load_fsaverage_bundle <- function(density = "std.8",
                                  surfs = c("smoothwm", "pial", "inflated", "white", "sphere"),
                                  default_label = "smoothwm") {
  density <- match.arg(density, choices = "std.8")
  choices <- c("smoothwm", "pial", "inflated", "white", "sphere")
  surfs <- match.arg(surfs, choices = choices, several.ok = TRUE)
  if (length(surfs) == 0) stop("surfs must include at least one surface label")
  default_label <- match.arg(default_label, surfs)

  hemis <- c("lh", "rh")

  # load each requested surface once, then repackage by hemisphere
  by_surf <- lapply(surfs, load_fsaverage_std8)
  names(by_surf) <- surfs

  sets <- lapply(hemis, function(h) {
    geoms <- lapply(by_surf, function(lst) lst[[h]])
    surface_set(geoms, hemi = h, default_label = default_label)
  })
  names(sets) <- hemis
  sets
}
