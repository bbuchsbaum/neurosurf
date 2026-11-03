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
#' bundled: \code{"fsaverage5"} (10,242 vertices per hemisphere; inflated,
#' white, and pial surfaces) and the legacy \code{"std.8"} mesh (642 vertices per
#' hemisphere; smoothwm, pial, inflated, white, and sphere surfaces). Use
#' fsaverage5 for figures and examples; std.8 is kept for small tests.
#'
#' @param density Surface density: \code{"std.8"} (default, for backward
#'   compatibility) or \code{"fsaverage5"}.
#' @param surf Character string specifying which surface to load. One of
#'   \code{"smoothwm"}, \code{"pial"}, \code{"inflated"}, \code{"white"},
#'   or \code{"sphere"} for std.8; \code{"inflated"}, \code{"white"}, or
#'   \code{"pial"} for fsaverage5. Defaults to \code{"smoothwm"} for std.8 and
#'   \code{"inflated"} for fsaverage5.
#'
#' @return A named list with elements \code{"lh"} and \code{"rh"}, each a
#'   \code{SurfaceGeometry} instance.
#'
#' @details The fsaverage5 files are the FreeSurfer fsaverage5 template as
#'   redistributed by nilearn, under the FreeSurfer license.
#'
#'   The fsaverage5 white and pial surfaces share one triangulation, so they
#'   can be passed together to \code{\link{vol_to_surf}} to sample a volume through the
#'   cortical ribbon. Their coordinates are in fsaverage (MNI305) space, which
#'   differs by a few millimetres from MNI152; results in MNI152 space are
#'   therefore approximate.
#'
#' @seealso \code{\link{load_fsaverage_sulc}} for the matching sulcal depth.
#'
#' @examples
#' \donttest{
#' fs <- load_fsaverage("fsaverage5", "inflated")
#' nrow(coords(fs$lh))
#'
#' # White + pial pair for volume-to-surface projection
#' wm <- load_fsaverage("fsaverage5", "white")
#' pial <- load_fsaverage("fsaverage5", "pial")
#' }
#'
#' @export
load_fsaverage <- function(density = c("std.8", "fsaverage5"), surf = NULL) {
  density <- match.arg(density)
  if (density == "std.8") {
    return(load_fsaverage_std8(surf = surf %||% "smoothwm"))
  }
  surf <- match.arg(surf %||% "inflated", c("inflated", "white", "pial"))
  prefix <- c(inflated = "infl", white = "white", pial = "pial")[[surf]]
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
#' \code{\link{surface_figure}} or \code{\link{render_surface_rgba}}. Positive values are deeper
#' (sulcal); the renderers detect this polarity automatically.
#'
#' @param density Template density; currently \code{"fsaverage5"}.
#' @return A named list with numeric vectors \code{"lh"} and \code{"rh"},
#'   one value per vertex of \code{\link{load_fsaverage}} for the same density.
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
#' @param density Surface density: \code{"std.8"} (default) or
#'   \code{"fsaverage5"}.
#' @param surfs Character vector of surface labels to include. For std.8, any
#'   of \code{c("smoothwm","pial","inflated","white","sphere")}; for
#'   fsaverage5, any of \code{c("inflated","white","pial")}. Defaults to all
#'   surfaces available at the chosen density.
#' @param default_label Which label to treat as default when none is
#'   specified. Defaults to \code{"smoothwm"} for std.8 and
#'   \code{"inflated"} for fsaverage5.
#'
#' @return A named list with elements \code{\"lh\"} and \code{\"rh\"}, each a
#'   \code{\linkS4class{SurfaceSet}} containing the requested variants.
#'
#' @examples
#' \donttest{
#' bundle <- load_fsaverage_bundle()
#' lh_set <- bundle$lh
#' rh_set <- bundle$rh
#' fs5 <- load_fsaverage_bundle("fsaverage5")
#' }
#'
#' @export
load_fsaverage_bundle <- function(density = c("std.8", "fsaverage5"),
                                  surfs = NULL,
                                  default_label = NULL) {
  density <- match.arg(density)
  choices <- if (density == "std.8") {
    c("smoothwm", "pial", "inflated", "white", "sphere")
  } else {
    c("inflated", "white", "pial")
  }
  surfs <- match.arg(surfs %||% choices, choices = choices, several.ok = TRUE)
  if (length(surfs) == 0) stop("surfs must include at least one surface label")
  default_label <- default_label %||%
    if (density == "std.8") "smoothwm" else "inflated"
  default_label <- match.arg(default_label, surfs)

  hemis <- c("lh", "rh")

  # load each requested surface once, then repackage by hemisphere
  by_surf <- lapply(surfs, function(s) load_fsaverage(density, s))
  names(by_surf) <- surfs

  sets <- lapply(hemis, function(h) {
    geoms <- lapply(by_surf, function(lst) lst[[h]])
    surface_set(geoms, hemi = h, default_label = default_label)
  })
  names(sets) <- hemis
  sets
}
