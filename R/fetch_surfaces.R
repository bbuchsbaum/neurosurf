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
#' show_surface_plot(fs$lh, fs$rh, views = c("lateral", "medial"))
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


#' Fetch fsaverage surfaces
#'
#' This is a high-level wrapper that mirrors the API of neuromaps'
#' \code{fetch_fsaverage()} but is currently limited to the \code{"std.8"}
#' decimated fsaverage surfaces that ship with neurosurf. The function name
#' uses "load" rather than "fetch" to follow common R idioms.
#'
#' @param density Character string specifying surface density. At present only
#'   \code{"std.8"} is supported.
#' @param surf Character string specifying which surface to load. One of
#'   \code{"smoothwm"}, \code{"pial"}, \code{"inflated"}, \code{"white"},
#'   or \code{"sphere"}. Defaults to \code{"smoothwm"}.
#'
#' @return A named list with elements \code{"lh"} and \code{"rh"}, each a
#'   \code{SurfaceGeometry} instance.
#'
#' @examples
#' \donttest{
#' fs <- load_fsaverage(density = "std.8", surf = "inflated")
#' show_surface_plot(fs$lh, fs$rh, views = c("lateral", "medial"))
#' }
#'
#' @export
load_fsaverage <- function(density = "std.8",
                           surf = c("smoothwm", "pial", "inflated", "white", "sphere")) {
  density <- match.arg(density, choices = "std.8")
  load_fsaverage_std8(surf = surf)
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
