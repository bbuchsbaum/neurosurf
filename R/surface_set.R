#' SurfaceSet: bundle multiple surface variants for one hemisphere
#'
#' A `SurfaceSet` keeps several `SurfaceGeometry` variants (e.g., pial, white,
#' inflated, sphere) for the same hemisphere together with a default choice.
#'
#' @slot hemi Character string for hemisphere (e.g., "lh" or "rh").
#' @slot surfaces Named list of `SurfaceGeometry` objects keyed by their label.
#' @slot default_label Character string giving the label to use by default.
#'
#' @export
setClass("SurfaceSet",
  representation = representation(
    hemi = "character",
    surfaces = "list",
    default_label = "character"
  ),
  validity = function(object) {
    errors <- character()
    if (length(object@surfaces) == 0) {
      errors <- c(errors, "'surfaces' must contain at least one geometry")
    }
    if (is.null(names(object@surfaces)) || any(names(object@surfaces) == "")) {
      errors <- c(errors, "'surfaces' must be a *named* list keyed by label")
    }
    all_geoms <- vapply(object@surfaces, inherits, logical(1), what = "SurfaceGeometry")
    if (!all(all_geoms)) {
      errors <- c(errors, "all elements of 'surfaces' must be SurfaceGeometry objects")
    }
    # shared hemisphere
    hemis <- vapply(object@surfaces, function(g) g@hemi, character(1))
    if (length(unique(hemis)) > 1) {
      errors <- c(errors, "all geometries must share the same hemisphere")
    }
    # shared vertex count
    nv <- vapply(object@surfaces, function(g) ncol(g@mesh$vb), integer(1))
    if (length(unique(nv)) > 1) {
      errors <- c(errors, "all geometries must have the same vertex count")
    }
    if (length(object@default_label) != 1 || is.na(object@default_label)) {
      errors <- c(errors, "'default_label' must be a single, non-NA label")
    }
    if (!(object@default_label %in% names(object@surfaces))) {
      errors <- c(errors, "'default_label' must match one of the surface labels")
    }
    if (length(errors)) errors else TRUE
  }
)

#' Construct a SurfaceSet
#'
#' @param ... Named `SurfaceGeometry` objects, or a single named list of them.
#' @param hemi Hemisphere code; defaults to the hemi of the first geometry.
#' @param default_label Optional default label; defaults to the first provided label.
#'
#' @return A `SurfaceSet` instance.
#' @export
surface_set <- function(..., hemi = NULL, default_label = NULL) {
  geoms <- list(...)
  if (length(geoms) == 1 && is.list(geoms[[1]]) && is.null(names(geoms))) {
    geoms <- geoms[[1]]
  }
  if (is.null(names(geoms)) || any(names(geoms) == "")) {
    stop("surface_set: supply named geometries keyed by their label")
  }
  if (is.null(hemi)) {
    hemi <- geoms[[1]]@hemi
  }
  if (is.null(default_label)) {
    default_label <- names(geoms)[1]
  }
  new("SurfaceSet", hemi = hemi, surfaces = geoms, default_label = default_label)
}

#' Retrieve a geometry from a SurfaceSet
#'
#' @param x SurfaceSet
#' @param label Label of the desired surface variant. Defaults to the set's default.
#'
#' @return A `SurfaceGeometry` object.
#' @export
get_surface <- function(x, label = NULL) {
  stopifnot(is(x, "SurfaceSet"))
  if (is.null(label)) label <- x@default_label
  if (!label %in% names(x@surfaces)) {
    stop("surface label not found in set: ", label)
  }
  x@surfaces[[label]]
}

resolve_surface_geometry <- function(geom, label = NULL) {
  if (is(geom, "SurfaceSet")) {
    get_surface(geom, label)
  } else if (is(geom, "SurfaceGeometry")) {
    geom
  } else {
    stop("expected a SurfaceGeometry or SurfaceSet")
  }
}

#' List available surface labels
#' @param x SurfaceSet
#' @return Character vector of labels
#' @export
surface_labels <- function(x) {
  stopifnot(is(x, "SurfaceSet"))
  names(x@surfaces)
}

# convenience helpers and pass-through methods
#' @rdname SurfaceSet-class
#' @export
setMethod("geometry", signature(x = "SurfaceSet"), function(x) get_surface(x))

#' @param x a `SurfaceSet`
#' @param ... additional arguments forwarded to the underlying geometry method
#' @rdname SurfaceSet-class
#' @export
setMethod("vertices", signature(x = "SurfaceSet"), function(x, ...) vertices(get_surface(x), ...))

#' @rdname SurfaceSet-class
#' @export
setMethod("faces", signature(x = "SurfaceSet"), function(x, ...) faces(get_surface(x), ...))

#' @rdname SurfaceSet-class
#' @export
setMethod("nodes", signature(x = "SurfaceSet"), function(x) nodes(get_surface(x)))

#' @rdname SurfaceSet-class
#' @export
setMethod("graph", signature(x = "SurfaceSet"), function(x, ...) graph(get_surface(x), ...))

#' @rdname SurfaceSet-class
#' @export
setMethod("curvature", signature(x = "SurfaceSet"), function(x, ...) curvature(get_surface(x), ...))
