# Internal counter for generating unique figure paths when knitting.
.ns_montage_state <- new.env(parent = emptyenv())
.ns_montage_state$n <- 0L

# Draw a single panel into the *current* rgl (sub)scene.
# Dispatches on the object type so callers can mix bare geometries, data
# surfaces, and surface sets in one montage.
.ns_draw_panel <- function(obj, args) {
  if (is.character(obj)) {
    obj <- read_surf_geometry(obj)
  }
  args$new_window <- FALSE
  if (methods::is(obj, "SurfaceGeometry")) {
    do.call(view_surface, c(list(obj), args))
  } else {
    # NeuroSurface / LabeledNeuroSurface / ColorMappedNeuroSurface /
    # VertexColoredNeuroSurface / SurfaceSet all have plot() methods that
    # extract their own data and forward extra arguments to view_surface().
    # Use the string form so dispatch resolves to the S4 generic, not the
    # base S3 plot (which would treat the second argument as `y`).
    do.call("plot", c(list(obj), args))
  }
}

# Take a snapshot of the current scene, honouring headless (rgl.useNULL) builds.
# Returns TRUE on success.
.ns_take_snapshot <- function(file) {
  if (rgl::rgl.useNULL()) {
    if (!requireNamespace("webshot2", quietly = TRUE)) {
      return(FALSE)
    }
    rgl::snapshot3d(file, webshot = TRUE)
  } else {
    rgl::rgl.snapshot(file)
  }
  file.exists(file)
}

# Render one panel in its own clean scene and return it as an RGB array,
# or NULL if the snapshot is unavailable / blank.
.ns_snapshot_panel <- function(obj, args, width, height) {
  f <- tempfile(fileext = ".png")
  on.exit(unlink(f), add = TRUE)

  rgl::open3d()
  on.exit(try(rgl::close3d(), silent = TRUE), add = TRUE)
  rgl::par3d(windowRect = c(0, 0, width, height))
  rgl::bg3d(color = "white")

  .ns_draw_panel(obj, args)

  if (!.ns_take_snapshot(f) || .ns_snapshot_is_blank(f)) {
    return(NULL)
  }
  .ns_to_rgb(png::readPNG(f))
}

# Coerce a readPNG result (grey, RGB, or RGBA) to an RGB array, compositing
# any alpha channel over white.
.ns_to_rgb <- function(a) {
  d <- dim(a)
  if (length(d) == 2L) {
    a <- array(a, dim = c(d, 1L))
    d <- dim(a)
  }
  if (d[3L] == 1L) {
    a <- array(a[, , 1L], dim = c(d[1L], d[2L], 3L))
  } else if (d[3L] >= 4L) {
    alpha <- a[, , 4L]
    rgbm  <- a[, , 1:3, drop = FALSE]
    for (k in 1:3) {
      rgbm[, , k] <- rgbm[, , k] * alpha + (1 - alpha)
    }
    a <- rgbm
  } else {
    a <- a[, , 1:3, drop = FALSE]
  }
  a
}

# Place an RGB array on a white H x W canvas, centred.
.ns_place_on_canvas <- function(a, H, W) {
  out <- array(1, dim = c(H, W, 3L))
  h <- dim(a)[1L]
  w <- dim(a)[2L]
  r0 <- (H - h) %/% 2L
  c0 <- (W - w) %/% 2L
  out[(r0 + 1L):(r0 + h), (c0 + 1L):(c0 + w), ] <- a
  out
}

# Tile a list of RGB arrays into a single nrow x ncol image (row-major).
.ns_compose_grid <- function(arrays, nrow, ncol) {
  H <- max(vapply(arrays, function(a) dim(a)[1L], integer(1)))
  W <- max(vapply(arrays, function(a) dim(a)[2L], integer(1)))
  canvas <- array(1, dim = c(H * nrow, W * ncol, 3L))
  for (i in seq_along(arrays)) {
    r <- (i - 1L) %/% ncol
    cc <- (i - 1L) %% ncol
    cell <- .ns_place_on_canvas(arrays[[i]], H, W)
    canvas[(r * H + 1L):(r * H + H), (cc * W + 1L):(cc * W + W), ] <- cell
  }
  canvas
}

# Build an mfrow3d scene containing every panel (used for interactive display
# and for the rglwidget fallback when static snapshots are unavailable).
.ns_montage_scene <- function(specs, nrow, ncol) {
  rgl::open3d()
  rgl::mfrow3d(nrow, ncol, sharedMouse = TRUE)
  for (i in seq_along(specs)) {
    if (i > 1L) rgl::next3d()
    .ns_draw_panel(specs[[i]]$obj, specs[[i]]$args)
  }
}

# Normalise a single panel entry to list(obj=, args=).
.ns_norm_panel <- function(p, shared) {
  if (methods::isVirtualClass(class(p))) {
    stop("Cannot plot a virtual-class object as a montage panel.")
  }
  if (isS4(p) || is.character(p)) {
    obj <- p
    panel_args <- list()
  } else if (is.list(p)) {
    nm <- names(p)
    if (is.null(nm)) nm <- rep("", length(p))
    idx <- which(!nzchar(nm))
    if (!length(idx)) {
      stop("Each panel list must include the surface as an unnamed element, ",
           "e.g. list(my_surface, thresh = c(-2, 2)).")
    }
    obj <- p[[idx[1L]]]
    panel_args <- p[-idx[1L]]
  } else {
    stop("Each panel must be a surface object, a file path, or a list().")
  }
  list(obj = obj, args = utils::modifyList(shared, panel_args, keep.null = TRUE))
}


#' Arrange multiple surface views into a single montage figure
#'
#' @description
#' Renders several surface views as one figure: a row (the default) or grid of
#' panels. It is designed for R Markdown / pkgdown documents, where it replaces
#' the repetitive "snapshot each panel, check it, otherwise fall back to an
#' interactive widget" boilerplate with a single call.
#'
#' When the session can produce static snapshots (an interactive `rgl` device,
#' or a headless build with \pkg{webshot2} installed) the panels are captured
#' and tiled into one PNG. When knitting this is returned via
#' \code{\link[knitr]{include_graphics}}; otherwise the composed image is
#' written to \code{file}. If snapshots are unavailable, the panels are drawn
#' into a single \code{\link[rgl]{mfrow3d}} scene and returned as one
#' \code{\link[rgl]{rglwidget}} (a single combined widget, never one widget per
#' panel).
#'
#' @param panels A list of panels. Each element is either a surface object
#'   (\code{\link{SurfaceGeometry}}, \code{\link{NeuroSurface}} and its
#'   subclasses, or a \code{SurfaceSet}), a file path readable by
#'   \code{\link{read_surf_geometry}}, or a list whose first \emph{unnamed}
#'   element is such an object and whose remaining \emph{named} elements are
#'   per-panel display arguments (for example \code{thresh}, \code{vals},
#'   \code{cmap}, \code{irange}, or \code{viewpoint}). Per-panel arguments
#'   override the shared arguments passed through \code{...}.
#' @param ncol,nrow Integer layout controls. If both are \code{NULL} (default),
#'   all panels are placed in a single row. If only one is supplied the other is
#'   derived from the number of panels.
#' @param width,height Pixel dimensions used when snapshotting each panel.
#' @param file Optional output path for the composed PNG. When knitting, a
#'   figure path is generated automatically and this is ignored. When called
#'   interactively, supplying \code{file} forces composition to that path
#'   instead of opening an interactive window.
#' @param ... Shared display arguments forwarded to every panel (e.g.
#'   \code{cmap}, \code{irange}, \code{specular}, \code{viewpoint}). These are
#'   passed to \code{\link{view_surface}} (for \code{SurfaceGeometry} panels) or
#'   to the object's \code{plot} method (for data surfaces).
#'
#' @return
#' When knitting, a \code{knitr} image object (static path) or a single
#' \code{rglwidget} (fallback). When called interactively, the composed file
#' path (if \code{file} is given) or \code{invisible(NULL)} after drawing an
#' interactive \code{mfrow3d} window.
#'
#' @details
#' Each static panel is rendered in its own fresh \code{rgl} scene, so lighting
#' is computed independently and panels never share or clobber one another's
#' lights. Panels are centred on equally sized white cells so differing
#' silhouettes stay aligned.
#'
#' @examples
#' \donttest{
#' geom <- example_surface_geometry()
#' set.seed(1)
#' vals <- rnorm(nrow(coords(geom)))
#' ns <- smooth(NeuroSurface(geom, indices = seq_along(vals), data = vals))
#'
#' out <- tempfile(fileext = ".png")
#' if (interactive()) {
#'   surface_montage(
#'     list(
#'       ns,
#'       list(ns, thresh = c(-0.5, 0.5)),
#'       list(ns, thresh = c(-1, 1))
#'     ),
#'     cmap = grDevices::rainbow(100), irange = c(-2, 2),
#'     ncol = 3, file = out
#'   )
#' }
#' }
#'
#' @seealso \code{\link{view_surface}}, \code{\link{snapshot_surface}},
#'   \code{\link{surface_plot}}
#' @export
surface_montage <- function(panels, ncol = NULL, nrow = NULL,
                            width = 600, height = 450, file = NULL, ...) {
  if (!is.list(panels) || !length(panels)) {
    stop("'panels' must be a non-empty list of surface panels.")
  }

  shared <- list(...)
  specs <- lapply(panels, .ns_norm_panel, shared = shared)
  n <- length(specs)

  if (is.null(ncol) && is.null(nrow)) {
    nrow <- 1L
    ncol <- n
  } else if (is.null(ncol)) {
    ncol <- ceiling(n / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(n / ncol)
  }
  nrow <- as.integer(nrow)
  ncol <- as.integer(ncol)

  knitting <- isTRUE(getOption("knitr.in.progress"))
  want_file <- knitting || !is.null(file)

  if (want_file) {
    if (knitting && is.null(file)) {
      .ns_montage_state$n <- .ns_montage_state$n + 1L
      file <- knitr::fig_path(paste0("-montage-", .ns_montage_state$n, ".png"))
    }
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

    arrays <- lapply(specs, function(s) {
      .ns_snapshot_panel(s$obj, s$args, width = width, height = height)
    })

    if (!any(vapply(arrays, is.null, logical(1)))) {
      canvas <- .ns_compose_grid(arrays, nrow, ncol)
      png::writePNG(canvas, target = file)
      if (knitting) {
        return(knitr::include_graphics(file))
      }
      return(invisible(file))
    }

    # Static snapshots unavailable: fall back to one combined widget.
    if (knitting) {
      .ns_montage_scene(specs, nrow, ncol)
      widget <- rgl::rglwidget()
      try(rgl::close3d(), silent = TRUE)
      return(widget)
    }
    warning("surface_montage(): could not produce static snapshots; ",
            "nothing was written to 'file'.")
    return(invisible(NULL))
  }

  # Interactive display.
  .ns_montage_scene(specs, nrow, ncol)
  invisible(NULL)
}
