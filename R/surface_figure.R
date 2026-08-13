#' Static multi-view surface figure with a shared colour scale
#'
#' Renders one or both hemispheres from several canonical views with
#' [render_surface_rgba()] and arranges the panels into a single figure with
#' an optional colour bar. All panels share the same threshold, limits, and
#' palette, so the figure carries one interpretable colour scale.
#'
#' This is the package's high-level entry point for a static, publication
#' figure. It runs headlessly (CI, cluster, Quarto, PDF) and requires neither
#' OpenGL nor a browser. `plot()` and `print()` draw the figure on the current
#' graphics device; [write_surface_figure()] writes it to PNG.
#'
#' @param lh,rh [SurfaceGeometry] objects for the hemispheres to draw. Supply
#'   at least one.
#' @param values Vertex values. With both hemispheres, a named list with
#'   elements `lh`/`rh` (or `left`/`right`); with one hemisphere, a numeric
#'   vector or a one-element named list.
#' @param anatomy Optional anatomy metric (for example curvature from a
#'   matching white surface), in the same form as `values`.
#' @param views Character vector of camera views drawn for each hemisphere,
#'   from `"lateral"`, `"medial"`, `"dorsal"`, `"ventral"`. Views are rows of
#'   the figure; hemispheres are columns.
#' @param threshold,tail,limits,palette,overlay_alpha,alpha_ramp,camera_mode
#'   Shared rendering contract applied to every panel; see
#'   [render_surface_rgba()]. `limits` defaults to the finite range of
#'   `values` across hemispheres.
#' @param cortex_mask Optional cortex-domain mask, in the same form as
#'   `values`.
#' @param legend Draw a colour bar beneath the panels.
#' @param legend_title Text under the colour bar, typically the statistic and
#'   its units.
#' @param panel_width,panel_height Pixel dimensions of each rendered panel.
#' @param antialias Integer supersampling factor per panel.
#' @param ... Additional arguments passed to every [render_surface_rgba()]
#'   call.
#'
#' @return A `surface_figure` object: the rendered `surface_rgba` panels plus
#'   layout and colour-scale metadata. `plot()` draws it and invisibly
#'   returns it.
#'
#' @examples
#' \donttest{
#' fs <- load_fsaverage_std8("inflated")
#' stat <- lapply(fs[c("lh", "rh")], function(g) coords(g)[, 3] / 10)
#' fig <- surface_figure(
#'   lh = fs$lh, rh = fs$rh,
#'   values = stat,
#'   threshold = 1, limits = c(-3, 3),
#'   legend_title = "z",
#'   panel_width = 300, panel_height = 200
#' )
#' plot(fig)
#' }
#'
#' @seealso [render_surface_rgba()] for the single-panel contract,
#'   [write_surface_figure()] to write PNG output, and [surfwidget()] for the
#'   interactive HTML counterpart.
#' @export
surface_figure <- function(lh = NULL,
                           rh = NULL,
                           values,
                           anatomy = NULL,
                           views = c("lateral", "medial"),
                           threshold = 0,
                           tail = c("two_sided", "positive", "negative"),
                           limits = NULL,
                           palette = c("#3B4CC0", "#F7F7F7", "#B40426"),
                           overlay_alpha = 0.85,
                           alpha_ramp = 0,
                           camera_mode = c("canonical", "presentation"),
                           cortex_mask = NULL,
                           legend = TRUE,
                           legend_title = NULL,
                           panel_width = 720,
                           panel_height = 450,
                           antialias = 2L,
                           ...) {
  tail <- match.arg(tail)
  camera_mode <- match.arg(camera_mode)
  views <- match.arg(views, c("lateral", "medial", "dorsal", "ventral"),
                     several.ok = TRUE)

  geometries <- list()
  if (!is.null(lh)) geometries$lh <- lh
  if (!is.null(rh)) geometries$rh <- rh
  if (!length(geometries)) {
    stop("Supply at least one of 'lh' or 'rh'.", call. = FALSE)
  }
  for (hemi in names(geometries)) {
    if (!inherits(geometries[[hemi]], "SurfaceGeometry")) {
      stop("'", hemi, "' must be a SurfaceGeometry object.", call. = FALSE)
    }
  }

  values <- .ns_figure_per_hemi(values, geometries, "values",
                                required = TRUE)
  anatomy <- .ns_figure_per_hemi(anatomy, geometries, "anatomy",
                                 required = FALSE)
  cortex_mask <- .ns_figure_per_hemi(cortex_mask, geometries, "cortex_mask",
                                     required = FALSE)

  if (is.null(limits)) {
    finite <- unlist(lapply(values, function(v) v[is.finite(v)]),
                     use.names = FALSE)
    if (!length(finite)) {
      stop("'values' contains no finite values; supply 'limits'.",
           call. = FALSE)
    }
    limits <- range(finite)
    if (limits[1] == limits[2]) limits <- limits + c(-1, 1) * 1e-8
  }

  panels <- list()
  for (view in views) {
    for (hemi in names(geometries)) {
      panels[[paste(hemi, view, sep = "_")]] <- render_surface_rgba(
        geometries[[hemi]],
        vertex_values = values[[hemi]],
        anatomy_metric = anatomy[[hemi]],
        cortex_mask = cortex_mask[[hemi]],
        camera = view,
        camera_mode = camera_mode,
        threshold = threshold,
        tail = tail,
        limits = limits,
        palette = palette,
        overlay_alpha = overlay_alpha,
        alpha_ramp = alpha_ramp,
        width = panel_width,
        height = panel_height,
        antialias = antialias,
        ...
      )
    }
  }

  structure(
    list(
      panels = panels,
      nrow = length(views),
      ncol = length(geometries),
      hemis = names(geometries),
      views = views,
      panel_width = as.integer(panel_width),
      panel_height = as.integer(panel_height),
      scale = list(
        limits = limits,
        threshold = threshold,
        tail = tail,
        palette = palette
      ),
      legend = isTRUE(legend),
      legend_title = legend_title
    ),
    class = c("surface_figure", "list")
  )
}

# Normalize per-hemisphere input: accept lh/rh or left/right list names, or a
# bare vector when a single hemisphere is drawn.
#' @noRd
.ns_figure_per_hemi <- function(x, geometries, what, required) {
  hemis <- names(geometries)
  if (is.null(x)) {
    if (required) stop("'", what, "' is required.", call. = FALSE)
    return(stats::setNames(vector("list", length(hemis)), hemis))
  }
  if (!is.list(x)) {
    if (length(hemis) != 1L) {
      stop("With both hemispheres, '", what,
           "' must be a named list with elements lh/rh (or left/right).",
           call. = FALSE)
    }
    x <- stats::setNames(list(x), hemis)
  }
  names(x)[names(x) == "left"] <- "lh"
  names(x)[names(x) == "right"] <- "rh"
  missing <- setdiff(hemis, names(x))
  if (length(missing)) {
    stop("'", what, "' is missing element(s): ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  for (hemi in hemis) {
    n <- nrow(coords(geometries[[hemi]]))
    if (!is.null(x[[hemi]]) && length(x[[hemi]]) != n) {
      stop("'", what, "$", hemi, "' has ", length(x[[hemi]]),
           " values for ", n, " vertices.", call. = FALSE)
    }
  }
  x[hemis]
}

#' @noRd
.ns_figure_panel_raster <- function(panel) {
  rgba <- panel$rgba
  grDevices::as.raster(array(as.numeric(rgba) / 255, dim = dim(rgba)))
}

# Build the complete figure grob: a hemisphere-by-view panel grid with an
# optional colour bar strip underneath.
#' @noRd
.ns_surface_figure_grob <- function(x) {
  layout <- grid::grid.layout(
    nrow = x$nrow, ncol = x$ncol,
    widths = grid::unit(rep(x$panel_width, x$ncol), "null"),
    heights = grid::unit(rep(x$panel_height, x$nrow), "null"),
    respect = TRUE
  )
  frame <- grid::frameGrob(layout = layout)
  k <- 0L
  for (row in seq_len(x$nrow)) {
    for (col in seq_len(x$ncol)) {
      k <- k + 1L
      frame <- grid::placeGrob(
        frame,
        grid::rasterGrob(.ns_figure_panel_raster(x$panels[[k]]),
                         interpolate = TRUE),
        row = row, col = col
      )
    }
  }
  if (!isTRUE(x$legend)) return(frame)

  outer <- grid::frameGrob(layout = grid::grid.layout(
    nrow = 2L, ncol = 1L,
    heights = grid::unit.c(grid::unit(1, "null"), grid::unit(11, "mm"))
  ))
  outer <- grid::placeGrob(outer, frame, row = 1L, col = 1L)
  grid::placeGrob(outer, .ns_figure_colorbar_grob(x$scale, x$legend_title),
                  row = 2L, col = 1L)
}

#' @noRd
.ns_figure_colorbar_grob <- function(scale, title) {
  ramp <- grDevices::colorRampPalette(scale$palette)(256L)
  bar <- grid::rasterGrob(
    grDevices::as.raster(matrix(ramp, nrow = 1L)),
    x = 0.5, y = 1, height = grid::unit(3, "mm"), width = 0.5,
    just = c("center", "top"), interpolate = TRUE
  )
  ticks <- unique(c(scale$limits[1], 0, scale$limits[2]))
  ticks <- ticks[ticks >= scale$limits[1] & ticks <= scale$limits[2]]
  tick_x <- 0.25 + 0.5 * (ticks - scale$limits[1]) /
    diff(scale$limits)
  labels <- grid::textGrob(
    formatC(ticks, format = "g", digits = 3),
    x = tick_x, y = grid::unit(1, "npc") - grid::unit(4.5, "mm"),
    just = c("center", "top"), gp = grid::gpar(cex = 0.7)
  )
  children <- grid::gList(bar, labels)
  if (!is.null(title) && nzchar(title)) {
    children <- grid::gList(
      children,
      grid::textGrob(
        title,
        x = 0.5, y = grid::unit(1, "npc") - grid::unit(8.5, "mm"),
        just = c("center", "top"), gp = grid::gpar(cex = 0.8)
      )
    )
  }
  grid::grobTree(children = children)
}

#' @rdname surface_figure
#' @param x A `surface_figure` object.
#' @method plot surface_figure
#' @export
plot.surface_figure <- function(x, ...) {
  grid::grid.newpage()
  grid::grid.draw(.ns_surface_figure_grob(x))
  invisible(x)
}

#' @rdname surface_figure
#' @method print surface_figure
#' @export
print.surface_figure <- function(x, ...) {
  plot(x, ...)
}

#' Write a surface figure to PNG
#'
#' Draws a [surface_figure()] on a PNG device sized to its panels. Uses the
#' `ragg` device when available, otherwise [grDevices::png()].
#'
#' @param x A `surface_figure` object.
#' @param file Output PNG path.
#' @param scale Multiplier applied to the figure's pixel dimensions.
#' @return The normalized path, invisibly.
#'
#' @seealso [surface_figure()], [write_surface_rgba()] for single panels.
#' @export
write_surface_figure <- function(x, file, scale = 1) {
  if (!inherits(x, "surface_figure")) {
    stop("'x' must be a surface_figure object.", call. = FALSE)
  }
  if (!is.numeric(scale) || length(scale) != 1L || !is.finite(scale) ||
      scale <= 0) {
    stop("'scale' must be a positive number.", call. = FALSE)
  }
  legend_px <- if (isTRUE(x$legend)) round(0.09 * x$nrow * x$panel_height)
               else 0L
  width <- round(scale * x$ncol * x$panel_width)
  height <- round(scale * (x$nrow * x$panel_height + legend_px))
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(file, width = width, height = height, units = "px")
  } else {
    grDevices::png(file, width = width, height = height)
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.draw(.ns_surface_figure_grob(x))
  invisible(normalizePath(file, mustWork = FALSE))
}
