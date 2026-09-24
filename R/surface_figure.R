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
#'   from `"lateral"`, `"medial"`, `"dorsal"`, `"ventral"`. Lateral and medial
#'   views are rows with one column per hemisphere. With both hemispheres,
#'   dorsal and ventral views show the hemispheres together in one panel,
#'   placed as a column beside the other views.
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
#' @param labels Draw hemisphere and view headers above the panels.
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
                           palette = NULL,
                           overlay_alpha = 1,
                           alpha_ramp = 0,
                           camera_mode = c("canonical", "presentation"),
                           cortex_mask = NULL,
                           legend = TRUE,
                           legend_title = NULL,
                           panel_width = 720,
                           panel_height = 450,
                           antialias = 3L,
                           labels = TRUE,
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

  if (is.null(palette)) palette <- .ns_default_palette(tail)

  render <- function(geometry, values, anatomy, cortex_mask, view, width,
                     height, pixel_scale) {
    render_surface_rgba(
      geometry,
      vertex_values = values,
      anatomy_metric = anatomy,
      cortex_mask = cortex_mask,
      camera = view,
      camera_mode = camera_mode,
      threshold = threshold,
      tail = tail,
      limits = limits,
      palette = palette,
      overlay_alpha = overlay_alpha,
      alpha_ramp = alpha_ramp,
      width = width,
      height = height,
      antialias = antialias,
      pixel_scale = pixel_scale,
      ...
    )
  }

  # With both hemispheres, dorsal and ventral views are single bilateral
  # panels placed as side columns spanning the lateral/medial rows; other
  # views are rows with one panel per hemisphere.
  bilateral <- length(geometries) == 2L
  side_views <- if (bilateral) intersect(views, c("dorsal", "ventral")) else
    character()
  row_views <- setdiff(views, side_views)
  n_rows <- max(1L, length(row_views))
  span_height <- n_rows * panel_height
  merged <- if (length(side_views)) {
    .ns_merge_hemispheres(geometries, values, anatomy, cortex_mask)
  }

  # One world-to-pixel scale for every panel, so the same cortex has the same
  # size in all views: the largest scale at which every panel fits.
  margin <- list(...)$margin %||% 0.04
  obliquity <- if (camera_mode == "presentation") {
    list(...)$presentation_obliquity %||% 7
  } else {
    0
  }
  fit <- function(vertices, view, hemi, width, height) {
    attr(.ns_project_surface_camera(vertices, view, hemi, width, height,
                                    margin, obliquity), "scale")
  }
  scales <- numeric()
  for (view in row_views) for (hemi in names(geometries)) {
    scales <- c(scales, fit(coords(geometries[[hemi]]), view, hemi,
                            panel_width, panel_height))
  }
  for (view in side_views) {
    scales <- c(scales, fit(coords(merged$geometry), view, "both",
                            1e9, span_height))
  }
  pixel_scale <- min(scales)

  panels <- list()
  placement <- list()
  col_widths <- rep(panel_width, if (length(row_views)) length(geometries)
                    else 0L)
  for (r in seq_along(row_views)) {
    view <- row_views[r]
    for (i in seq_along(geometries)) {
      hemi <- names(geometries)[i]
      name <- paste(hemi, view, sep = "_")
      panels[[name]] <- render(geometries[[hemi]], values[[hemi]],
                               anatomy[[hemi]], cortex_mask[[hemi]], view,
                               panel_width, panel_height, pixel_scale)
      placement[[name]] <- c(row_start = r, row_end = r,
                             col_start = i, col_end = i)
    }
  }
  for (view in side_views) {
    width <- .ns_fit_panel_width(coords(merged$geometry), view, pixel_scale,
                                 margin, span_height)
    col_widths <- c(col_widths, width)
    name <- paste("both", view, sep = "_")
    panels[[name]] <- render(merged$geometry, merged$values, merged$anatomy,
                             merged$cortex_mask, view, width, span_height,
                             pixel_scale)
    placement[[name]] <- c(row_start = 1L, row_end = n_rows,
                           col_start = length(col_widths),
                           col_end = length(col_widths))
  }
  row_labels <- if (isTRUE(labels) && length(row_views)) {
    c(lateral = "Lateral", medial = "Medial", dorsal = "Dorsal",
      ventral = "Ventral")[row_views]
  }
  headers <- if (isTRUE(labels)) {
    c(if (length(row_views)) {
        c(lh = "Left", rh = "Right")[names(geometries)]
      },
      c(dorsal = "Dorsal", ventral = "Ventral")[side_views])
  }

  structure(
    list(
      panels = panels,
      placement = placement,
      nrow = n_rows,
      ncol = length(col_widths),
      hemis = names(geometries),
      views = views,
      panel_width = as.integer(panel_width),
      panel_height = as.integer(panel_height),
      col_widths = as.integer(col_widths),
      row_heights = rep(as.integer(panel_height), n_rows),
      headers = unname(headers),
      row_labels = unname(row_labels),
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

# Width of a bilateral panel that holds the projected surface at
# `pixel_scale` with the renderer's margin, never narrower than needed to
# keep the panel's aspect sensible.
#' @noRd
.ns_fit_panel_width <- function(vertices, view, pixel_scale, margin,
                                height) {
  view_x <- vertices %*% .ns_camera_basis(view, "lh")[, 1]
  width <- diff(range(view_x)) * pixel_scale / (1 - 2 * margin)
  as.integer(ceiling(max(width, 0.25 * height)))
}

# Place two hemispheres side by side in one geometry for bilateral views.
# The meshes are translated apart along x with a fixed gap so that inflated
# surfaces, which overlap in native coordinates, do not interpenetrate.
#' @noRd
.ns_merge_hemispheres <- function(geometries, values, anatomy, cortex_mask) {
  lh <- coords(geometries$lh)
  rh <- coords(geometries$rh)
  gap <- 0.04 * max(diff(range(lh[, 1])), diff(range(rh[, 1])))
  lh[, 1] <- lh[, 1] - max(lh[, 1]) - gap / 2
  rh[, 1] <- rh[, 1] - min(rh[, 1]) + gap / 2
  faces <- rbind(faces(geometries$lh) - 1L,
                 faces(geometries$rh) - 1L + nrow(lh))
  fill <- function(x, geometry) {
    if (is.null(x)) rep(NA_real_, nrow(coords(geometry))) else x
  }
  merged_anatomy <- if (is.null(anatomy$lh) && is.null(anatomy$rh)) NULL else
    c(fill(anatomy$lh, geometries$lh), fill(anatomy$rh, geometries$rh))
  merged_mask <- if (is.null(cortex_mask$lh) && is.null(cortex_mask$rh)) NULL else
    c(cortex_mask$lh %||% rep(TRUE, nrow(lh)),
      cortex_mask$rh %||% rep(TRUE, nrow(rh)))
  list(
    geometry = SurfaceGeometry(rbind(lh, rh), faces, hemi = "both"),
    values = c(values$lh, values$rh),
    anatomy = merged_anatomy,
    cortex_mask = merged_mask
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

# Build the complete figure grob in one layout, so strips stay aligned with
# the aspect-preserving panel grid: an optional header row and row-label
# column (millimetre units) around the panel cells, and an optional colour
# bar row underneath. Bilateral panels span all panel rows.
#' @noRd
.ns_surface_figure_grob <- function(x) {
  has_header <- length(x$headers) > 0L
  has_rows <- length(x$row_labels) > 0L
  has_legend <- isTRUE(x$legend)
  heights <- grid::unit.c(
    grid::unit(if (has_header) .ns_header_height_mm else 0, "mm"),
    grid::unit(x$row_heights, "null"),
    grid::unit(if (has_legend) .ns_legend_height_mm else 0, "mm")
  )
  widths <- grid::unit.c(
    grid::unit(if (has_rows) .ns_row_label_width_mm else 0, "mm"),
    grid::unit(x$col_widths, "null")
  )
  frame <- grid::frameGrob(layout = grid::grid.layout(
    nrow = x$nrow + 2L, ncol = x$ncol + 1L,
    widths = widths, heights = heights, respect = TRUE
  ))
  for (name in names(x$panels)) {
    at <- x$placement[[name]]
    frame <- grid::placeGrob(
      frame,
      grid::rasterGrob(.ns_figure_panel_raster(x$panels[[name]]),
                       interpolate = TRUE),
      row = 1L + (at[["row_start"]]:at[["row_end"]]),
      col = 1L + (at[["col_start"]]:at[["col_end"]])
    )
  }
  label_gp <- grid::gpar(fontsize = 9, col = "#555555")
  for (i in seq_along(x$headers)) {
    frame <- grid::placeGrob(frame, grid::textGrob(
      x$headers[i], y = grid::unit(1.2, "mm"), just = c("centre", "bottom"),
      gp = label_gp
    ), row = 1L, col = i + 1L)
  }
  for (r in seq_along(x$row_labels)) {
    frame <- grid::placeGrob(frame, grid::textGrob(
      x$row_labels[r], rot = 90, x = grid::unit(1, "npc") - grid::unit(1, "mm"),
      just = c("centre", "bottom"), gp = label_gp
    ), row = r + 1L, col = 1L)
  }
  if (has_legend) {
    frame <- grid::placeGrob(
      frame, .ns_figure_colorbar_grob(x$scale, x$legend_title),
      row = x$nrow + 2L, col = 2:(x$ncol + 1L)
    )
  }
  frame
}

.ns_row_label_width_mm <- 5
.ns_legend_height_mm <- 15
.ns_header_height_mm <- 6

# Suprathreshold colour-bar segments as value intervals.
#' @noRd
.ns_colorbar_segments <- function(scale) {
  lim <- scale$limits
  thr <- scale$threshold
  segs <- switch(scale$tail,
    two_sided = if (thr > 0) {
      list(c(lim[1], -thr), c(thr, lim[2]))
    } else {
      list(lim)
    },
    positive = list(c(max(thr, lim[1]), lim[2])),
    negative = list(c(lim[1], min(-thr, lim[2])))
  )
  Filter(function(s) s[2] > s[1], segs)
}

#' @noRd
.ns_format_tick <- function(v) {
  sub("^-", "\u2212", trimws(formatC(v, format = "g", digits = 3)))
}

#' @noRd
.ns_figure_colorbar_grob <- function(scale, title) {
  lut <- .ns_overlay_lut(scale$palette, scale$limits, scale$threshold,
                         scale$tail, 256L)
  segs <- .ns_colorbar_segments(scale)
  total_w <- 0.40
  gap <- if (length(segs) > 1L) 0.03 else 0
  span <- vapply(segs, diff, numeric(1))
  seg_w <- (total_w - gap * (length(segs) - 1L)) * span / sum(span)
  bar_top <- grid::unit(1, "npc") - grid::unit(2, "mm")
  bar_h <- grid::unit(2.6, "mm")
  label_y <- bar_top - bar_h - grid::unit(1.4, "mm")
  children <- list()
  x0 <- 0.5 - total_w / 2
  lut_v <- seq(scale$limits[1], scale$limits[2], length.out = nrow(lut))
  for (k in seq_along(segs)) {
    seg <- segs[[k]]
    v <- seq(seg[1], seg[2], length.out = 128L)
    idx <- findInterval(v, lut_v, all.inside = TRUE)
    cols <- grDevices::rgb(lut[idx, 1], lut[idx, 2], lut[idx, 3])
    children[[length(children) + 1L]] <- grid::rasterGrob(
      grDevices::as.raster(matrix(cols, nrow = 1L)),
      x = x0, y = bar_top, width = seg_w[k], height = bar_h,
      just = c("left", "top"), interpolate = TRUE
    )
    # Labels at a gap between segments are aligned away from the gap so
    # the two threshold labels never collide.
    ends <- c(x0, x0 + seg_w[k])
    hjust <- c(if (k > 1L) 0 else 0.5, if (k < length(segs)) 1 else 0.5)
    for (j in 1:2) {
      children[[length(children) + 1L]] <- grid::textGrob(
        .ns_format_tick(seg[j]), x = ends[j], y = label_y,
        hjust = hjust[j], vjust = 1,
        gp = grid::gpar(fontsize = 8, col = "#333333")
      )
    }
    x0 <- x0 + seg_w[k] + gap
  }
  if (!is.null(title) && nzchar(title)) {
    children[[length(children) + 1L]] <- grid::textGrob(
      title, x = 0.5, y = label_y - grid::unit(3.8, "mm"),
      just = c("centre", "top"),
      gp = grid::gpar(fontsize = 9, col = "#222222")
    )
  }
  grid::grobTree(children = do.call(grid::gList, children))
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
  panel_width <- scale * sum(x$col_widths)
  # Text and colour-bar sizes are set in points and millimetres; choose the
  # resolution so the panels lay out as a 7-inch-wide page figure.
  res <- panel_width / 7
  mm <- function(v) v / 25.4 * res
  width <- round(panel_width +
                   mm(if (length(x$row_labels)) .ns_row_label_width_mm else 0))
  height <- round(scale * sum(x$row_heights) +
                    mm((if (isTRUE(x$legend)) .ns_legend_height_mm else 0) +
                         (if (length(x$headers)) .ns_header_height_mm else 0)))
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(file, width = width, height = height, units = "px",
                  res = res, background = "white")
  } else {
    grDevices::png(file, width = width, height = height, res = res,
                   bg = "white")
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.draw(.ns_surface_figure_grob(x))
  invisible(normalizePath(file, mustWork = FALSE))
}
