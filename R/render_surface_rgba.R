#' Deterministic scalar-first surface rasterization
#'
#' Rasterizes a cortical triangle mesh with a per-pixel z-buffer and
#' barycentric interpolation of the scalar field. Thresholding and palette
#' mapping occur after scalar interpolation at each sample. The backend is a
#' CPU implementation and requires neither OpenGL nor a browser.
#'
#' @param geometry A [SurfaceGeometry] object.
#' @param vertex_values Numeric value per vertex.
#' @param anatomy_metric Optional numeric anatomy metric per vertex. Values are
#'   robustly scaled to [0, 1] and modulate a quiet grey substrate.
#' @param cortex_mask Logical cortex-domain mask per vertex. Overlay color is
#'   never painted on triangles touching a masked vertex.
#' @param camera One of `"lateral"`, `"medial"`, `"dorsal"`, or `"ventral"`.
#' @param camera_mode Strict canonical orthographic output or a presentation
#'   camera with a small, explicit obliquity.
#' @param presentation_obliquity Obliquity in degrees used only for
#'   `camera_mode = "presentation"`.
#' @param width,height Output dimensions in pixels.
#' @param threshold Non-negative absolute scalar threshold.
#' @param tail Threshold tail: two-sided, positive, or negative.
#' @param palette Character vector of at least two colors.
#' @param limits Numeric scalar-color limits.
#' @param overlay_alpha Maximum overlay opacity.
#' @param alpha_ramp Width of the opacity ramp above threshold.
#' @param antialias Integer supersampling factor.
#' @param margin Fractional panel margin.
#' @param medial_wall Whether masked-domain triangles are neutrally shaded,
#'   omitted, or independently outlined.
#' @param outer_contour Draw a one-pixel contour only where covered cortex
#'   touches background connected to the image exterior. Enclosed holes and
#'   depth discontinuities are not treated as outer contour.
#' @param outer_contour_color Contour color.
#' @param background Background color.
#' @param return_buffers Include scalar and depth buffers for diagnostics.
#'
#' @return A `surface_rgba` list with raw RGBA, coverage, and overlay-alpha
#'   arrays plus camera and rendering provenance.
#' @export
render_surface_rgba <- function(geometry,
                                vertex_values,
                                anatomy_metric = NULL,
                                cortex_mask = NULL,
                                camera = c("lateral", "medial", "dorsal",
                                           "ventral"),
                                camera_mode = c("canonical", "presentation"),
                                presentation_obliquity = 7,
                                width = 1200L,
                                height = 750L,
                                threshold = 0,
                                tail = c("two_sided", "positive", "negative"),
                                palette = c("#3B4CC0", "#F7F7F7", "#B40426"),
                                limits = NULL,
                                overlay_alpha = 0.85,
                                alpha_ramp = 0,
                                antialias = 2L,
                                margin = 0.04,
                                medial_wall = c("shade", "mask", "outline"),
                                outer_contour = TRUE,
                                outer_contour_color = "#595959",
                                background = "#FBFBF8",
                                return_buffers = FALSE) {
  if (!inherits(geometry, "SurfaceGeometry")) {
    stop("'geometry' must be a SurfaceGeometry object.", call. = FALSE)
  }
  camera <- match.arg(camera)
  camera_mode <- match.arg(camera_mode)
  tail <- match.arg(tail)
  medial_wall <- match.arg(medial_wall)
  vertices <- coords(geometry)
  n <- nrow(vertices)
  if (!is.numeric(vertex_values) || length(vertex_values) != n) {
    stop("'vertex_values' must be numeric with one value per vertex.",
         call. = FALSE)
  }
  if (is.null(cortex_mask)) cortex_mask <- rep(TRUE, n)
  if (!is.logical(cortex_mask) || length(cortex_mask) != n ||
      anyNA(cortex_mask)) {
    stop("'cortex_mask' must be a non-missing logical value per vertex.",
         call. = FALSE)
  }
  anatomy <- .ns_scale_anatomy_metric(anatomy_metric, n)
  if (is.null(limits)) {
    finite <- vertex_values[is.finite(vertex_values)]
    limits <- if (length(finite)) range(finite) else c(-1, 1)
    if (limits[1] == limits[2]) limits <- limits + c(-1, 1) * 1e-8
  }
  if (!is.numeric(limits) || length(limits) != 2L ||
      any(!is.finite(limits)) || limits[1] >= limits[2]) {
    stop("'limits' must be two finite increasing numbers.", call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      !is.finite(threshold) || threshold < 0) {
    stop("'threshold' must be a non-negative numeric scalar.", call. = FALSE)
  }
  if (!is.numeric(overlay_alpha) || length(overlay_alpha) != 1L ||
      !is.finite(overlay_alpha) || overlay_alpha < 0 || overlay_alpha > 1) {
    stop("'overlay_alpha' must be in [0, 1].", call. = FALSE)
  }
  antialias <- as.integer(antialias)
  if (length(antialias) != 1L || is.na(antialias) || antialias < 1L) {
    stop("'antialias' must be a positive integer.", call. = FALSE)
  }

  projected <- .ns_project_surface_camera(
    vertices, camera = camera, hemi = geometry@hemi,
    width = width, height = height, margin = margin,
    presentation_obliquity = if (camera_mode == "presentation") {
      presentation_obliquity
    } else {
      0
    }
  )
  faces <- t(geometry@mesh$it)
  palette_rgba <- .ns_palette_matrix(palette, 256L)
  bg <- grDevices::col2rgb(background) / 255
  tail_code <- switch(tail, two_sided = 0L, positive = 1L, negative = -1L)

  out <- cpp_rasterize_surface_scalar(
    projected = projected,
    faces = faces,
    values = as.numeric(vertex_values),
    anatomy = anatomy,
    cortex_mask = cortex_mask,
    width = as.integer(width),
    height = as.integer(height),
    threshold = threshold,
    tail = tail_code,
    limits = limits,
    palette = palette_rgba,
    overlay_alpha = overlay_alpha,
    alpha_ramp = alpha_ramp,
    base_low = 0.72,
    base_high = 0.90,
    medial_wall_policy = if (medial_wall == "mask") 1L else 0L,
    background = as.numeric(bg[, 1L]),
    supersample = antialias,
    return_buffers = isTRUE(return_buffers)
  )
  if (isTRUE(outer_contour)) {
    edge <- .ns_outer_contour_mask(out$coverage)
    col <- as.raw(grDevices::col2rgb(outer_contour_color, alpha = TRUE)[, 1L])
    for (channel in seq_len(4L)) {
      plane <- out$rgba[, , channel]
      plane[edge] <- col[[channel]]
      out$rgba[, , channel] <- plane
    }
  }
  if (identical(medial_wall, "outline")) {
    wall_edge <- .ns_domain_boundary_mask(out$cortex_coverage, out$coverage)
    for (channel in seq_len(3L)) {
      plane <- out$rgba[, , channel]
      plane[wall_edge] <- as.raw(90L)
      out$rgba[, , channel] <- plane
    }
  }
  out$camera <- list(
    view = camera,
    projection = paste0(camera_mode, "_orthographic"),
    presentation_obliquity = if (camera_mode == "presentation") {
      presentation_obliquity
    } else {
      0
    },
    hemi = geometry@hemi,
    margin = margin
  )
  out$provenance <- list(
    backend = "cpu_barycentric",
    scalar_interpolation = "barycentric",
    depth_test = "per_sample_z_buffer",
    antialias = antialias,
    threshold = threshold,
    tail = tail,
    limits = limits,
    medial_wall = medial_wall
  )
  class(out) <- c("surface_rgba", "list")
  out
}

.ns_domain_boundary_mask <- function(cortex_coverage, coverage) {
  wall <- coverage & !cortex_coverage
  nr <- nrow(coverage)
  nc <- ncol(coverage)
  adjacent_wall <- matrix(FALSE, nr, nc)
  adjacent_wall[-1L, ] <- adjacent_wall[-1L, ] | wall[-nr, ]
  adjacent_wall[-nr, ] <- adjacent_wall[-nr, ] | wall[-1L, ]
  adjacent_wall[, -1L] <- adjacent_wall[, -1L] | wall[, -nc]
  adjacent_wall[, -nc] <- adjacent_wall[, -nc] | wall[, -1L]
  cortex_coverage & adjacent_wall
}

.ns_outer_contour_mask <- function(coverage) {
  nr <- nrow(coverage)
  nc <- ncol(coverage)
  exterior <- matrix(FALSE, nr, nc)
  queue_r <- integer(nr * nc)
  queue_c <- integer(nr * nc)
  head <- 1L
  tail <- 0L
  enqueue <- function(r, c) {
    if (r < 1L || r > nr || c < 1L || c > nc || coverage[r, c] ||
        exterior[r, c]) return(invisible(NULL))
    tail <<- tail + 1L
    queue_r[[tail]] <<- r
    queue_c[[tail]] <<- c
    exterior[r, c] <<- TRUE
    invisible(NULL)
  }
  for (c in seq_len(nc)) {
    enqueue(1L, c)
    enqueue(nr, c)
  }
  for (r in seq_len(nr)) {
    enqueue(r, 1L)
    enqueue(r, nc)
  }
  while (head <= tail) {
    r <- queue_r[[head]]
    c <- queue_c[[head]]
    head <- head + 1L
    enqueue(r - 1L, c)
    enqueue(r + 1L, c)
    enqueue(r, c - 1L)
    enqueue(r, c + 1L)
  }
  neighbor_exterior <- matrix(FALSE, nr, nc)
  neighbor_exterior[-1L, ] <- neighbor_exterior[-1L, ] | exterior[-nr, ]
  neighbor_exterior[-nr, ] <- neighbor_exterior[-nr, ] | exterior[-1L, ]
  neighbor_exterior[, -1L] <- neighbor_exterior[, -1L] | exterior[, -nc]
  neighbor_exterior[, -nc] <- neighbor_exterior[, -nc] | exterior[, -1L]
  coverage & neighbor_exterior
}

#' Write a deterministic surface raster to PNG
#'
#' @param x A `surface_rgba` object.
#' @param file Output PNG path.
#' @return The normalized path, invisibly.
#' @export
write_surface_rgba <- function(x, file) {
  if (!inherits(x, "surface_rgba")) {
    stop("'x' must be a surface_rgba object.", call. = FALSE)
  }
  rgba <- array(as.numeric(x$rgba) / 255, dim = dim(x$rgba))
  png::writePNG(rgba, target = file)
  invisible(normalizePath(file, mustWork = TRUE))
}

#' Marching-triangle threshold segments
#'
#' Computes exact linear threshold crossings from the same vertex-scalar model
#' used by [render_surface_rgba()]. Positive and negative levels are handled as
#' independent regions for two-sided maps.
#'
#' @param vertices N by 2 or N by 3 projected vertex coordinates.
#' @param faces F by 3 one-based vertex indices.
#' @param values Numeric scalar per vertex.
#' @param threshold Positive threshold magnitude.
#' @param tail Two-sided, positive, or negative levels.
#' @return A data frame with two rows per threshold segment.
#' @export
surface_threshold_segments <- function(vertices, faces, values, threshold,
                                       tail = c("two_sided", "positive",
                                                "negative")) {
  tail <- match.arg(tail)
  vertices <- as.matrix(vertices)
  faces <- as.matrix(faces)
  if (ncol(vertices) < 2L || ncol(faces) != 3L ||
      length(values) != nrow(vertices)) {
    stop("vertices, faces, and values have incompatible dimensions.",
         call. = FALSE)
  }
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      !is.finite(threshold) || threshold <= 0) {
    stop("'threshold' must be a positive numeric scalar.", call. = FALSE)
  }
  levels <- switch(tail, two_sided = c(-threshold, threshold),
                   positive = threshold, negative = -threshold)
  edges <- rbind(c(1L, 2L), c(2L, 3L), c(3L, 1L))
  out <- list()
  segment_id <- 0L
  for (face_id in seq_len(nrow(faces))) {
    ids <- faces[face_id, ]
    fv <- values[ids]
    if (any(!is.finite(fv))) next
    for (level in levels) {
      points <- list()
      for (edge_id in seq_len(nrow(edges))) {
        a <- edges[edge_id, 1L]
        b <- edges[edge_id, 2L]
        va <- fv[[a]]
        vb <- fv[[b]]
        if (va == vb || (va - level) * (vb - level) > 0) next
        fraction <- (level - va) / (vb - va)
        if (fraction < 0 || fraction > 1) next
        points[[length(points) + 1L]] <-
          vertices[ids[[a]], , drop = TRUE] + fraction *
          (vertices[ids[[b]], , drop = TRUE] -
             vertices[ids[[a]], , drop = TRUE])
      }
      if (length(points) < 2L) next
      pts <- unique(round(do.call(rbind, points), 14L))
      if (nrow(pts) < 2L) next
      segment_id <- segment_id + 1L
      out[[length(out) + 1L]] <- data.frame(
        x = pts[1:2, 1L],
        y = pts[1:2, 2L],
        z = if (ncol(pts) >= 3L) pts[1:2, 3L] else NA_real_,
        level = level,
        face_id = face_id,
        segment_id = segment_id
      )
    }
  }
  if (!length(out)) {
    return(data.frame(x = numeric(), y = numeric(), z = numeric(),
                      level = numeric(), face_id = integer(),
                      segment_id = integer()))
  }
  do.call(rbind, out)
}

.ns_scale_anatomy_metric <- function(metric, n) {
  if (is.null(metric)) return(rep(0.65, n))
  if (!is.numeric(metric) || length(metric) != n) {
    stop("'anatomy_metric' must be numeric with one value per vertex.",
         call. = FALSE)
  }
  finite <- is.finite(metric)
  if (!any(finite)) return(rep(0.65, n))
  lim <- stats::quantile(metric[finite], c(0.02, 0.98), names = FALSE,
                         type = 8)
  if (!is.finite(diff(lim)) || diff(lim) <= 0) {
    out <- rep(0.65, n)
  } else {
    out <- (metric - lim[1L]) / diff(lim)
    out <- pmax(0, pmin(1, out))
    out[!finite] <- 0.65
  }
  out
}

.ns_palette_matrix <- function(colors, n) {
  if (!is.character(colors) || length(colors) < 2L) {
    stop("'palette' must contain at least two colors.", call. = FALSE)
  }
  rgb <- grDevices::col2rgb(grDevices::colorRampPalette(colors)(n),
                            alpha = TRUE) / 255
  t(rgb)
}

.ns_project_surface_camera <- function(vertices, camera, hemi, width, height,
                                       margin = 0.04,
                                       presentation_obliquity = 0) {
  left <- hemi %in% c("lh", "left", "L")
  if (presentation_obliquity != 0 && camera %in% c("lateral", "medial")) {
    direction <- if (camera == "lateral") 1 else -1
    if (!left) direction <- -direction
    theta <- direction * presentation_obliquity * pi / 180
    x <- vertices[, 1] * cos(theta) - vertices[, 2] * sin(theta)
    y <- vertices[, 1] * sin(theta) + vertices[, 2] * cos(theta)
    vertices[, 1] <- x
    vertices[, 2] <- y
  }
  if (camera == "lateral") {
    view_dir <- if (left) c(-1, 0, 0) else c(1, 0, 0)
    xy <- cbind(if (left) -vertices[, 2] else vertices[, 2], vertices[, 3])
  } else if (camera == "medial") {
    view_dir <- if (left) c(1, 0, 0) else c(-1, 0, 0)
    xy <- cbind(if (left) -vertices[, 2] else vertices[, 2], vertices[, 3])
  } else if (camera == "dorsal") {
    view_dir <- c(0, 0, 1)
    xy <- cbind(vertices[, 1], vertices[, 2])
  } else {
    view_dir <- c(0, 0, -1)
    xy <- cbind(vertices[, 1], -vertices[, 2])
  }
  xr <- range(xy[, 1], finite = TRUE)
  yr <- range(xy[, 2], finite = TRUE)
  avail_w <- width * (1 - 2 * margin)
  avail_h <- height * (1 - 2 * margin)
  scale <- min(avail_w / max(diff(xr), .Machine$double.eps),
               avail_h / max(diff(yr), .Machine$double.eps))
  x <- (xy[, 1] - mean(xr)) * scale + width / 2
  y <- height / 2 - (xy[, 2] - mean(yr)) * scale
  depth <- as.numeric(vertices %*% view_dir)
  cbind(x, y, depth)
}
