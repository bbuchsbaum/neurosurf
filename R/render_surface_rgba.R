#' Deterministic scalar-first surface rasterization
#'
#' Rasterizes a cortical triangle mesh with a per-sample z-buffer and
#' barycentric interpolation of the scalar field. Thresholding and palette
#' mapping occur after scalar interpolation at each sample. Surfaces are lit
#' with interpolated (Phong) normals, and coarse meshes are subdivided for
#' display. The backend is a CPU implementation and requires neither OpenGL
#' nor a browser.
#'
#' Display subdivision uses the interpolating modified-butterfly scheme:
#' original vertices keep their positions and values, and each new vertex's
#' scalar value is clamped to the range of its edge's endpoints, so
#' subdivision smooths threshold contours without creating suprathreshold
#' regions that linear interpolation would not.
#'
#' @param geometry A [SurfaceGeometry] object.
#' @param vertex_values Numeric value per vertex.
#' @param anatomy_metric Optional numeric anatomy metric per vertex, such as
#'   sulcal depth or curvature. Values are robustly scaled and mapped to the
#'   grey levels in `anatomy_range`, with sulci darker than gyri.
#' @param anatomy_style Underlay mapping: legacy publication shading, centered
#'   continuous contrast, or binary folding contrast.
#' @param anatomy_midpoint Passed to [normalize_surface_anatomy()].
#' @param anatomy_invert Reverse the metric's polarity. `"auto"` (default)
#'   inverts metrics that are larger in sulci (FreeSurfer `sulc` and `curv`)
#'   and keeps metrics that are larger on gyral crowns (such as
#'   [curvature()]), judged by the metric's correlation with the displayed
#'   mesh's mean curvature. The decision is recorded in the provenance.
#' @param anatomy_range Dark and light gray levels in [0, 1].
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
#' @param palette Character vector of at least two colours, ordered from low to
#'   high values. With a positive threshold the palette spans only the
#'   suprathreshold range: for a two-sided map the first half of the colours
#'   is the negative ramp and the second half the positive ramp. `NULL` uses a
#'   default suited to `tail`.
#' @param limits Numeric scalar-color limits.
#' @param overlay_alpha Maximum overlay opacity.
#' @param alpha_ramp Width of the opacity ramp above threshold.
#' @param antialias Integer supersampling factor per axis.
#' @param margin Fractional panel margin.
#' @param medial_wall Whether masked-domain triangles are neutrally shaded,
#'   omitted, or independently outlined.
#' @param outer_contour Draw an anti-aliased silhouette contour, about one
#'   pixel wide, where covered cortex touches background connected to the
#'   image exterior. Enclosed holes and depth discontinuities are not treated
#'   as outer contour.
#' @param outer_contour_color Contour colour; an alpha channel sets its
#'   opacity.
#' @param background Background color.
#' @param lighting `TRUE` for the default two-light Blinn-Phong shading,
#'   `FALSE` for flat (unlit) rendering, or a named list overriding any of
#'   `ambient`, `key`, `fill`, `specular`, `shininess`, `key_dir`, `fill_dir`
#'   (view-space directions: x right, y up, z toward the viewer),
#'   `overlay_shading` (share of the shading applied to overlay colour), and
#'   `sky` (hemispheric ambient fraction). Overlay shading is normalized so a
#'   surface facing the camera shows exactly the colour-bar colour.
#' @param subdivide `"auto"` subdivides coarse meshes for display until the
#'   median projected edge is at most four pixels (up to three levels);
#'   `FALSE` disables it; an integer sets the number of levels.
#' @param pixel_scale Output pixels per surface coordinate unit (usually
#'   mm). `NULL` fits the surface to the panel; a common value gives several
#'   panels one scale.
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
                                palette = NULL,
                                limits = NULL,
                                overlay_alpha = 1,
                                alpha_ramp = 0,
                                antialias = 3L,
                                margin = 0.04,
                                medial_wall = c("shade", "mask", "outline"),
                                outer_contour = TRUE,
                                outer_contour_color = "#6E6E6ED9",
                                background = "#FFFFFF",
                                lighting = TRUE,
                                subdivide = "auto",
                                pixel_scale = NULL,
                                return_buffers = FALSE,
                                anatomy_style = c("publication", "continuous", "binary"),
                                anatomy_midpoint = NULL,
                                anatomy_invert = "auto",
                                anatomy_range = c(0.70, 0.93)) {
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
  anatomy_style <- match.arg(anatomy_style)
  if (!is.numeric(anatomy_range) || length(anatomy_range) != 2L ||
      any(!is.finite(anatomy_range)) || any(anatomy_range < 0 | anatomy_range > 1) ||
      anatomy_range[1] >= anatomy_range[2]) {
    stop("'anatomy_range' must be two increasing gray levels in [0, 1].", call. = FALSE)
  }
  if (identical(anatomy_invert, "auto")) {
    anatomy_invert <- .ns_anatomy_is_sulcal_positive(anatomy_metric, geometry)
  } else if (!is.logical(anatomy_invert) || length(anatomy_invert) != 1L ||
             is.na(anatomy_invert)) {
    stop("'anatomy_invert' must be TRUE, FALSE, or \"auto\".", call. = FALSE)
  }
  anatomy <- if (anatomy_style == "publication") {
    a <- .ns_scale_anatomy_metric(anatomy_metric, n)
    if (anatomy_invert && !is.null(anatomy_metric)) 1 - a else a
  } else {
    if (is.null(anatomy_metric)) anatomy_metric <- rep(0, n)
    if (length(anatomy_metric) != n) stop("Anatomy length must match vertices.")
    normalize_surface_anatomy(anatomy_metric, anatomy_style,
                              anatomy_midpoint, anatomy_invert) + 0.5
  }
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

  out <- .ns_render_rgba_mesh(
    vertices, t(geometry@mesh$it), vertex_values, anatomy, cortex_mask,
    hemi = geometry@hemi, camera = camera, camera_mode = camera_mode,
    presentation_obliquity = presentation_obliquity, width = width,
    height = height, threshold = threshold, tail = tail, palette = palette,
    limits = limits, overlay_alpha = overlay_alpha, alpha_ramp = alpha_ramp,
    antialias = antialias, margin = margin, medial_wall = medial_wall,
    outer_contour = outer_contour, outer_contour_color = outer_contour_color,
    background = background, lighting = lighting, subdivide = subdivide,
    return_buffers = return_buffers, anatomy_range = anatomy_range,
    extent = if (!is.null(pixel_scale)) list(scale = pixel_scale)
  )
  out$provenance[c("anatomy_style", "anatomy_midpoint", "anatomy_invert")] <-
    list(anatomy_style, anatomy_midpoint, anatomy_invert)
  out
}

# Rasterize a prepared mesh (1-based faces, per-vertex values, anatomy in
# [0, 1], and cortex mask). `extent` optionally fixes the projected window
# and scale so several renders share one pixel scale.
.ns_render_rgba_mesh <- function(vertices, faces, vertex_values, anatomy,
                                 cortex_mask, hemi, camera, camera_mode,
                                 presentation_obliquity, width, height,
                                 threshold, tail, palette, limits,
                                 overlay_alpha, alpha_ramp, antialias, margin,
                                 medial_wall, outer_contour,
                                 outer_contour_color, background, lighting,
                                 subdivide, return_buffers, anatomy_range,
                                 extent = NULL) {
  obliquity <- if (camera_mode == "presentation") presentation_obliquity else 0
  levels <- .ns_subdivision_levels(subdivide, vertices, faces, camera,
                                   hemi, width, height, margin,
                                   obliquity, extent)
  if (levels > 0L) {
    display <- .ns_subdivide_display_mesh(vertices, faces, vertex_values,
                                          anatomy, cortex_mask, levels)
    vertices <- display$vertices
    faces <- display$faces
    vertex_values <- display$values
    anatomy <- display$anatomy
    cortex_mask <- display$cortex_mask
  }
  projected <- .ns_project_surface_camera(
    vertices, camera = camera, hemi = hemi,
    width = width, height = height, margin = margin,
    presentation_obliquity = obliquity, extent = extent
  )
  if (is.null(palette)) palette <- .ns_default_palette(tail)
  palette_rgba <- .ns_overlay_lut(palette, limits, threshold, tail, 256L)
  light <- .ns_resolve_lighting(lighting)
  view_normals <- if (is.null(light)) NULL else
    .ns_mesh_vertex_normals(vertices, faces) %*% attr(projected, "basis")
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
    base_low = anatomy_range[1],
    base_high = anatomy_range[2],
    medial_wall_policy = if (medial_wall == "mask") 1L else 0L,
    background = as.numeric(bg[, 1L]),
    supersample = antialias,
    return_buffers = isTRUE(return_buffers),
    normals = view_normals,
    lighting = light,
    contour = if (isTRUE(outer_contour)) {
      c(grDevices::col2rgb(outer_contour_color, alpha = TRUE)[, 1L] / 255, 0.9)
    }
  )
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
    hemi = hemi,
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
    medial_wall = medial_wall,
    anatomy_range = anatomy_range,
    lighting = !is.null(light),
    subdivision_levels = levels
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

# TRUE when larger anatomy values mark sulci (as FreeSurfer sulc and curv
# do), judged by correlation with the display mesh's mean curvature, which is
# larger on convex (gyral) vertices. Such metrics are inverted so sulci
# render dark. Weak or undefined correlations leave the metric as given.
.ns_anatomy_is_sulcal_positive <- function(metric, geometry) {
  if (is.null(metric)) return(FALSE)
  convexity <- tryCatch(Rvcg::vcgCurve(geometry@mesh)$meanvb,
                        error = function(e) NULL)
  if (is.null(convexity) || length(convexity) != length(metric)) return(FALSE)
  ok <- is.finite(metric) & is.finite(convexity)
  if (sum(ok) < 10L) return(FALSE)
  r <- suppressWarnings(stats::cor(metric[ok], convexity[ok],
                                   method = "spearman"))
  isTRUE(r < -0.1)
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
    # Smoothstep separates sulci from gyri while keeping gradations.
    out <- out * out * (3 - 2 * out)
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

# Default overlay palettes, ordered from low to high values. A two-sided
# palette has an even number of anchors: the first half is the negative ramp
# (strongest to weakest), the second the positive ramp (weakest to strongest).
.ns_default_palette <- function(tail) {
  switch(tail,
    two_sided = c("#123A7C", "#2F6DB5", "#6FA6DD",
                  "#EE8F66", "#CF3E2F", "#7A0A1F"),
    positive = c("#F7A541", "#E8622C", "#C22A2C", "#7A0A1F"),
    negative = c("#0B2A63", "#1F5AA8", "#3B82CC", "#6DB0E6")
  )
}

# Colour lookup table evaluated on an even grid over `limits`, as the
# rasterizer indexes it. With a positive threshold the palette spans only the
# suprathreshold range, so the weakest displayed value receives the palette's
# threshold-end colour rather than an interior (often pale) colour.
.ns_overlay_lut <- function(palette, limits, threshold, tail, n = 256L) {
  v <- seq(limits[1], limits[2], length.out = n)
  ramp <- function(cols, t) {
    if (!length(t)) return(matrix(0, 0L, 3L))
    rgb <- grDevices::colorRamp(cols, space = "Lab")(pmin(1, pmax(0, t)))
    rgb / 255
  }
  if (threshold <= 0 && tail == "two_sided") {
    t <- (v - limits[1]) / diff(limits)
    out <- ramp(palette, t)
  } else if (tail == "two_sided") {
    half <- length(palette) %/% 2L
    neg <- palette[seq_len(length(palette) - half)]
    pos <- palette[(length(palette) - half + 1L):length(palette)]
    if (length(neg) < 2L) neg <- rep(neg, 2L)
    if (length(pos) < 2L) pos <- rep(pos, 2L)
    out <- matrix(0.5, n, 3)
    hi <- max(limits[2], threshold)
    lo <- min(limits[1], -threshold)
    ip <- v >= 0
    out[ip, ] <- ramp(pos, (v[ip] - threshold) / max(hi - threshold, 1e-12))
    out[!ip, ] <- ramp(neg, (v[!ip] - lo) / max(-threshold - lo, 1e-12))
  } else if (tail == "positive") {
    lo <- max(threshold, limits[1])
    out <- ramp(palette, (v - lo) / max(limits[2] - lo, 1e-12))
  } else {
    hi <- min(-threshold, limits[2])
    out <- ramp(palette, (v - limits[1]) / max(hi - limits[1], 1e-12))
  }
  cbind(out, 1)
}

# Number of display subdivision levels. "auto" subdivides until the median
# projected edge is at most `target_px` output pixels (at most three levels),
# so coarse meshes render with smooth silhouettes and threshold contours.
.ns_subdivision_levels <- function(subdivide, vertices, faces, camera, hemi,
                                   width, height, margin, obliquity,
                                   extent = NULL, target_px = 4) {
  if (isFALSE(subdivide)) return(0L)
  if (is.numeric(subdivide)) {
    if (length(subdivide) != 1L || !is.finite(subdivide) || subdivide < 0 ||
        subdivide != round(subdivide)) {
      stop("'subdivide' must be \"auto\", FALSE, or a non-negative integer.",
           call. = FALSE)
    }
    return(as.integer(subdivide))
  }
  if (!identical(subdivide, "auto")) {
    stop("'subdivide' must be \"auto\", FALSE, or a non-negative integer.",
         call. = FALSE)
  }
  scale <- attr(.ns_project_surface_camera(
    vertices, camera, hemi, width, height, margin, obliquity, extent
  ), "scale")
  edge_mm <- stats::median(sqrt(rowSums(
    (vertices[faces[, 1], , drop = FALSE] -
       vertices[faces[, 2], , drop = FALSE])^2
  )))
  px <- edge_mm * scale
  if (!is.finite(px) || px <= target_px) return(0L)
  as.integer(min(3, ceiling(log2(px / target_px))))
}

# Interpolating butterfly subdivision of geometry and per-vertex fields for
# display. Original vertices keep their positions and values.
.ns_subdivide_display_mesh <- function(vertices, faces, values, anatomy,
                                       cortex_mask, levels) {
  attrs <- cbind(vertices, values, anatomy)
  for (i in seq_len(levels)) {
    res <- cpp_butterfly_subdivide(faces, attrs, clamp_cols = c(3L, 4L))
    p <- res$parents
    cortex_mask <- cortex_mask[p[, 1]] & cortex_mask[p[, 2]]
    faces <- res$faces
    attrs <- res$attrs
  }
  list(vertices = attrs[, 1:3, drop = FALSE], faces = faces,
       values = attrs[, 4], anatomy = attrs[, 5], cortex_mask = cortex_mask)
}

# Orthographic camera basis. Returns a 3 x 3 matrix whose columns are the
# screen-right, screen-up, and toward-viewer axes in world coordinates, so
# `vertices %*% basis` gives (x, y, depth) with larger depth nearer. Every
# view is a proper rotation (determinant +1): no view is a mirror image.
.ns_camera_basis <- function(camera, hemi, presentation_obliquity = 0) {
  left <- hemi %in% c("lh", "left", "L")
  toward <- switch(camera,
    lateral = if (left) c(-1, 0, 0) else c(1, 0, 0),
    medial = if (left) c(1, 0, 0) else c(-1, 0, 0),
    dorsal = c(0, 0, 1),
    ventral = c(0, 0, -1)
  )
  up <- switch(camera, lateral = , medial = c(0, 0, 1),
               dorsal = c(0, 1, 0), ventral = c(0, -1, 0))
  right <- c(up[2] * toward[3] - up[3] * toward[2],
             up[3] * toward[1] - up[1] * toward[3],
             up[1] * toward[2] - up[2] * toward[1])
  basis <- cbind(right, up, toward)
  if (presentation_obliquity != 0 && camera %in% c("lateral", "medial")) {
    # Rotate the object about the superior axis so the view turns slightly
    # toward the anterior pole.
    direction <- if (camera == "lateral") 1 else -1
    if (!left) direction <- -direction
    theta <- direction * presentation_obliquity * pi / 180
    rot <- rbind(c(cos(theta), sin(theta), 0),
                 c(-sin(theta), cos(theta), 0),
                 c(0, 0, 1))
    basis <- rot %*% basis
  }
  unname(basis)
}

.ns_project_surface_camera <- function(vertices, camera, hemi, width, height,
                                       margin = 0.04,
                                       presentation_obliquity = 0,
                                       extent = NULL) {
  basis <- .ns_camera_basis(camera, hemi, presentation_obliquity)
  view <- vertices %*% basis
  xr <- extent$x %||% range(view[, 1], finite = TRUE)
  yr <- extent$y %||% range(view[, 2], finite = TRUE)
  avail_w <- width * (1 - 2 * margin)
  avail_h <- height * (1 - 2 * margin)
  scale <- extent$scale %||%
    min(avail_w / max(diff(xr), .Machine$double.eps),
        avail_h / max(diff(yr), .Machine$double.eps))
  x <- (view[, 1] - mean(xr)) * scale + width / 2
  y <- height / 2 - (view[, 2] - mean(yr)) * scale
  out <- cbind(x, y, depth = view[, 3])
  attr(out, "basis") <- basis
  attr(out, "scale") <- scale
  out
}

# Area-weighted vertex normals of a triangle mesh (faces are 1-based rows).
.ns_mesh_vertex_normals <- function(vertices, faces) {
  a <- vertices[faces[, 1], , drop = FALSE]
  e1 <- vertices[faces[, 2], , drop = FALSE] - a
  e2 <- vertices[faces[, 3], , drop = FALSE] - a
  fn <- cbind(e1[, 2] * e2[, 3] - e1[, 3] * e2[, 2],
              e1[, 3] * e2[, 1] - e1[, 1] * e2[, 3],
              e1[, 1] * e2[, 2] - e1[, 2] * e2[, 1])
  acc <- rowsum(fn[rep(seq_len(nrow(faces)), 3L), , drop = FALSE], c(faces))
  n <- matrix(0, nrow(vertices), 3)
  n[as.integer(rownames(acc)), ] <- acc
  len <- sqrt(rowSums(n^2))
  len[len == 0] <- 1
  n / len
}

.ns_lighting_defaults <- list(
  ambient = 0.58, key = 0.40, fill = 0.12, specular = 0.05, shininess = 24,
  key_dir = c(-0.5, 0.6, 0.62), fill_dir = c(0.35, -0.6, 0.72),
  overlay_shading = 0.6, sky = 0.15
)

# Resolve the public `lighting` argument into the rasterizer's parameter
# vector, or NULL for unlit (flat) rendering.
.ns_resolve_lighting <- function(lighting) {
  if (isFALSE(lighting) || is.null(lighting)) return(NULL)
  params <- .ns_lighting_defaults
  if (is.list(lighting)) {
    unknown <- setdiff(names(lighting), names(params))
    if (length(unknown) || is.null(names(lighting))) {
      stop("'lighting' list may contain only: ",
           paste(names(params), collapse = ", "), call. = FALSE)
    }
    params[names(lighting)] <- lighting
  } else if (!isTRUE(lighting)) {
    stop("'lighting' must be TRUE, FALSE, or a named list.", call. = FALSE)
  }
  with(params, c(ambient, key, fill, specular, shininess, key_dir, fill_dir,
                 overlay_shading, sky))
}

#' Normalize an anatomical surface underlay
#'
#' Converts curvature or sulcal depth to a centered display metric in
#' [-0.5, 0.5], suitable for the surfview curvature layer. This changes only
#' anatomical shading, never statistical values or surface coordinates.
#' @param metric Finite numeric anatomy values, one per vertex.
#' @param style Continuous robust scaling or binary folding contrast.
#' @param midpoint Boundary between dark and light. NULL uses the median,
#'   including for the historical [0, 1] output of [curvature()]. Use zero
#'   for a native signed metric whose anatomical boundary is zero.
#' @param invert Reverse dark/light polarity. Select this from the source
#'   metric's sign convention; signs differ between curvature producers.
#' @return A numeric vector in [-0.5, 0.5]. Constant metrics remain neutral.
#' @export
normalize_surface_anatomy <- function(metric,
                                      style = c("continuous", "binary"),
                                      midpoint = NULL, invert = FALSE) {
  style <- match.arg(style)
  if (!is.numeric(metric) || !length(metric) || any(!is.finite(metric))) {
    stop("'metric' must be a non-empty finite numeric vector.", call. = FALSE)
  }
  if (!is.null(midpoint) && (!is.numeric(midpoint) || length(midpoint) != 1L ||
                           !is.finite(midpoint))) {
    stop("'midpoint' must be NULL or one finite number.", call. = FALSE)
  }
  if (!is.logical(invert) || length(invert) != 1L || is.na(invert)) {
    stop("'invert' must be TRUE or FALSE.", call. = FALSE)
  }
  if (diff(range(metric)) == 0) return(rep(0, length(metric)))
  centered <- metric - (midpoint %||% stats::median(metric))
  if (style == "binary") {
    out <- 0.5 * sign(centered)
  } else {
    scale <- as.numeric(stats::quantile(abs(centered), 0.98, type = 8))
    if (scale == 0) scale <- max(abs(centered))
    out <- pmax(-0.5, pmin(0.5, centered / (2 * scale)))
  }
  if (invert) -out else out
}

#' Saturated signed surface overlay colors
#' @param signed Include the negative blue/cyan ramp as well as red/yellow.
#' @return Hexadecimal color anchors, ordered from low to high values.
#' @export
surface_heat_colors <- function(signed = TRUE) {
  if (!is.logical(signed) || length(signed) != 1L || is.na(signed)) {
    stop("'signed' must be TRUE or FALSE.", call. = FALSE)
  }
  positive <- grDevices::colorRampPalette(c("#FF0000", "#FF0000", "#FFFF00"))(128)
  if (signed) c(grDevices::colorRampPalette(c("#00FFFF", "#0000FF", "#0000FF"))(128),
                positive) else positive
}

#' Derive a stable anatomical curvature underlay
#'
#' Averages computed curvature over mesh neighbors before display. This is an
#' anatomical texture operation: neither coordinates nor statistical overlays
#' are modified. Prefer a native curvature/sulcal metric when available.
#' @param geometry Matching folded (usually white) [SurfaceGeometry].
#' @param iterations Number of adjacency averaging steps, including the vertex
#'   itself. Zero returns [curvature()] unchanged. This is not an FWHM in mm.
#' @return Numeric anatomical metric with one value per vertex.
#' @export
anatomical_curvature <- function(geometry, iterations = 5L) {
  if (!is.numeric(iterations) || length(iterations) != 1L ||
      !is.finite(iterations) || iterations < 0 || iterations != as.integer(iterations)) {
    stop("'iterations' must be a non-negative integer.", call. = FALSE)
  }
  metric <- curvature(geometry)
  if (iterations == 0L) return(metric)
  f <- faces(geometry)
  edges <- unique(rbind(f[,c(1,2)], f[,c(2,3)], f[,c(3,1)],
                        f[,c(2,1)], f[,c(3,2)], f[,c(1,3)]))
  # Include the vertex itself, and retain isolated vertices unchanged.
  edges <- unique(rbind(edges, cbind(seq_along(metric), seq_along(metric))))
  from <- edges[,1]
  to <- edges[,2]
  counts <- tabulate(from, length(metric))
  for (i in seq_len(iterations)) {
    sums <- rowsum(metric[to], from, reorder=TRUE)
    metric <- as.numeric(sums) / counts
  }
  metric
}
