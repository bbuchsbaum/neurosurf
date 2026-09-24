#' Style settings for parcel surface rendering
#'
#' Collects the shading, line, and medial-wall settings used by
#' [render_surface_parcels()]. The defaults target publication figures: a
#' quiet sulcal-depth underlay, soft directional light, filled parcels
#' outlined in a darker shade of their own colour, faint boundaries between
#' unfilled parcels, and a flat light medial wall that reads as "no data".
#'
#' Line specifications are lists with `col` (a colour, or `"darken"` for the
#' filled-parcel outline), `alpha` in \[0, 1\], and `radius` (dilation in
#' supersampled pixels; 0 is a hairline). Set `alpha = 0` to disable a line
#' class.
#'
#' @param gyrus,sulcus Underlay grey levels in \[0, 1\] for gyral crowns and
#'   sulcal fundi.
#' @param ambient,diffuse Lambert lighting weights.
#' @param light Light direction in screen space (x right, y up, z toward the
#'   viewer).
#' @param medial Medial-wall grey level.
#' @param medial_views Cameras in which label 0 is drawn as medial wall. In
#'   other views (where only slivers of it show, e.g. at the temporal pole)
#'   it takes the anatomical underlay instead.
#' @param medial_light Fraction of the medial-wall tone held constant; the
#'   remainder follows the lighting, so 1 is completely flat.
#' @param fill_light How strongly lighting modulates parcel fills (0 = flat).
#' @param fill_anatomy How strongly sulcal depth shows through parcel fills.
#' @param fill_line Outline around filled parcels.
#' @param parcel_line Boundaries between unfilled parcels.
#' @param medial_line Boundary between cortex and the medial wall.
#' @param contour Outer silhouette.
#' @param rim_fade Supersampled-pixel distance from the silhouette within
#'   which unfilled-parcel boundaries are suppressed.
#' @param line_light_fade Fade unfilled-parcel boundaries out on surfaces
#'   turned away from the light, where they read as scratches.
#' @param ... Named overrides of the arguments above, convenient when
#'   passing a list.
#' @return A `surface_parcel_style` list.
#' @export
surface_parcel_style <- function(gyrus = 0.89,
                                 sulcus = 0.68,
                                 ambient = 0.68,
                                 diffuse = 0.32,
                                 light = c(-0.3, 0.45, 0.85),
                                 medial = 0.82,
                                 medial_light = 0.9,
                                 medial_views = "medial",
                                 fill_light = 0.4,
                                 fill_anatomy = 0.06,
                                 fill_line = list(col = "#1A1A1A", alpha = 0.3,
                                                  radius = 1L),
                                 parcel_line = list(col = "#FFFFFF",
                                                    alpha = 0.35, radius = 0L),
                                 medial_line = list(col = "#8C8C8C",
                                                    alpha = 0.6, radius = 0L),
                                 contour = list(col = "#8A8A8A", alpha = 0.75,
                                                radius = 1L),
                                 rim_fade = 3L,
                                 line_light_fade = TRUE,
                                 ...) {
  style <- list(
    gyrus = gyrus, sulcus = sulcus, ambient = ambient, diffuse = diffuse,
    light = light, medial = medial, medial_light = medial_light,
    medial_views = medial_views,
    fill_light = fill_light, fill_anatomy = fill_anatomy,
    fill_line = fill_line, parcel_line = parcel_line,
    medial_line = medial_line, contour = contour, rim_fade = rim_fade,
    line_light_fade = line_light_fade
  )
  extra <- list(...)
  unknown <- setdiff(names(extra), names(style))
  if (length(unknown)) {
    stop("Unknown surface_parcel_style() setting(s): ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  style[names(extra)] <- extra
  for (nm in c("gyrus", "sulcus", "ambient", "diffuse", "medial",
               "medial_light", "fill_light", "fill_anatomy")) {
    x <- style[[nm]]
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0 || x > 1) {
      stop("'", nm, "' must be a number in [0, 1].", call. = FALSE)
    }
  }
  if (!is.character(style$medial_views) ||
      !all(style$medial_views %in% c("lateral", "medial", "dorsal", "ventral"))) {
    stop("'medial_views' must name cameras: lateral, medial, dorsal, ventral.",
         call. = FALSE)
  }
  if (!is.logical(style$line_light_fade) || length(style$line_light_fade) != 1L ||
      is.na(style$line_light_fade)) {
    stop("'line_light_fade' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(style$light) || length(style$light) != 3L ||
      any(!is.finite(style$light)) || sum(style$light^2) == 0) {
    stop("'light' must be a finite non-zero 3-vector.", call. = FALSE)
  }
  for (nm in c("fill_line", "parcel_line", "medial_line", "contour")) {
    style[[nm]] <- .ns_parcel_line_spec(style[[nm]], nm)
  }
  structure(style, class = c("surface_parcel_style", "list"))
}

.ns_parcel_line_spec <- function(spec, name) {
  if (is.null(spec)) return(list(col = "#000000", alpha = 0, radius = 0L))
  if (!is.list(spec)) stop("'", name, "' must be a list.", call. = FALSE)
  spec$col <- spec$col %||% "#000000"
  spec$alpha <- spec$alpha %||% 1
  spec$radius <- as.integer(spec$radius %||% 0L)
  if (!is.numeric(spec$alpha) || length(spec$alpha) != 1L ||
      !is.finite(spec$alpha) || spec$alpha < 0 || spec$alpha > 1) {
    stop("'", name, "$alpha' must be in [0, 1].", call. = FALSE)
  }
  if (is.na(spec$radius) || spec$radius < 0L) {
    stop("'", name, "$radius' must be a non-negative integer.", call. = FALSE)
  }
  spec
}

#' Approximate sulcal depth from white and inflated surfaces
#'
#' Estimates a FreeSurfer-style sulcal depth map as the signed displacement
#' between a white (or pial) surface and its inflated counterpart, measured
#' along the inflated surface normals after matching the two meshes' centres
#' and RMS radii. Positive values mark gyral crowns and negative values sulcal
#' fundi. Both geometries must share topology.
#'
#' @param white Folded `SurfaceGeometry` (white or pial).
#' @param inflated Inflated `SurfaceGeometry` with the same vertices and faces.
#' @param iterations Laplacian smoothing iterations applied to the result.
#' @return A numeric vector with one value per vertex.
#' @export
surface_sulcal_proxy <- function(white, inflated, iterations = 4L) {
  if (!inherits(white, "SurfaceGeometry") ||
      !inherits(inflated, "SurfaceGeometry")) {
    stop("'white' and 'inflated' must be SurfaceGeometry objects.",
         call. = FALSE)
  }
  w <- t(white@mesh$vb[1:3, , drop = FALSE])
  v <- t(inflated@mesh$vb[1:3, , drop = FALSE])
  faces <- t(inflated@mesh$it)
  if (nrow(w) != nrow(v) || !identical(dim(white@mesh$it), dim(inflated@mesh$it))) {
    stop("'white' and 'inflated' must share mesh topology.", call. = FALSE)
  }
  normals <- .ns_outward_normals(v, faces)
  wc <- sweep(w, 2, colMeans(w))
  vc <- sweep(v, 2, colMeans(v))
  scale <- sqrt(mean(rowSums(wc^2)) / max(mean(rowSums(vc^2)), .Machine$double.eps))
  depth <- rowSums((wc - vc * scale) * normals)
  .ns_smooth_vertex_field(.ns_mesh_mean_operator(faces, nrow(v)), depth,
                          as.integer(iterations))
}

#' Prepare a labelled surface for parcel rendering
#'
#' Precomputes the per-geometry quantities used by [render_surface_parcels()]:
#' outward vertex normals, a normalized anatomical underlay, and smoothed
#' label memberships that let parcel boundaries follow smooth curves through
#' mesh triangles instead of stair-stepping along triangle edges. Reuse the
#' result across cameras and value maps.
#'
#' @param geometry Display `SurfaceGeometry` (typically inflated).
#' @param labels Integer parcel label per vertex; 0 marks the medial wall.
#' @param anatomy_metric Optional sulcal-depth or curvature value per vertex,
#'   larger on gyri. `NULL` gives a neutral underlay.
#' @param cortex_mask Optional logical per vertex; `FALSE` vertices are
#'   treated as medial wall. Defaults to `labels != 0`.
#' @param boundary_smooth Laplacian iterations applied to label memberships.
#'   0 gives piecewise-linear boundaries through triangle edge midpoints.
#' @return A `surface_parcel_prep` object.
#' @export
prepare_surface_parcels <- function(geometry, labels, anatomy_metric = NULL,
                                    cortex_mask = NULL, boundary_smooth = 3L) {
  if (!inherits(geometry, "SurfaceGeometry")) {
    stop("'geometry' must be a SurfaceGeometry object.", call. = FALSE)
  }
  vertices <- t(geometry@mesh$vb[1:3, , drop = FALSE])
  faces <- t(geometry@mesh$it)
  n <- nrow(vertices)
  labels <- as.integer(labels)
  if (length(labels) != n || anyNA(labels)) {
    stop("'labels' must hold one non-missing integer per vertex.", call. = FALSE)
  }
  if (!is.null(cortex_mask)) {
    if (!is.logical(cortex_mask) || length(cortex_mask) != n || anyNA(cortex_mask)) {
      stop("'cortex_mask' must be a non-missing logical value per vertex.",
           call. = FALSE)
    }
    labels[!cortex_mask] <- 0L
  }
  anatomy <- if (is.null(anatomy_metric)) {
    rep(0.5, n)
  } else {
    if (!is.numeric(anatomy_metric) || length(anatomy_metric) != n ||
        any(!is.finite(anatomy_metric))) {
      stop("'anatomy_metric' must hold one finite value per vertex.",
           call. = FALSE)
    }
    normalize_surface_anatomy(anatomy_metric, "continuous") + 0.5
  }
  boundary_smooth <- as.integer(boundary_smooth)
  if (length(boundary_smooth) != 1L || is.na(boundary_smooth) ||
      boundary_smooth < 0L) {
    stop("'boundary_smooth' must be a non-negative integer.", call. = FALSE)
  }

  # Smoothed one-hot memberships, sampled per face for each of the face's
  # three candidate labels at each of its three vertices.
  ulab <- sort(unique(labels))
  k <- match(labels, ulab)
  membership <- Matrix::sparseMatrix(i = seq_len(n), j = k, x = 1,
                                     dims = c(n, length(ulab)))
  if (boundary_smooth > 0L) {
    op <- .ns_mesh_mean_operator(faces, n)
    for (it in seq_len(boundary_smooth)) {
      membership <- 0.5 * membership + 0.5 * (op %*% membership)
    }
  }
  trip <- Matrix::summary(methods::as(membership, "CsparseMatrix"))
  key <- trip$i + n * (trip$j - 1)
  face_scores <- matrix(0, nrow(faces), 9L)
  for (cand in 1:3) {
    for (vtx in 1:3) {
      hit <- match(faces[, vtx] + n * (k[faces[, cand]] - 1), key)
      col <- (cand - 1L) * 3L + vtx
      face_scores[, col] <- ifelse(is.na(hit), 0, trip$x[hit])
    }
  }

  structure(list(
    geometry = geometry,
    hemi = geometry@hemi,
    vertices = vertices,
    faces = faces,
    labels = labels,
    normals = .ns_outward_normals(vertices, faces),
    anatomy = anatomy,
    face_scores = face_scores,
    boundary_smooth = boundary_smooth
  ), class = "surface_parcel_prep")
}

#' Render a parcel map on a surface
#'
#' Rasterizes a labelled surface with deferred per-pixel shading. Each output
#' pixel is supersampled; every sample resolves its parcel from smoothed label
#' memberships, so parcel edges are smooth and antialiased. Parcels with a
#' finite value (or an explicit colour) are filled and outlined; all others
#' show the shaded anatomical underlay with faint boundaries. The medial wall
#' is drawn flat and light.
#'
#' @param x A `surface_parcel_prep` from [prepare_surface_parcels()].
#' @param values Optional numeric parcel values named by label id. Unnamed
#'   values must follow the sorted non-zero labels. `NA` leaves a parcel
#'   unfilled.
#' @param colors Optional fill colours named by label id, used instead of
#'   `values` (e.g. categorical atlas colours).
#' @param camera View: `"lateral"`, `"medial"`, `"dorsal"`, or `"ventral"`.
#' @param camera_mode,presentation_obliquity Canonical or slightly oblique
#'   camera; see [render_surface_rgba()].
#' @param width,height Output raster size in pixels.
#' @param antialias Supersampling factor per axis.
#' @param palette Colours for `values`, low to high.
#' @param limits Two increasing numbers mapped to the palette ends. Defaults
#'   to the symmetric absolute range of `values`.
#' @param style A [surface_parcel_style()] list.
#' @param margin Fractional margin around the surface.
#' @return A list with `rgba` (a `height` x `width` x 4 raw array with
#'   straight alpha; transparent where the surface is absent), `labels` (the
#'   parcel label at each output pixel centre; `NA` off-surface), `camera`,
#'   and `provenance`.
#' @export
render_surface_parcels <- function(x,
                                   values = NULL,
                                   colors = NULL,
                                   camera = c("lateral", "medial", "dorsal",
                                              "ventral"),
                                   camera_mode = c("canonical", "presentation"),
                                   presentation_obliquity = 7,
                                   width = 900L,
                                   height = 620L,
                                   antialias = 3L,
                                   palette = c("#3B4CC0", "#F7F7F7", "#B40426"),
                                   limits = NULL,
                                   style = surface_parcel_style(),
                                   margin = 0.01) {
  if (!inherits(x, "surface_parcel_prep")) {
    stop("'x' must come from prepare_surface_parcels().", call. = FALSE)
  }
  camera <- match.arg(camera)
  camera_mode <- match.arg(camera_mode)
  if (!inherits(style, "surface_parcel_style")) {
    style <- do.call(surface_parcel_style, as.list(style))
  }
  antialias <- as.integer(antialias)
  if (length(antialias) != 1L || is.na(antialias) || antialias < 1L) {
    stop("'antialias' must be a positive integer.", call. = FALSE)
  }
  width <- as.integer(width)
  height <- as.integer(height)
  fill_rgb <- .ns_parcel_fill_rgb(x$labels, values, colors, palette, limits)

  ss <- antialias
  sw <- width * ss
  sh <- height * ss
  projected <- .ns_project_surface_camera(
    x$vertices, camera = camera, hemi = x$hemi, width = sw, height = sh,
    margin = margin,
    presentation_obliquity = if (camera_mode == "presentation") {
      presentation_obliquity
    } else {
      0
    }
  )
  gb <- cpp_rasterize_surface_gbuffer(projected, x$faces, sw, sh)
  cov <- gb$face > 0L
  fi <- gb$face[cov]
  w0 <- gb$w0[cov]
  w1 <- gb$w1[cov]
  w2 <- 1 - w0 - w1
  v0 <- x$faces[fi, 1]
  v1 <- x$faces[fi, 2]
  v2 <- x$faces[fi, 3]

  # Parcel per sample: the face candidate with the largest interpolated
  # smoothed membership.
  fs <- x$face_scores
  score <- function(cand) {
    base <- (cand - 1L) * 3L
    fs[cbind(fi, base + 1L)] * w0 + fs[cbind(fi, base + 2L)] * w1 +
      fs[cbind(fi, base + 3L)] * w2
  }
  s0 <- score(1L)
  s1 <- score(2L)
  s2 <- score(3L)
  l0 <- x$labels[v0]
  lab <- ifelse(s0 >= s1 & s0 >= s2, l0,
                ifelse(s1 >= s2, x$labels[v1], x$labels[v2]))

  # Lighting from interpolated normals in camera space.
  b <- attr(projected, "basis")
  basis <- list(right = b[, 1], up = b[, 2], toward = b[, 3])
  nrm <- x$normals[v0, , drop = FALSE] * w0 +
    x$normals[v1, , drop = FALSE] * w1 + x$normals[v2, , drop = FALSE] * w2
  nx <- as.numeric(nrm %*% basis$right)
  ny <- as.numeric(nrm %*% basis$up)
  nz <- as.numeric(nrm %*% basis$toward)
  nl <- pmax(sqrt(nx^2 + ny^2 + nz^2), .Machine$double.eps)
  light <- style$light / sqrt(sum(style$light^2))
  ndl <- pmax(0, (nx * light[1] + ny * light[2] + nz * light[3]) / nl)
  lit <- style$ambient + style$diffuse * ndl
  lit_rel <- lit / (style$ambient + style$diffuse)

  anat <- x$anatomy[v0] * w0 + x$anatomy[v1] * w1 + x$anatomy[v2] * w2
  grey <- (style$sulcus + anat * (style$gyrus - style$sulcus)) * lit
  if (!camera %in% style$medial_views) {
    # Outside medial views the wall only shows as slivers; shade them as
    # cortex and give them a label of their own so no wall outline is drawn.
    lab[lab == 0L] <- -3L
  }
  medial <- lab == 0L
  grey[medial] <- style$medial *
    (style$medial_light + (1 - style$medial_light) * lit_rel[medial])
  r <- g <- b <- grey

  fill_idx <- match(lab, fill_rgb$labels)
  filled <- !medial & !is.na(fill_idx)
  if (any(filled)) {
    mod <- ((1 - style$fill_light) + style$fill_light * lit_rel[filled]) *
      ((1 - style$fill_anatomy) + style$fill_anatomy * 1.3 *
         (0.5 + 0.5 * anat[filled]))
    rgb <- fill_rgb$rgb[fill_idx[filled], , drop = FALSE]
    r[filled] <- rgb[, 1] * mod
    g[filled] <- rgb[, 2] * mod
    b[filled] <- rgb[, 3] * mod
  }

  planes <- lapply(list(r, g, b), function(v) {
    m <- matrix(0, sh, sw)
    m[cov] <- pmin(1, pmax(0, v))
    m
  })
  alpha <- matrix(0, sh, sw)
  alpha[cov] <- 1

  # Edge classes at supersampled resolution.
  li <- matrix(-1L, sh, sw)
  li[cov] <- lab
  fi_mask <- matrix(FALSE, sh, sw)
  fi_mask[cov] <- filled
  right <- .ns_shift(li, 0L, 1L, -1L)
  down <- .ns_shift(li, 1L, 0L, -1L)
  edge_any <- li != right | li != down
  touches <- function(value) li == value | right == value | down == value
  off <- touches(-1L)
  wall <- touches(0L)
  fill_touch <- fi_mask | .ns_shift(fi_mask, 0L, 1L, FALSE) |
    .ns_shift(fi_mask, 1L, 0L, FALSE)
  contour_e <- edge_any & off
  fill_e <- edge_any & !off & fill_touch
  wall_e <- edge_any & !off & !fill_touch & wall
  parcel_e <- edge_any & !off & !fill_touch & !wall
  if (style$rim_fade > 0L) {
    parcel_e <- parcel_e & !.ns_dilate(contour_e, as.integer(style$rim_fade))
  }

  light_fade <- NULL
  if (isTRUE(style$line_light_fade)) {
    light_fade <- matrix(1, sh, sw)
    light_fade[cov] <- pmin(1, pmax(0, (ndl - 0.15) / 0.35))
  }
  paint <- function(mask, spec, inside_only = TRUE, fade = NULL) {
    if (spec$alpha <= 0 || !any(mask)) return(invisible())
    m <- .ns_dilate(mask, spec$radius)
    if (inside_only) m <- m & alpha > 0
    col <- grDevices::col2rgb(spec$col)[, 1] / 255
    a <- if (is.null(fade)) spec$alpha else spec$alpha * fade[m]
    for (ch in 1:3) {
      p <- planes[[ch]]
      p[m] <- p[m] * (1 - a) + col[ch] * a
      planes[[ch]] <<- p
    }
    if (!inside_only) {
      newly <- m & alpha == 0
      for (ch in 1:3) {
        p <- planes[[ch]]
        p[newly] <- col[ch]
        planes[[ch]] <<- p
      }
      alpha[newly] <<- a
    }
    invisible()
  }
  paint(parcel_e, style$parcel_line, fade = light_fade)
  paint(wall_e, style$medial_line)
  if (identical(style$fill_line$col, "darken")) {
    if (style$fill_line$alpha > 0 && any(fill_e)) {
      m <- .ns_dilate(fill_e, style$fill_line$radius) & fi_mask
      for (ch in 1:3) {
        p <- planes[[ch]]
        p[m] <- p[m] * (1 - style$fill_line$alpha)
        planes[[ch]] <- p
      }
    }
  } else {
    paint(fill_e, style$fill_line)
  }
  paint(contour_e, style$contour, inside_only = FALSE)

  # Box-filter downsample with premultiplied alpha, returned as straight alpha.
  a_out <- .ns_box_down(alpha, ss)
  rgba <- array(as.raw(0L), c(height, width, 4L))
  denom <- pmax(a_out, 1e-12)
  for (ch in 1:3) {
    prem <- .ns_box_down(planes[[ch]] * alpha, ss) / denom
    rgba[, , ch] <- as.raw(round(255 * pmin(1, pmax(0, prem))))
  }
  rgba[, , 4] <- as.raw(round(255 * pmin(1, a_out)))
  centre <- as.integer(ceiling(ss / 2))
  label_out <- li[seq(centre, by = ss, length.out = height),
                  seq(centre, by = ss, length.out = width)]
  label_out[label_out == -3L] <- 0L
  label_out[label_out < 0L] <- NA_integer_

  list(
    rgba = rgba,
    labels = label_out,
    camera = list(
      view = camera,
      projection = paste0(camera_mode, "_orthographic"),
      hemi = x$hemi,
      margin = margin
    ),
    provenance = list(
      backend = "cpu_deferred_parcels",
      antialias = antialias,
      boundary_smooth = x$boundary_smooth,
      limits = fill_rgb$limits,
      n_filled = length(fill_rgb$labels)
    )
  )
}

.ns_parcel_fill_rgb <- function(labels, values, colors, palette, limits) {
  ids <- sort(unique(labels[labels != 0L]))
  empty <- list(labels = integer(), rgb = matrix(0, 0, 3), limits = limits)
  if (!is.null(colors)) {
    if (!is.null(values)) {
      stop("Supply 'values' or 'colors', not both.", call. = FALSE)
    }
    keys <- names(colors)
    if (is.null(keys)) {
      if (length(colors) != length(ids)) {
        stop("Unnamed 'colors' must have one entry per non-zero label.",
             call. = FALSE)
      }
      keys <- as.character(ids)
    }
    keep <- !is.na(colors)
    if (!any(keep)) return(empty)
    return(list(
      labels = as.integer(keys[keep]),
      rgb = t(grDevices::col2rgb(colors[keep]) / 255),
      limits = limits
    ))
  }
  if (is.null(values)) return(empty)
  if (!is.numeric(values)) stop("'values' must be numeric.", call. = FALSE)
  keys <- names(values)
  if (is.null(keys)) {
    if (length(values) != length(ids)) {
      stop("Unnamed 'values' must have one entry per non-zero label.",
           call. = FALSE)
    }
    keys <- as.character(ids)
  }
  keep <- is.finite(values)
  if (is.null(limits)) {
    m <- if (any(keep)) max(abs(values[keep])) else 1
    if (m == 0) m <- 1
    limits <- c(-m, m)
  }
  if (!is.numeric(limits) || length(limits) != 2L || any(!is.finite(limits)) ||
      limits[1] >= limits[2]) {
    stop("'limits' must be two finite increasing numbers.", call. = FALSE)
  }
  if (!any(keep)) {
    empty$limits <- limits
    return(empty)
  }
  ramp <- grDevices::colorRamp(palette, space = "Lab")
  t01 <- pmin(1, pmax(0, (values[keep] - limits[1]) / diff(limits)))
  list(labels = as.integer(keys[keep]), rgb = ramp(t01) / 255, limits = limits)
}

.ns_outward_normals <- function(vertices, faces) {
  a <- vertices[faces[, 1], , drop = FALSE]
  u <- vertices[faces[, 2], , drop = FALSE] - a
  w <- vertices[faces[, 3], , drop = FALSE] - a
  fn <- cbind(u[, 2] * w[, 3] - u[, 3] * w[, 2],
              u[, 3] * w[, 1] - u[, 1] * w[, 3],
              u[, 1] * w[, 2] - u[, 2] * w[, 1])
  n <- nrow(vertices)
  normals <- matrix(0, n, 3)
  for (corner in 1:3) {
    s <- rowsum(fn, faces[, corner])
    idx <- as.integer(rownames(s))
    normals[idx, ] <- normals[idx, ] + s
  }
  normals <- normals / pmax(sqrt(rowSums(normals^2)), .Machine$double.eps)
  centred <- sweep(vertices, 2, colMeans(vertices))
  if (mean(rowSums(normals * centred)) < 0) normals <- -normals
  normals
}

.ns_mesh_mean_operator <- function(faces, n) {
  i <- c(faces[, 1], faces[, 2], faces[, 3], faces[, 2], faces[, 3], faces[, 1])
  j <- c(faces[, 2], faces[, 3], faces[, 1], faces[, 1], faces[, 2], faces[, 3])
  adj <- Matrix::sparseMatrix(i = i, j = j, x = 1, dims = c(n, n))
  adj@x[] <- 1
  deg <- Matrix::rowSums(adj)
  deg[deg == 0] <- 1
  Matrix::Diagonal(x = 1 / deg) %*% adj
}

.ns_smooth_vertex_field <- function(op, x, iterations, lambda = 0.5) {
  for (it in seq_len(iterations)) {
    x <- (1 - lambda) * x + lambda * as.numeric(op %*% x)
  }
  x
}

.ns_shift <- function(m, dy, dx, fill) {
  h <- nrow(m)
  w <- ncol(m)
  out <- matrix(fill, h, w)
  ys <- max(1L, 1L + dy):min(h, h + dy)
  xs <- max(1L, 1L + dx):min(w, w + dx)
  if (length(ys) && length(xs) && min(ys) <= max(ys) && min(xs) <= max(xs)) {
    out[ys, xs] <- m[ys - dy, xs - dx]
  }
  out
}

.ns_dilate <- function(mask, radius) {
  if (radius <= 0L) return(mask)
  out <- mask
  for (dy in -radius:radius) {
    for (dx in -radius:radius) {
      if (dy * dy + dx * dx <= radius * radius + 0.5) {
        out <- out | .ns_shift(mask, dy, dx, FALSE)
      }
    }
  }
  out
}

.ns_box_down <- function(m, ss) {
  if (ss == 1L) return(m)
  h <- nrow(m) %/% ss
  w <- ncol(m) %/% ss
  out <- matrix(0, h, w)
  for (a in seq_len(ss)) {
    for (b in seq_len(ss)) {
      out <- out + m[seq(a, by = ss, length.out = h),
                     seq(b, by = ss, length.out = w)]
    }
  }
  out / (ss * ss)
}
