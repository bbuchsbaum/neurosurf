#' @noRd
#' @keywords internal
hk <- function(x, sigma=1) {
  exp((-x^2)/(2*sigma^2))
}

#' @noRd
#' @keywords internal
get_mode <- function(v) {

  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Apply affine transform to vertex coordinates
#'
#' @param coords Nx3 matrix of coordinates
#' @param xform 4x4 affine transformation matrix
#' @return Nx3 matrix of transformed coordinates
#' @noRd
#' @keywords internal
.ns_apply_affine <- function(coords, xform) {
  if (identical(xform, diag(4))) {
    return(coords)
  }
  # coords is Nx3, xform is 4x4

  # world = xform %*% [x, y, z, 1]^T
  n <- nrow(coords)
  hom <- cbind(coords, rep(1, n))  # Nx4 homogeneous coords
  transformed <- hom %*% t(xform)  # Nx4

  transformed[, 1:3, drop = FALSE]
}

#' @noRd
#' @keywords internal
.ns_vertex_normals_coords <- function(coords, faces) {
  # faces expected 1-based (as stored in SurfaceGeometry@mesh$it)
  if (is.null(dim(faces))) {
    faces <- t(faces)
  }
  if (ncol(faces) != 3) {
    stop("faces must have 3 columns (triangles).")
  }

  f1 <- faces[, 1]
  f2 <- faces[, 2]
  f3 <- faces[, 3]

  v1 <- coords[f1, , drop = FALSE]
  v2 <- coords[f2, , drop = FALSE]
  v3 <- coords[f3, , drop = FALSE]

  e1 <- v2 - v1
  e2 <- v3 - v1

  fn <- cbind(
    e1[, 2] * e2[, 3] - e1[, 3] * e2[, 2],
    e1[, 3] * e2[, 1] - e1[, 1] * e2[, 3],
    e1[, 1] * e2[, 2] - e1[, 2] * e2[, 1]
  )

  vn <- matrix(0, nrow(coords), 3)
  for (i in seq_len(nrow(faces))) {
    idx <- faces[i, ]
    vn[idx, ] <- vn[idx, ] + matrix(rep(fn[i, ], times = 3),
                                    ncol = 3, byrow = TRUE)
  }

  lens <- sqrt(rowSums(vn^2))
  keep <- lens > 0
  vn[keep, ] <- vn[keep, ] / lens[keep]
  vn
}

#' @noRd
#' @keywords internal
.ns_make_samples <- function(va, vb, faces,
                             sampling = c("midpoint", "normal_line", "thickness"),
                             n_samples = NULL,
                             depth = NULL,
                             radius = 3) {
  sampling <- match.arg(sampling)
  n_vertices <- nrow(va)

  if (sampling == "midpoint") {
    pts <- (va + vb) / 2
    return(array(pts, dim = c(n_vertices, 1, 3)))
  }

  if (sampling == "thickness") {
    if (!is.null(depth)) {
      frac <- depth
      n_samples <- length(depth)
    } else {
      n_samples <- if (is.null(n_samples)) 5L else n_samples
      frac <- seq(0, 1, length.out = n_samples)
    }
    pts <- array(NA_real_, dim = c(n_vertices, n_samples, 3))
    delta <- vb - va
    for (i in seq_len(n_samples)) {
      pts[, i, ] <- va + frac[i] * delta
    }
    return(pts)
  }

  # normal_line
  base <- (va + vb) / 2
  n_samples <- if (is.null(n_samples)) 7L else n_samples
  offsets <- if (!is.null(depth)) {
    depth * radius
  } else {
    seq(-radius, radius, length.out = n_samples)
  }

  normals <- .ns_vertex_normals_coords(base, faces)
  pts <- array(NA_real_, dim = c(n_vertices, n_samples, 3))
  for (i in seq_len(n_samples)) {
    pts[, i, ] <- base + normals * offsets[i]
  }
  pts
}

#' Sample a volume at world coordinates with explicit voxel interpolation
#'
#' neuroim2's continuous grid coordinates use the same 1-based convention as
#' array indices and \code{index_to_grid()}. Voxel centres therefore already
#' land on integer array indices before nearest or trilinear interpolation.
#'
#' @noRd
.ns_sample_volume <- function(vol, coords, interpolation = c("nearest", "linear"),
                              mask = NULL, fill = 0, na_rm = FALSE) {
  interpolation <- match.arg(interpolation)
  grid <- neuroim2::coord_to_grid(vol, coords)
  dims <- dim(vol)[seq_len(3L)]
  vol_arr <- as.array(vol)
  mask_arr <- if (is.null(mask)) {
    array(TRUE, dim = dims)
  } else {
    mask_vec <- as.logical(mask[])
    if (length(mask_vec) != prod(dims)) {
      stop("mask must be the same size as vol", call. = FALSE)
    }
    array(mask_vec, dim = dims)
  }

  n <- nrow(grid)
  value <- rep(fill, n)
  valid <- rep(FALSE, n)

  if (identical(interpolation, "nearest")) {
    idx <- round(grid)
    inside <- apply(idx >= 1 & sweep(idx, 2L, dims, `<=`), 1L, all)
    if (any(inside)) {
      ii <- idx[inside, , drop = FALSE]
      vv <- vol_arr[ii]
      ok <- mask_arr[ii] & is.finite(vv)
      pos <- which(inside)[ok]
      value[pos] <- vv[ok]
      valid[pos] <- TRUE
    }
    return(list(value = value, valid = valid))
  }

  lower <- floor(grid)
  frac <- grid - lower
  numerator <- numeric(n)
  weight_sum <- numeric(n)
  invalid_weight <- logical(n)

  for (dx in 0:1) for (dy in 0:1) for (dz in 0:1) {
    corner <- lower + matrix(c(dx, dy, dz), nrow = n, ncol = 3L,
                             byrow = TRUE)
    weight <- (if (dx == 0L) 1 - frac[, 1L] else frac[, 1L]) *
      (if (dy == 0L) 1 - frac[, 2L] else frac[, 2L]) *
      (if (dz == 0L) 1 - frac[, 3L] else frac[, 3L])
    contributes <- weight > sqrt(.Machine$double.eps)
    inside <- apply(corner >= 1 & sweep(corner, 2L, dims, `<=`), 1L, all)
    usable <- contributes & inside
    corner_valid <- rep(FALSE, n)

    if (any(usable)) {
      cc <- corner[usable, , drop = FALSE]
      vv <- vol_arr[cc]
      ok <- mask_arr[cc] & is.finite(vv)
      pos <- which(usable)[ok]
      numerator[pos] <- numerator[pos] + weight[pos] * vv[ok]
      weight_sum[pos] <- weight_sum[pos] + weight[pos]
      corner_valid[pos] <- TRUE
    }
    invalid_weight <- invalid_weight | (contributes & !corner_valid)
  }

  if (isTRUE(na_rm)) {
    valid <- weight_sum > 0
  } else {
    valid <- weight_sum > 0 & !invalid_weight
  }
  value[valid] <- numerator[valid] / weight_sum[valid]
  list(value = value, valid = valid)
}

#' @noRd
.ns_aggregate_samples <- function(values, valid, aggregate, fill) {
  n_vertices <- nrow(values)
  centre <- (ncol(values) + 1) / 2
  vapply(seq_len(n_vertices), function(i) {
    keep <- which(valid[i, ])
    if (!length(keep)) return(fill)
    x <- values[i, keep]
    switch(
      aggregate,
      mean = mean(x),
      mode = get_mode(x),
      closest = x[which.min(abs(keep - centre))],
      stop("Unknown depth aggregation.", call. = FALSE)
    )
  }, numeric(1))
}

#' @noRd
.ns_surface_smooth_mm <- function(values, coords, faces, fwhm) {
  if (fwhm <= 0) return(values)
  sigma <- fwhm / sqrt(8 * log(2))
  edges <- rbind(faces[, c(1L, 2L), drop = FALSE],
                 faces[, c(2L, 3L), drop = FALSE],
                 faces[, c(3L, 1L), drop = FALSE])
  edges <- t(apply(edges, 1L, sort))
  edges <- unique(edges)
  distance <- sqrt(rowSums((coords[edges[, 1L], , drop = FALSE] -
                            coords[edges[, 2L], , drop = FALSE])^2))
  weight <- exp(-(distance^2) / (2 * sigma^2))
  numerator <- ifelse(is.finite(values), values, 0)
  denominator <- as.numeric(is.finite(values))
  out_num <- numerator
  out_den <- denominator
  for (i in seq_len(nrow(edges))) {
    a <- edges[i, 1L]
    b <- edges[i, 2L]
    w <- weight[[i]]
    if (is.finite(values[[b]])) {
      out_num[[a]] <- out_num[[a]] + w * values[[b]]
      out_den[[a]] <- out_den[[a]] + w
    }
    if (is.finite(values[[a]])) {
      out_num[[b]] <- out_num[[b]] + w * values[[a]]
      out_den[[b]] <- out_den[[b]] + w
    }
  }
  out <- rep(NA_real_, length(values))
  keep <- out_den > 0
  out[keep] <- out_num[keep] / out_den[keep]
  out
}

#' Map values from a 3D volume to a surface in the same coordinate space
#'
#' This function maps values from a 3D volume to a surface representation,
#' allowing for different mapping strategies.
#'
#' @param surf_wm The white matter (inner) surface, typically of class \code{SurfaceGeometry}.
#' @param surf_pial The pial (outer) surface, typically of class \code{SurfaceGeometry}.
#' @param vol An image volume of type \code{NeuroVol} that is to be mapped to the surface.
#' @param mask A mask defining valid voxels. In the legacy KNN contract, NULL
#'   retains historical behavior and treats only finite non-zero voxels as
#'   candidates. Explicit nearest/linear interpolation samples the full finite
#'   voxel grid, including zeros; an explicit mask restricts that grid.
#' @param fun The mapping function to use. Options are:
#'   \itemize{
#'     \item "avg": Average of nearby voxels (default)
#'     \item "nn": Nearest neighbor
#'     \item "mode": Most frequent value among nearby voxels
#'   }
#' @param knn The number of nearest neighbors in the legacy KNN contract.
#' @param sigma Legacy Gaussian-KNN bandwidth.
#' @param dthresh Legacy KNN distance cutoff. Explicit nearest and linear
#'   interpolation instead use grid bounds and mask validity.
#' @param fill Value used when no valid sample is available. For strict linear
#'   interpolation this includes an out-of-volume, NA, or masked corner; with
#'   \code{na_rm = TRUE}, remaining corner weights are renormalized instead.
#' @param sampling How to place sample points relative to the white/pial pair.
#'   Options are:
#'   \itemize{
#'     \item "midpoint" (default): original behaviour, samples at the midpoint between white and pial.
#'     \item "thickness": samples along the white→pial line at fractions given by \code{depth} or evenly spaced.
#'     \item "normal_line": samples along the vertex normal centred on the midpoint, spanning \code{radius} in both directions (or using \code{depth} offsets).
#'   }
#' @param n_samples Number of samples per vertex for \code{sampling != "midpoint"} when \code{depth} is not supplied.
#' @param depth Optional numeric vector controlling sampling positions; interpreted as fractions of thickness (for "thickness") or multiples of \code{radius} (for "normal_line").
#' @param radius Radius (in voxel units) for normal-line sampling when
#'   \code{sampling = "normal_line"}; also used as the distance scale when
#'   interpreting \code{depth} offsets for that mode.
#' @param knn Number of cached nearest voxel candidates per surface sample.
#' @param dthresh Maximum cached candidate distance in volume world-coordinate
#'   units.
#' @param sampler Optional surface sampler object created by \code{surface_sampler()}.
#'   When provided, the sampler is reused and other sampling-related arguments are ignored.
#' @param interpolation Voxel interpolation contract. \code{"legacy"}
#'   (default) preserves the historical midpoint Gaussian-KNN path and the
#'   historical nearest-sample path for multi-depth sampling. \code{"nearest"}
#'   samples the full voxel grid, including zero-valued voxels. \code{"linear"}
#'   performs trilinear interpolation of the scalar field at each sample point.
#' @param aggregate Explicit aggregation across cortical-depth samples for
#'   non-legacy interpolation: \code{"mean"}, categorical \code{"mode"}, or
#'   \code{"closest"} (the valid sample nearest the ribbon midpoint). If NULL,
#'   it is inferred from \code{fun} for compatibility.
#' @param na_rm For trilinear interpolation, whether a sample may renormalize
#'   interpolation weights after NA, masked, or out-of-volume corners are
#'   removed. The default FALSE returns \code{fill} for that sample, preventing
#'   interpolation across a missing-data or mask boundary.
#' @param surface_smooth_fwhm Tangential surface smoothing in mm. Zero
#'   (default) disables smoothing. Positive values apply a topology-local
#'   Gaussian edge-weighted pass whose spatial weights use surface-coordinate
#'   millimetres; this is deliberately separate from voxel interpolation and
#'   cortical-depth aggregation.
#'
#' @return A \code{NeuroSurface} object containing the mapped values.
#'
#' @examples
#' \donttest{
#' # Load standard white and pial surfaces from the package
#' wm_surf_file <- system.file("extdata", "std.8_lh.white.asc", package = "neurosurf")
#' pial_surf_file <- system.file("extdata", "std.8_lh.pial.asc", package = "neurosurf")
#'
#' surf_wm <- read_surf_geometry(wm_surf_file)
#' surf_pial <- read_surf_geometry(pial_surf_file)
#'
#' # Create a dummy volume for demonstration purposes
#' bb <- matrix(c(-80, 80, -120, 80, -60, 90), 3, 2, byrow = TRUE)
#' spacing <- c(1, 1, 1)
#' dims <- ceiling(abs(bb[,2] - bb[,1]) / spacing)
#' origin <- bb[,1]
#' sp <- neuroim2::NeuroSpace(dims, spacing, origin)
#' vol <- neuroim2::NeuroVol(rnorm(prod(dims)), sp)
#'
#' # Map volume to surface using average mapping
#' mapped_surf <- vol_to_surf(surf_wm, surf_pial, vol, fun = "avg")
#' print(mapped_surf)
#' }
#'
#' @export
#' @importFrom FNN get.knnx
#' @importFrom neuroim2 index_to_coord NeuroSpace NeuroVol
vol_to_surf <- function(surf_wm, surf_pial, vol, mask = NULL, 
                        fun = c("avg", "nn", "mode"), knn = 6, sigma = 8,
                        dthresh = sigma * 2, fill = 0,
                        sampling = c("midpoint", "normal_line", "thickness"),
                        n_samples = NULL,
                        depth = NULL,
                        radius = 3,
                        sampler = NULL,
                        interpolation = c("legacy", "nearest", "linear"),
                        aggregate = NULL,
                        na_rm = FALSE,
                        surface_smooth_fwhm = 0) {
  fun <- match.arg(fun)
  sampling <- match.arg(sampling)
  interpolation <- match.arg(interpolation)

  if (!is.numeric(surface_smooth_fwhm) ||
      length(surface_smooth_fwhm) != 1L ||
      !is.finite(surface_smooth_fwhm) || surface_smooth_fwhm < 0) {
    stop("'surface_smooth_fwhm' must be a non-negative numeric scalar.",
         call. = FALSE)
  }
  if (!is.logical(na_rm) || length(na_rm) != 1L || is.na(na_rm)) {
    stop("'na_rm' must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.null(aggregate)) {
    aggregate <- switch(fun, avg = "mean", nn = "closest", mode = "mode")
  }
  aggregate <- match.arg(aggregate, c("mean", "mode", "closest"))
  if (identical(interpolation, "linear") && identical(aggregate, "mode")) {
    stop("aggregate = 'mode' is invalid with linear interpolation.",
         call. = FALSE)
  }

  if (!is.null(sampler)) {
    if (!identical(interpolation, "legacy")) {
      stop("A reusable sampler currently supports interpolation = 'legacy' only.",
           call. = FALSE)
    }
    return(.ns_apply_surface_sampler(sampler, vol, fun = fun,
                                     sigma = sigma, fill = fill))
  }

  # Get vertices in surface coordinates

  va_surf <- vertices(surf_wm)
  vb_surf <- vertices(surf_pial)

  # Apply surf_to_world transform to convert to world (volume) coordinates

  xform <- surf_to_world(surf_wm)
  va <- .ns_apply_affine(va_surf, xform)
  vb <- .ns_apply_affine(vb_surf, xform)
  vavg <- (va + vb) / 2

  # The legacy KNN contract selects candidate voxels up front and historically
  # treats zero-valued voxels as absent when no mask is supplied. Explicit
  # nearest/linear interpolation instead samples the full grid; zeros remain
  # legitimate scalar values and mask/NA validity is handled per sample.
  if (!identical(interpolation, "legacy")) {
    if (identical(sampling, "thickness") && is.null(depth) &&
        is.null(n_samples)) {
      depth <- seq(0.1, 0.9, length.out = 5L)
    }
    faces <- t(surf_wm@mesh$it)
    samples <- .ns_make_samples(
      va, vb, faces,
      sampling = sampling,
      n_samples = n_samples,
      depth = depth,
      radius = radius
    )
    n_vertices <- dim(samples)[1L]
    n_samp <- dim(samples)[2L]
    sample_mat <- do.call(
      rbind,
      lapply(seq_len(n_samp), function(i) samples[, i, , drop = FALSE][, 1, ])
    )
    sampled <- .ns_sample_volume(
      vol = vol,
      coords = sample_mat,
      interpolation = interpolation,
      mask = mask,
      fill = fill,
      na_rm = na_rm
    )
    vals_mat <- matrix(sampled$value, nrow = n_vertices, ncol = n_samp)
    valid_mat <- matrix(sampled$valid, nrow = n_vertices, ncol = n_samp)
    mapped_vals <- .ns_aggregate_samples(
      vals_mat, valid_mat, aggregate = aggregate, fill = fill
    )
    mapped_vals <- .ns_surface_smooth_mm(
      mapped_vals, va, faces, surface_smooth_fwhm
    )
    return(NeuroSurface(surf_wm, seq_along(mapped_vals), mapped_vals))
  }

  # Build index set from mask if supplied; otherwise use non-zero voxels.
  if (!is.null(mask)) {
    mask_vec <- as.logical(mask[])
    if (length(mask_vec) != length(vol)) {
      stop("mask must be the same size as vol")
    }
    indices <- which(mask_vec & !is.na(vol[]))
  } else {
    indices <- which(!is.na(vol[]) & vol[] != 0)
  }

  if (length(indices) == 0) {
    stop("No voxels available for mapping (check mask or volume contents).")
  }

  volgrid <- neuroim2::index_to_coord(vol, indices)

  # Legacy path: original midpoint + KNN behaviour for exact backward compatibility
  legacy_midpoint <- sampling == "midpoint" &&
    (is.null(n_samples) || n_samples == 1) &&
    is.null(depth)

  if (legacy_midpoint) {
    k_eff <- min(knn, nrow(volgrid))
    if (k_eff < 1) {
      stop("Not enough voxels to perform nearest-neighbor search.")
    }
    if (k_eff < knn) {
      warning("Fewer available voxels than 'knn'; using k = ", k_eff)
    }

    nnres <- FNN::get.knnx(volgrid, vavg, k = k_eff)

    mapped_vals <- vapply(seq_len(nrow(nnres$nn.index)), function(i) {
      d <- nnres$nn.dist[i, ]
      keep <- which(d < dthresh)
      if (length(keep) == 0) {
        return(fill)
      }
      idx <- nnres$nn.index[i, keep]

      if (fun == "avg") {
        wts <- hk(d[keep], sigma = sigma)
        wts <- wts / sum(wts)
        sum(wts * vol[indices[idx]])
      } else if (fun == "nn") {
        vol[indices[idx[1]]]
      } else if (fun == "mode") {
        get_mode(vol[indices[idx]])
      } else {
        stop("Unknown mapping function.")
      }
    }, numeric(1))
  } else {
    faces <- t(surf_wm@mesh$it)
    samples <- .ns_make_samples(
      va, vb, faces,
      sampling = sampling,
      n_samples = n_samples,
      depth = depth,
      radius = radius
    )

    n_vertices <- dim(samples)[1]
    n_samp <- dim(samples)[2]

    sample_mat <- matrix(NA_real_, nrow = n_vertices * n_samp, ncol = 3)
    row_id <- 1L
    for (s in seq_len(n_samp)) {
      idx <- row_id:(row_id + n_vertices - 1L)
      sample_mat[idx, ] <- samples[, s, ]
      row_id <- row_id + n_vertices
    }

    knn_res <- FNN::get.knnx(volgrid, sample_mat, k = 1)
    vals <- vol[indices[knn_res$nn.index[, 1]]]
    dists <- knn_res$nn.dist[, 1]

    vals_mat <- matrix(vals, nrow = n_vertices, ncol = n_samp, byrow = FALSE)
    dist_mat <- matrix(dists, nrow = n_vertices, ncol = n_samp, byrow = FALSE)

    mapped_vals <- vapply(seq_len(n_vertices), function(i) {
      vvals <- vals_mat[i, ]
      vd <- dist_mat[i, ]
      keep <- which(vd < dthresh)
      if (!length(keep)) {
        return(fill)
      }
      vvals <- vvals[keep]
      vd <- vd[keep]
      if (fun == "avg") {
        wts <- hk(vd, sigma = sigma)
        wts <- wts / sum(wts)
        sum(wts * vvals)
      } else if (fun == "nn") {
        vvals[which.min(vd)]
      } else if (fun == "mode") {
        get_mode(vvals)
      } else {
        stop("Unknown mapping function.")
      }
    }, numeric(1))
  }

  faces <- t(surf_wm@mesh$it)
  mapped_vals <- .ns_surface_smooth_mm(
    mapped_vals, va, faces, surface_smooth_fwhm
  )
  NeuroSurface(surf_wm, seq_along(mapped_vals), mapped_vals)
}


#' Build a reusable surface sampler for multi-frame volumes
#'
#' @description
#' Precompute voxel neighbors and distances for each surface vertex so that
#' repeated volume-to-surface projections (e.g., 4D time series) can be done
#' quickly without rebuilding nearest-neighbor searches.
#'
#' @param surf_wm White-matter (inner) surface, \code{SurfaceGeometry}.
#' @param surf_pial Pial (outer) surface, \code{SurfaceGeometry}.
#' @param vol_template A \code{NeuroVol} used to define voxel space and
#'   candidate voxels (via \code{mask} or non-zero entries).
#' @param mask Optional mask limiting candidate voxels; if \code{NULL}, all
#'   non-zero voxels in \code{vol_template} are used.
#' @inheritParams vol_to_surf
#' @param radius Radius (in voxel units) for normal-line sampling when
#'   \code{sampling = "normal_line"}; also used as the distance scale when
#'   interpreting \code{depth} offsets for that mode.
#'
#' @return A list with class \code{"surface_sampler"} containing precomputed
#'   neighbor indices and distances for each vertex.
#'
#' @examples
#' \donttest{
#' # Requires white and pial surfaces plus a template volume
#' # wm <- read_surf_geometry("lh.white")
#' # pial <- read_surf_geometry("lh.pial")
#' # template_vol <- neuroim2::read_vol("template.nii")
#' # sampler <- surface_sampler(wm, pial, template_vol)
#' }
#'
#' @export
surface_sampler <- function(surf_wm, surf_pial, vol_template,
                            mask = NULL,
                            sampling = c("midpoint", "normal_line", "thickness"),
                            n_samples = NULL,
                            depth = NULL,
                            radius = 3,
                            knn = 6,
                            dthresh = 16) {
  sampling <- match.arg(sampling)

  # Get vertices in surface coordinates and transform to world coordinates
  va_surf <- vertices(surf_wm)
  vb_surf <- vertices(surf_pial)
  xform <- surf_to_world(surf_wm)
  va <- .ns_apply_affine(va_surf, xform)
  vb <- .ns_apply_affine(vb_surf, xform)

  if (!is.null(mask)) {
    mask_vec <- as.logical(mask[])
    if (length(mask_vec) != length(vol_template)) {
      stop("mask must be the same size as vol_template")
    }
    indices <- which(mask_vec & !is.na(vol_template[]))
  } else {
    indices <- which(!is.na(vol_template[]) & vol_template[] != 0)
  }

  if (length(indices) == 0) {
    stop("No voxels available for sampler construction (check mask or template volume).")
  }

  volgrid <- neuroim2::index_to_coord(vol_template, indices)

  samples <- .ns_make_samples(
    va, vb, t(surf_wm@mesh$it),
    sampling = sampling,
    n_samples = n_samples,
    depth = depth,
    radius = radius
  )

  n_vertices <- dim(samples)[1]
  n_samp <- dim(samples)[2]

  sample_mat <- matrix(NA_real_, nrow = n_vertices * n_samp, ncol = 3)
  row_id <- 1L
  for (s in seq_len(n_samp)) {
    idx <- row_id:(row_id + n_vertices - 1L)
    sample_mat[idx, ] <- samples[, s, ]
    row_id <- row_id + n_vertices
  }

  k_eff <- min(knn, nrow(volgrid))
  if (k_eff < 1) {
    stop("Not enough voxels to build sampler.")
  }

  nn_res <- FNN::get.knnx(volgrid, sample_mat, k = k_eff)

  idx_array <- array(NA_integer_, dim = c(n_vertices, n_samp, k_eff))
  dist_array <- array(NA_real_, dim = c(n_vertices, n_samp, k_eff))

  row_id <- 1L
  for (s in seq_len(n_samp)) {
    idx <- row_id:(row_id + n_vertices - 1L)
    idx_array[, s, ] <- nn_res$nn.index[idx, , drop = FALSE]
    dist_array[, s, ] <- nn_res$nn.dist[idx, , drop = FALSE]
    row_id <- row_id + n_vertices
  }

  structure(
    list(
      indices = indices,
      nn_index = idx_array,
      nn_dist = dist_array,
      geometry = surf_wm,
      params = list(
        sampling = sampling,
        n_samples = n_samples,
        depth = depth,
        radius = radius,
        knn = k_eff,
        dthresh = dthresh
      )
    ),
    class = "surface_sampler"
  )
}

#' Apply a precomputed surface sampler to a volume
#'
#' @param sampler A sampler object returned by \code{surface_sampler()}.
#' @param vol A \code{NeuroVol} with the same grid as the template used to
#'   build the sampler.
#' @param fun Aggregation function: "avg", "nn", or "mode".
#' @param sigma Bandwidth for Gaussian weights when \code{fun = "avg"}.
#' @param fill Value used when no valid voxels fall within \code{dthresh}.
#'
#' @return \code{NeuroSurface} with mapped data.
#'
#' @examples
#' \donttest{
#' # Requires surface sampler and volume data
#' # sampler <- surface_sampler(geometry, vol)
#' # result <- apply_surface_sampler(sampler, vol)
#' }
#'
#' @export
apply_surface_sampler <- function(sampler, vol, fun = c("avg", "nn", "mode"),
                                  sigma = 8, fill = 0) {
  fun <- match.arg(fun)
  .ns_apply_surface_sampler(sampler, vol, fun = fun, sigma = sigma, fill = fill)
}

#' @noRd
.ns_apply_surface_sampler <- function(sampler, vol, fun, sigma, fill) {
  stopifnot(inherits(sampler, "surface_sampler"))
  indices <- sampler$indices
  stopifnot(length(indices) > 0)

  vals <- vol[indices]
  n_vertices <- dim(sampler$nn_index)[1]
  n_samp <- dim(sampler$nn_index)[2]
  k_eff <- dim(sampler$nn_index)[3]

  mapped_vals <- numeric(n_vertices)
  dthresh <- sampler$params$dthresh

  for (i in seq_len(n_vertices)) {
    idxs <- as.integer(sampler$nn_index[i, , ])
    dists <- as.numeric(sampler$nn_dist[i, , ])

    keep <- which(dists < dthresh)
    if (!length(keep)) {
      mapped_vals[i] <- fill
      next
    }
    flat_idx <- idxs[keep]
    flat_dist <- dists[keep]
    vox_vals <- vals[flat_idx]

    if (fun == "avg") {
      wts <- hk(flat_dist, sigma = sigma)
      wts <- wts / sum(wts)
      mapped_vals[i] <- sum(wts * vox_vals)
    } else if (fun == "nn") {
      mapped_vals[i] <- vox_vals[which.min(flat_dist)]
    } else if (fun == "mode") {
      mapped_vals[i] <- get_mode(vox_vals)
    } else {
      stop("Unknown fun")
    }
  }

  NeuroSurface(geometry = sampler$geometry,
               indices = seq_along(mapped_vals),
               data = mapped_vals)
}

#' Extract sparse matrix triplets from a surface sampler
#'
#' @description
#' Converts a precomputed `surface_sampler` into sparse matrix triplet format
#' (i, j, x) suitable for constructing a dgCMatrix or for interop with other
#' packages (e.g., neurofunctor). The triplets represent a vertices × voxels
#' projection matrix with normalized Gaussian weights.
#'
#' @param sampler A sampler object returned by \code{surface_sampler()}.
#' @param sigma Bandwidth for Gaussian kernel weights. If NULL, uses the
#'   default from the sampler's dthresh (dthresh/2).
#' @param normalize Logical; if TRUE (default), weights for each vertex sum to 1.
#' @param min_weight Minimum weight threshold; entries below this are dropped.
#'   Default is 1e-10 to remove numerical noise.
#'
#' @return A list with class \code{"vol2surf_triplets"} containing:
#'   \describe{
#'     \item{i}{Integer vector of vertex indices (1-based row indices)}
#'     \item{j}{Integer vector of voxel indices (1-based column indices into
#'       the volume's linear index space, corresponding to sampler$indices)}
#'     \item{x}{Numeric vector of weights}
#'     \item{dims}{Integer vector c(n_vertices, n_voxels) for matrix dimensions}
#'     \item{voxel_indices}{The sampler$indices mapping j values to volume positions}
#'     \item{n_vertices}{Number of surface vertices}
#'     \item{n_voxels}{Number of candidate voxels}
#'     \item{nnz}{Number of non-zero entries}
#'     \item{params}{List of parameters used (sigma, normalize, min_weight, dthresh)}
#'   }
#'
#' @details
#' The output triplets define a sparse matrix P where P[i,j] is the weight

#' for vertex i from voxel j. The actual volume voxel index is
#' \code{voxel_indices[j]}. To construct a dgCMatrix in R:
#'
#' \code{Matrix::sparseMatrix(i = triplets$i, j = triplets$j, x = triplets$x,
#'                            dims = triplets$dims)}
#'
#' @seealso \code{\link{surface_sampler}}, \code{\link{apply_surface_sampler}}
#'
#' @examples
#' \donttest{
#' # Requires a surface sampler
#' # sampler <- surface_sampler(wm, pial, template_vol)
#' # triplets <- sampler_to_triplets(sampler)
#' }
#'
#' @export
sampler_to_triplets <- function(sampler, sigma = NULL, normalize = TRUE,
                                 min_weight = 1e-10) {
  stopifnot(inherits(sampler, "surface_sampler"))

  # Extract dimensions
  n_vertices <- dim(sampler$nn_index)[1]
  n_samp <- dim(sampler$nn_index)[2]
  k_eff <- dim(sampler$nn_index)[3]
  n_voxels <- length(sampler$indices)
  dthresh <- sampler$params$dthresh

  # Default sigma if not provided
  if (is.null(sigma)) {
    sigma <- dthresh / 2
  }

  # Pre-allocate lists for triplets (will be combined later)
  # Maximum possible entries: n_vertices * n_samp * k_eff
  max_entries <- n_vertices * n_samp * k_eff
  i_vec <- integer(max_entries)
  j_vec <- integer(max_entries)
  x_vec <- numeric(max_entries)
  ptr <- 0L

  # Track coverage (vertices with at least one valid voxel)
  valid_vertices <- logical(n_vertices)

  for (v in seq_len(n_vertices)) {
    # Flatten indices and distances across samples and neighbors
    idxs <- as.integer(sampler$nn_index[v, , ])
    dists <- as.numeric(sampler$nn_dist[v, , ])

    # Filter by distance threshold
    keep <- which(dists < dthresh)
    if (length(keep) == 0) {
      next
    }

    valid_vertices[v] <- TRUE
    flat_idx <- idxs[keep]
    flat_dist <- dists[keep]

    # Compute Gaussian weights
    wts <- hk(flat_dist, sigma = sigma)

    # Aggregate weights by unique voxel index (same voxel may appear multiple times)
    unique_idx <- unique(flat_idx)
    agg_wts <- numeric(length(unique_idx))
    for (u in seq_along(unique_idx)) {
      agg_wts[u] <- sum(wts[flat_idx == unique_idx[u]])
    }

    # Normalize if requested
    if (normalize && sum(agg_wts) > 0) {
      agg_wts <- agg_wts / sum(agg_wts)
    }

    # Filter by minimum weight
    keep_wt <- which(agg_wts >= min_weight)
    if (length(keep_wt) == 0) {
      next
    }

    n_add <- length(keep_wt)
    idx_range <- (ptr + 1L):(ptr + n_add)

    i_vec[idx_range] <- v
    j_vec[idx_range] <- unique_idx[keep_wt]
    x_vec[idx_range] <- agg_wts[keep_wt]
    ptr <- ptr + n_add
  }

  # Trim to actual size
  if (ptr > 0) {
    i_vec <- i_vec[1:ptr]
    j_vec <- j_vec[1:ptr]
    x_vec <- x_vec[1:ptr]
  } else {
    i_vec <- integer(0)
    j_vec <- integer(0)
    x_vec <- numeric(0)
  }

  structure(
    list(
      i = i_vec,
      j = j_vec,
      x = x_vec,
      dims = c(n_vertices, n_voxels),
      voxel_indices = sampler$indices,
      n_vertices = n_vertices,
      n_voxels = n_voxels,
      nnz = length(i_vec),
      coverage = sum(valid_vertices),
      valid_vertices = which(valid_vertices),
      params = list(
        sigma = sigma,
        normalize = normalize,
        min_weight = min_weight,
        dthresh = dthresh,
        sampling = sampler$params$sampling
      )
    ),
    class = "vol2surf_triplets"
  )
}

#' @export
print.vol2surf_triplets <- function(x, ...) {
  cat("vol2surf_triplets object\n")
  cat("------------------------\n")
  cat(sprintf("  Vertices: %d\n", x$n_vertices))
  cat(sprintf("  Voxels:   %d\n", x$n_voxels))

  cat(sprintf("  Non-zeros: %d (%.1f%% sparse)\n",
              x$nnz, 100 * (1 - x$nnz / (x$n_vertices * x$n_voxels))))
  cat(sprintf("  Coverage: %d/%d vertices (%.1f%%)\n",
              x$coverage, x$n_vertices, 100 * x$coverage / x$n_vertices))
  cat(sprintf("  Avg nnz/row: %.1f\n", x$nnz / max(x$coverage, 1)))
  cat("\nParameters:\n")
  cat(sprintf("  sigma:     %.2f\n", x$params$sigma))
  cat(sprintf("  normalize: %s\n", x$params$normalize))
  cat(sprintf("  dthresh:   %.2f\n", x$params$dthresh))
  cat(sprintf("  sampling:  %s\n", x$params$sampling))
  invisible(x)
}
