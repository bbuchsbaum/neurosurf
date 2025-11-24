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

#' @noRd
#' @keywords internal
.ns_vertex_normals <- function(coords, faces) {
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

  normals <- .ns_vertex_normals(base, faces)
  pts <- array(NA_real_, dim = c(n_vertices, n_samples, 3))
  for (i in seq_len(n_samples)) {
    pts[, i, ] <- base + normals * offsets[i]
  }
  pts
}

#' Map values from a 3D volume to a surface in the same coordinate space
#'
#' This function maps values from a 3D volume to a surface representation,
#' allowing for different mapping strategies.
#'
#' @param surf_wm The white matter (inner) surface, typically of class \code{SurfaceGeometry}.
#' @param surf_pial The pial (outer) surface, typically of class \code{SurfaceGeometry}.
#' @param vol An image volume of type \code{NeuroVol} that is to be mapped to the surface.
#' @param mask A mask defining the valid voxels in the image volume. If NULL, all non-zero voxels are considered valid.
#' @param fun The mapping function to use. Options are:
#'   \itemize{
#'     \item "avg": Average of nearby voxels (default)
#'     \item "nn": Nearest neighbor
#'     \item "mode": Most frequent value among nearby voxels
#'   }
#' @param knn The number of nearest neighbors to consider for mapping (default: 6).
#' @param sigma The bandwidth of the smoothing kernel for the "avg" mapping function (default: 8).
#' @param dthresh The maximum distance threshold for valid mapping. A voxel is only considered if it is less than \code{dthresh} units away from the vertex (default: 2 * sigma).
#' @param fill Value used when no nearby voxels are found (default: 0 to preserve previous behavior).
#' @param sampling How to place sample points relative to the white/pial pair.
#'   Options are:
#'   \itemize{
#'     \item "midpoint" (default): original behaviour, samples at the midpoint between white and pial.
#'     \item "thickness": samples along the white→pial line at fractions given by \code{depth} or evenly spaced.
#'     \item "normal_line": samples along the vertex normal centred on the midpoint, spanning \code{radius} in both directions (or using \code{depth} offsets).
#'   }
#' @param n_samples Number of samples per vertex for \code{sampling != "midpoint"} when \code{depth} is not supplied.
#' @param depth Optional numeric vector controlling sampling positions; interpreted as fractions of thickness (for "thickness") or multiples of \code{radius} (for "normal_line").
#' @param sampler Optional surface sampler object created by \code{surface_sampler()}.
#'   When provided, the sampler is reused and other sampling-related arguments are ignored.
#'
#' @return A \code{NeuroSurface} object containing the mapped values.
#'
#' @examples
#' \donttest{
#' # Load standard white and pial surfaces from the package
#' wm_surf_file <- system.file("extdata", "std.8.lh.white.asc", package = "neurosurf")
#' pial_surf_file <- system.file("extdata", "std.8.lh.pial.asc", package = "neurosurf")
#' 
#' surf_wm <- read_surf_geometry(wm_surf_file)
#' surf_pial <- read_surf_geometry(pial_surf_file)
#' 
#' # Load an example volume (replace with actual loading code later)
#' # vol <- neuroim2::read_vol("path/to/volume.nii")
#' 
#' # Example: Create a dummy volume for demonstration purposes
#' # This should be replaced with real volume data
#' library(neuroim2)
#' # Assume the surfaces are in a space roughly covered by this bounding box
#' # Adjust dimensions and origin based on your actual data alignment
#' bb <- matrix(c(-80, 80, -120, 80, -60, 90), 3, 2, byrow = TRUE)
#' spacing <- c(1, 1, 1)
#' dims <- ceiling(abs(bb[,2] - bb[,1]) / spacing)
#' origin <- bb[,1]
#' sp <- NeuroSpace(dims, spacing, origin)
#' vol <- NeuroVol(rnorm(prod(dims)), sp)
#' 
#' # Map volume to surface using average mapping
#' mapped_surf <- vol_to_surf(surf_wm, surf_pial, vol, fun = "avg")
#' print(summary(series(mapped_surf)))
#' 
#' # Map volume to surface using nearest neighbor mapping
#' mapped_surf_nn <- vol_to_surf(surf_wm, surf_pial, vol, fun = "nn")
#' print(summary(series(mapped_surf_nn)))
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
                        sampler = NULL) {
  fun <- match.arg(fun)
  sampling <- match.arg(sampling)

  if (!is.null(sampler)) {
    return(.ns_apply_surface_sampler(sampler, vol, fun = fun,
                                     sigma = sigma, fill = fill))
  }

  va <- vertices(surf_wm)
  vb <- vertices(surf_pial)
  vavg <- (va + vb) / 2

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
#'
#' @return A list with class \code{"surface_sampler"} containing precomputed
#'   neighbor indices and distances for each vertex.
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

  va <- vertices(surf_wm)
  vb <- vertices(surf_pial)

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
