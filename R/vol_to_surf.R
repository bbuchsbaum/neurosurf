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
#' @param radius Radius (in voxel units) for normal-line sampling when
#'   \code{sampling = "normal_line"}; also used as the distance scale when
#'   interpreting \code{depth} offsets for that mode.
#' @param sampler Optional surface sampler object created by \code{surface_sampler()}.
#'   When provided, the sampler is reused and other sampling-related arguments are ignored.
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
                        sampler = NULL) {
  fun <- match.arg(fun)
  sampling <- match.arg(sampling)

  if (!is.null(sampler)) {
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
#' @param radius Radius (in voxel units) for normal-line sampling when
#'   \code{sampling = "normal_line"}; also used as the distance scale when
#'   interpreting \code{depth} offsets for that mode.
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
