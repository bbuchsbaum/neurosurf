#' Compute a sparse hull of boundary voxels in world space
#'
#' @param vol NeuroVol (from neuroim2)
#' @param mask optional logical vector or NeuroVol mask in the same space
#' @param n_points approximate maximum number of hull points (default 2000)
#' @param thresh numeric threshold; if \code{NULL}, uses median of non-zero voxels
#'
#' @return numeric matrix with columns x, y, z in world coordinates
#' @noRd
compute_hull_world <- function(vol,
                               mask = NULL,
                               n_points = 2000L,
                               thresh = NULL) {
  stopifnot(inherits(vol, "NeuroVol"))

  vals <- as.numeric(vol[])
  dims <- dim(vol)
  sp <- neuroim2::space(vol)
  grid2world <- neuroim2::trans(sp)

  if (is.null(thresh)) {
    nz <- vals[vals != 0]
    if (length(nz) == 0L) {
      stop("volume has no non-zero voxels")
    }
    thresh <- stats::quantile(nz, probs = 0.5)
  }

  if (!is.null(mask)) {
    if (inherits(mask, "NeuroVol")) {
      mask_vals <- as.logical(mask[])
    } else {
      mask_vals <- as.logical(mask)
    }
    if (length(mask_vals) != length(vals)) {
      stop("mask must have same length as vol")
    }
  } else {
    mask_vals <- logical(0)
  }

  compute_hull_world_cpp(
    vol = vals,
    dims = as.integer(dims),
    grid2world = grid2world,
    thresh = as.numeric(thresh),
    n_points = as.integer(n_points),
    mask = mask_vals
  )
}

#' Estimate rigid transform from hull points to SDF grid
#'
#' @param hull_world numeric matrix (n x 3) of hull points in world coordinates
#' @param sdf_vol NeuroVol containing signed-distance values of the template surface
#' @param init_par length-6 numeric vector (tx, ty, tz, rx, ry, rz), translations in mm and rotations in radians
#' @param outside_penalty value returned when sampling outside the SDF grid
#' @param method optimization method passed to roptim (default "L-BFGS-B")
#'
#' @return list with elements \code{par}, \code{T}, and \code{value}
#' @noRd
estimate_sdf_rigid <- function(hull_world,
                               sdf_vol,
                               init_par = rep(0, 6),
                               outside_penalty = 20,
                               method = "L-BFGS-B") {
  if (!is.matrix(hull_world) || ncol(hull_world) != 3) {
    stop("hull_world must be an n x 3 numeric matrix")
  }
  stopifnot(inherits(sdf_vol, "NeuroVol"))

  sp_sdf <- neuroim2::space(sdf_vol)
  world2grid <- neuroim2::inverse_trans(sp_sdf)
  trans_sdf <- neuroim2::trans(sp_sdf)

  if (length(init_par) != 6) {
    stop("init_par must have length 6")
  }

  if (all(init_par == 0)) {
    hull_ctr <- colMeans(hull_world)
    center_grid <- (dim(sdf_vol) + 1) / 2
    center_world <- as.numeric(c(center_grid, 1) %*% t(trans_sdf))[1:3]
    init_par[1:3] <- center_world - hull_ctr
  }

  estimate_sdf_rigid_cpp(
    hull_world = as.matrix(hull_world),
    sdf = as.numeric(sdf_vol[]),
    sdf_dim = as.integer(dim(sdf_vol)),
    world2grid_sdf = world2grid,
    init_par = as.numeric(init_par),
    outside_penalty = as.numeric(outside_penalty),
    method = method
  )
}

#' Apply a 4x4 world-space transform to SurfaceGeometry vertices
#'
#' @param surf SurfaceGeometry
#' @param T 4x4 numeric matrix (homogeneous transform)
#'
#' @return SurfaceGeometry with transformed vertices
#' @noRd
transform_surface_geometry <- function(surf, T) {
  stopifnot(inherits(surf, "SurfaceGeometry"))
  if (!is.matrix(T) || !all(dim(T) == c(4, 4))) {
    stop("T must be a 4x4 numeric matrix")
  }

  v <- coords(surf)
  homo <- cbind(v, 1)
  v_new <- (homo %*% t(T))[, 1:3, drop = FALSE]

  faces <- t(surf@mesh$it) - 1L
  SurfaceGeometry(vert = v_new,
                  faces = faces,
                  hemi = surf@hemi,
                  label = surf@label)
}
