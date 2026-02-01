#' Map a volume to surface after SDF-based rigid alignment
#'
#' This wrapper estimates a rigid transform from the input volume to the
#' template surface space using a signed distance field (SDF), then reuses
#' \code{vol_to_surf()} for the actual sampling in volume space.
#'
#' @param surf_wm White-matter (inner) surface in template space
#' @param surf_pial Pial (outer) surface in template space
#' @param vol NeuroVol in (approximate) template/MNI space
#' @param sdf_vol NeuroVol SDF of the template cortical surface
#' @param mask optional mask for hull extraction and mapping
#' @param n_hull number of hull points to retain (approximate)
#' @param thresh optional intensity threshold for hull computation
#' @param init_par optional initial parameters (tx, ty, tz, rx, ry, rz)
#' @param outside_penalty value used when SDF sampling is outside the grid
#' @param method optimization method passed to roptim (default "L-BFGS-B")
#' @param ... passed through to \code{vol_to_surf()} (e.g., \code{fun}, \code{knn}, \code{sigma})
#'
#' @return A \code{NeuroSurface} with values mapped from \code{vol}
#'
#' @examples
#' \donttest{
#' # Requires surfaces and volumes in template space
#' # wm <- read_surf_geometry("lh.white")
#' # pial <- read_surf_geometry("lh.pial")
#' # vol <- neuroim2::read_vol("mydata.nii")
#' # sdf <- neuroim2::read_vol("template_sdf.nii")
#' # result <- vol_to_surf_sdf(wm, pial, vol, sdf)
#' }
#'
#' @export
vol_to_surf_sdf <- function(surf_wm,
                            surf_pial,
                            vol,
                            sdf_vol,
                            mask = NULL,
                            n_hull = 2000L,
                            thresh = NULL,
                            init_par = rep(0, 6),
                            outside_penalty = 20,
                            method = "L-BFGS-B",
                            ...) {
  hull_world <- compute_hull_world(vol,
                                   mask = mask,
                                   n_points = n_hull,
                                   thresh = thresh)

  if (nrow(hull_world) == 0L) {
    stop("compute_hull_world() returned zero hull points")
  }

  fit <- estimate_sdf_rigid(hull_world,
                            sdf_vol,
                            init_par = init_par,
                            outside_penalty = outside_penalty,
                            method = method)

  T_vol2surf <- fit$T
  T_surf2vol <- solve(T_vol2surf)

  surf_wm_vol <- transform_surface_geometry(surf_wm, T_surf2vol)
  surf_pial_vol <- transform_surface_geometry(surf_pial, T_surf2vol)

  vol_to_surf(surf_wm_vol,
              surf_pial_vol,
              vol,
              mask = mask,
              ...)
}

