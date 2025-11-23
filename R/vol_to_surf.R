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
                        fun = c("avg", "nn", "mode"), knn = 6, sigma = 8, dthresh = sigma * 2,
                        fill = 0) {
  fun <- match.arg(fun)
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

  NeuroSurface(surf_wm, seq_along(mapped_vals), mapped_vals)
}
