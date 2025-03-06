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
#' @param surf_wm The white matter (inner) surface, typically of class \code{NeuroSurface}.
#' @param surf_pial The pial (outer) surface, typically of class \code{NeuroSurface}.
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
#'
#' @return A \code{NeuroSurface} object containing the mapped values.
#'
#' @examples
#' \dontrun{
#' # Load example data (not included in package)
#' vol <- neuroim2::read_vol("path/to/volume.nii")
#' surf_wm <- read_surface("path/to/white_matter_surface.gii")
#' surf_pial <- read_surface("path/to/pial_surface.gii")
#'
#' # Map volume to surface using average mapping
#' mapped_surf <- vol_to_surf(surf_wm, surf_pial, vol, fun = "avg")
#'
#' # Map volume to surface using nearest neighbor mapping
#' mapped_surf_nn <- vol_to_surf(surf_wm, surf_pial, vol, fun = "nn")
#' }
#'
#' @export
#' @importFrom FNN get.knnx
#' @importFrom neuroim2 index_to_coord
vol_to_surf <- function(surf_wm, surf_pial, vol, mask = NULL, 
                        fun = c("avg", "nn", "mode"), knn = 6, sigma = 8, dthresh = sigma * 2) {
  fun <- match.arg(fun)
  va <- vertices(surf_wm)
  vb <- vertices(surf_pial)

  vavg <- (va + vb)/2

  volgrid <- if (!is.null(mask)) {
    indices <- which(vol != 0)
    neuroim2::index_to_coord(vol,indices)
  } else {
    indices <- which(vol!=0)
    neuroim2::index_to_coord(vol,indices)
  }

  nnres <- FNN::get.knnx(volgrid, vavg, k=knn)
  valid <- nnres$nn.dist[,1] < dthresh

  mapped_vals <- sapply(1:nrow(nnres$nn.index), function(i) {
    d <- nnres$nn.dist[i,]
    if (!valid[i] || length(which(d < dthresh)) == 0) {
      NA
    } else {
      idx <- nnres$nn.index[i,]


      if (fun == "avg") {
        wts <- hk(d, sigma=sigma)
        wts <- wts/sum(wts)
        sum(wts * vol[indices[idx]])
      } else if (fun == "nn") {
        vol[indices[idx[1]]]
      } else if (fun == "mode") {
        get_mode(vol[indices[idx]])
      } else {
        stop()
      }
    }
  })
  mapped_vals[is.na(mapped_vals)] <- 0
  #v <- which(!is.na(mapped_vals))
  NeuroSurface(surf_wm, 1:length(mapped_vals), mapped_vals)
}
