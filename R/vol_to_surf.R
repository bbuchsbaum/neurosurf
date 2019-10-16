
hk <- function(x, sigma=1) {
  exp((-x^2)/(2*sigma^2))
}


get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' map values from a 3d volume to a surface in the same coordinate space
#'
#' @param surf_wm the white matter (inner) surface
#' @param surf_pial the pial (outer) surface
#' @param vol an image volume of type \code{NeuroVol} that is to be mapped to surface
#' @param mask a mask defining the valid voxel in the image volume
#' @param fun the mapping function, which can be "avg" or "nn"
#' @param knn the number of nearest neighbors to consider (for "avg" mapping function)
#' @param sigma the bandwidth of the smoothing kernel (for the "avg" mapping function)
#' @param dthresh a valid mapping voxel only if it is less than \code{dthresh} units away from vertex.
#' TODO does this work?
#'
#' @examples
#'
#' volname <- system.file("inst/testdata/Schaefer2018_200Parcels_7Networks_order_FSLMNI152_1mm.nii", package="neurosurf")
#' vol <- neuroim2::read_vol(volname)
vol_to_surf <- function(surf_wm, surf_pial, vol, mask=NULL, fun=c("avg", "nn", "mode"), knn=6, sigma=8, dthresh=sigma*2) {
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
    if (!valid[i] || length(which(d < dthresh)) == 0) {
      NA
    } else {
      idx <- nnres$nn.index[i,]
      d <- nnres$nn.dist[i,]

      if (fun == "avg") {
        wts <- hk(d, sigma=sigma)
        wts <- wts/sum(wts)
        wts * vol[indices[idx]]
      } else if (fun == "nn") {
        vol[indices[idx[1]]]
      } else if (fun == "mode") {
        get_mode(vol[indices[idx]])
      } else {
        stop()
      }
    }
  })

  #v <- which(!is.na(mapped_vals))
  NeuroSurface(surf_wm, 1:length(mapped_vals), mapped_vals)
}
