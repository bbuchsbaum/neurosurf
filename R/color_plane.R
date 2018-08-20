#' @include all_class.R
#' @include all_generic.R
NULL


#' @keywords internal
#' @noRd
IntensityColorPlane <- function(intensity, cols=rainbow(255), alpha=1) {
  new("IntensityColorPlane", intensity=intensity, colmap=cols, alpha=alpha)
}

#' @keywords internal
#' @noRd
RGBColorPlane <- function(clrs) {
  stopifnot(is.matrix(clrs))
  if (ncol(clrs) == 3) {
    clrs <- cbind(clrs, 255)
  }

  if (ncol(clrs) != 4) {
    stop("'ncol' of 'clrs' must be 4 (r,g,b,a)")
  }
  new("RGBColorPlane", clrs=clrs)
}

#' @keywords internal
#' @noRd
ConstantColorPlane <- function(clr) {
  stopifnot(is.character(clr))
  new("ConstantColorPlane", clrs=clr)
}

#' @keywords internal
#' @noRd
HexColorPlane <- function(clr) {
  stopifnot(is.character(clr))
  new("HexColorPlane", clrs=clr)
}

#' @keywords internal
#' @noRd
rgb2hex <- function(r,g,b, alpha) rgb(r, g, b, alpha, maxColorValue = 255)


setMethod("blend_colors", signature(bottom="ColorPlane", top="ColorPlane", alpha="numeric"),
          def=function(bottom, top, alpha=1) {

            rgb1 <- as_rgb(bottom)
            rgb2 <- as_rgb(top)

            achan <- alpha_channel(top)
            alpha <- achan * alpha

            clrs <- (1-alpha)*rgb1[,1:3,drop=FALSE] + alpha*rgb2[,1:3,drop=FALSE]
            RGBColorPlane(clrs)

          })

setMethod("blend_colors", signature(bottom="ColorPlane", top="ColorPlane", alpha="missing"),
          def=function(bottom, top) {

            rgb1 <- as_rgb(bottom)
            rgb2 <- as_rgb(top)

            alpha <- alpha_channel(top)
            clrs <- (1-alpha)*rgb1[,1:3,drop=FALSE] + alpha*rgb2[,1:3,drop=FALSE]
            RGBColorPlane(clrs)

          })

setMethod("blend_colors", signature(bottom="HexColorPlane", top="RGBColorPlane", alpha="numeric"),
          def=function(bottom, top, alpha) {
            bottom <- as_rgb(bottom)

            alpha <- alpha_channel(top) * alpha
            ## multiple constant alpha with alpha channel of top level
            clrs <- (1-alpha)*bottom[,1:3,drop=FALSE] + alpha*top@clrs[,1:3,drop=FALSE]
            RGBColorPlane(clrs)
          })

setMethod("as_rgb", signature(x="RGBColorPlane"),
          def=function(x) x@clrs)

setMethod("as_rgb", signature(x="HexColorPlane"),
          def=function(x) t(col2rgb(x@clrs, alpha=TRUE)))


setMethod("as_hexcol", signature(x="RGBColorPlane"),
          def=function(x) {
            if (ncol(x@clrs) == 4) {
              rgb2hex(x@clrs[,1], x@clrs[,2], x@clrs[,3], x@clrs[,4])
            } else if (ncol(x@clrs) == 3) {
              rgb2hex(x@clrs[,1], x@clrs[,2], x@clrs[,3])
            } else {
              stop("as_hexcol: 'x' must have 3 or 4 columns")
            }
          })

setMethod("as_hexcol", signature(x="HexColorPlane"),
          def=function(x) x@clrs)

setMethod("alpha_channel", signature(x="HexColorPlane"),
          def=function(x, normalize=TRUE) {
            if (normalize) {
              col2rgb(x@clrs, alpha=TRUE)[4,]/255
            } else {
              col2rgb(x@clrs, alpha=TRUE)[4,]
            }
          })

setMethod("alpha_channel", signature(x="RGBColorPlane"),
          def=function(x, normalize=TRUE) {
            if (normalize) {
              x@clrs[,4]/255
            } else {
              x@clrs[,4]
            }
          })

setMethod("map_colors", signature=c("ConstantColorPlane"),
          def=function(x) {
            new("HexColorPlane", clrs=x@clrs)
          })


setMethod("map_colors", signature=c("IntensityColorPlane"),
          def=function(x, alpha=1, threshold=NULL, irange=NULL) {
            #browser()
            if (is.null(irange)) {
              irange <- range(x@intensity)
              clrs <- x@colmap[as.integer((x@intensity - irange[1])/ diff(irange) * (length(x@colmap) -1) + 1)]
            } else {
              full_range <- range(x@intensity)
              irange <- c(max(irange[1], full_range[1]), min(irange[2], full_range[2]))
              icol <- as.integer((x@intensity - irange[1])/diff(irange) * (length(x@colmap) -1) + 1)
              icol[icol < 1] <- 1
              icol[icol > length(x@colmap)] <- length(x@colmap)
              clrs <- x@colmap[icol]
            }

            if (!is.null(threshold)) {
              clrs <- col2rgb(clrs, alpha=TRUE)
              if (length(threshold) == 1) {
                trans <- x@intensity < threshold
                clrs[4,trans] <- 0
              } else if (length(threshold) == 2) {
                cat("thresholding ", threshold)
                trans <- x@intensity > threshold[1] & x@intensity < threshold[2]
                clrs[4,trans] <- 0

              } else {
                stop("threshold must be a numeric vector with 1 or 2 elements")
              }

              new("RGBColorPlane", clrs=t(clrs))

            } else {
              new("HexColorPlane", clrs=clrs)
            }


          })
