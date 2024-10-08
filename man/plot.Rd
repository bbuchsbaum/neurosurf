% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/view_surface.R
\name{plot,SurfaceGeometry,missing-method}
\alias{plot,SurfaceGeometry,missing-method}
\alias{plot,NeuroSurface,missing-method}
\alias{plot,LabeledNeuroSurface,missing-method}
\title{plot a surface}
\usage{
\S4method{plot}{SurfaceGeometry,missing}(
  x,
  vals = NA,
  cmap = grDevices::gray(seq(0, 1, length.out = 255)),
  vert_clrs = NULL,
  irange = range(vals),
  thresh = c(0, 0),
  alpha = 1,
  specular = "black",
  bgcol = "lightgray",
  ...
)

\S4method{plot}{NeuroSurface,missing}(
  x,
  cmap = grDevices::gray(seq(0, 1, length.out = 255)),
  vert_clrs = NULL,
  irange = range(x@data, na.rm = TRUE),
  thresh = c(0, 0),
  alpha = 1,
  specular = "black",
  bgcol = "lightgray",
  ...
)

\S4method{plot}{LabeledNeuroSurface,missing}(
  x,
  cmap = x@cols,
  vert_clrs = NULL,
  irange = range(x@data, na.rm = TRUE),
  thresh = c(0, 0),
  alpha = 1,
  specular = "black",
  bgcol = "lightgray",
  ...
)
}
\arguments{
\item{x}{the surface to plot}

\item{vals}{A numeric vector of values corresponding to each surface node. These values will be mapped to colors using the provided color map (\code{cmap}).}

\item{cmap}{A color map consisting of a vector of colors in hex format. Default is \code{rainbow(256, alpha = 1)}. This color map is used to color the surface based on the \code{vals} vector.}

\item{vert_clrs}{Optional vertex colors in hex format. If provided, these colors will override the colors generated from \code{vals} and \code{cmap}.}

\item{irange}{A numeric vector of length 2 indicating the lower and upper bounds of the intensity range for the color scale. Default is the range of \code{vals}.}

\item{thresh}{A numeric vector of length 2 indicating the lower and upper transparency thresholds. Nodes with values outside this range will be made transparent.}

\item{alpha}{A numeric value indicating the transparency level of the surface. The default is 1 (fully opaque). Values should be between 0 (fully transparent) and 1 (fully opaque).}

\item{specular}{A color in hex format or a numeric value indicating the specular reflection color used for lighting. Default is \code{"white"}.}

\item{bgcol}{A color or vector of colors in hex format used to shade the surface background. Default is \code{"lightgray"}.}

\item{...}{extra args to send to \code{view_surface}}
}
\description{
plot a surface
}
