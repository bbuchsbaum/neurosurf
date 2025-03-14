% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_class.R
\docType{class}
\name{ROISurfaceVector-class}
\alias{ROISurfaceVector-class}
\title{ROISurfaceVector}
\description{
A class that respresents a surface-based region of interest
}
\section{Slots}{

\describe{
\item{\code{geometry}}{the geometry of the parent surface: a \code{SurfaceGeometry} instance}

\item{\code{data}}{\code{matrix} data stored in ROI with number of columns equal to number of coordinates in ROI.}

\item{\code{coords}}{the surface-based coordinates of the data}

\item{\code{indices}}{the nodes of the parent surface stored in the \code{geometry} field.}
}}

