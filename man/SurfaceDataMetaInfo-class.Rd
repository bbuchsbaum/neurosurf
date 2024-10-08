% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/all_class.R
\docType{class}
\name{SurfaceDataMetaInfo-class}
\alias{SurfaceDataMetaInfo-class}
\title{SurfaceDataMetaInfo}
\description{
This class contains meta information for surface-based data (the values that map to a surface geometry)
}
\section{Slots}{

\describe{
\item{\code{header_file}}{name of the file containing meta information}

\item{\code{data_file}}{name of the file containing data}

\item{\code{file_descriptor}}{descriptor of image file format}

\item{\code{node_count}}{the number of nodes for which surface data exists}

\item{\code{nels}}{the number of data vectors (typically the number of columns in the surface data matrix; nels = 1 for a single surface data set)}

\item{\code{label}}{a label indicating the type of surface (e.g. white, pial, inflated, flat, spherical)}
}}

