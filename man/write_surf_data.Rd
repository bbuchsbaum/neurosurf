% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IO.R
\name{write_surf_data}
\alias{write_surf_data}
\title{Write Surface Data to File}
\usage{
write_surf_data(surf, outstem, hemi = "")
}
\arguments{
\item{surf}{An object of class \code{NeuroSurface} or \code{NeuroSurfaceVector} containing the surface data to be written.}

\item{outstem}{A character string specifying the base name for the output file (without extension).}

\item{hemi}{A character string specifying the hemisphere ("lh" for left, "rh" for right). Default is an empty string.}
}
\value{
This function does not return a value. It writes the data to a .1D.dset file as a side effect.
}
\description{
This function writes surface data from a NeuroSurface or NeuroSurfaceVector object to a .1D.dset file.
}
\details{
The function writes the surface data to a .1D.dset file, which is a tabular data format.
The output file contains node indices in the first column, followed by data values in subsequent columns.
The file name is constructed by combining \code{outstem}, \code{hemi} (if provided), and the extension ".1D.dset".
For NeuroSurfaceVector objects, all columns of data are written. For NeuroSurface objects, only the single data vector is written.
The data is written without row names, column names, or quotes.
}
\examples{
\dontrun{
# Assuming 'surf_data' is a NeuroSurface or NeuroSurfaceVector object
write_surf_data(surf_data, "output_data", "lh")
# This will create a file named "output_data_lh.1D.dset"
}

}
