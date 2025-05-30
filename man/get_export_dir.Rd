% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/directory_management.R
\name{get_export_dir}
\alias{get_export_dir}
\title{Get export directory}
\usage{
get_export_dir()
}
\value{
Character string containing the path to the exports directory
}
\description{
Returns the path to the exports directory, creating it if necessary.
This directory is used for storing exported annotation data, visualizations,
and analysis results. The function ensures the directory exists and is
accessible before returning its path.
}
\keyword{internal}
