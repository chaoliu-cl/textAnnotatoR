% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R
\name{find_code_transitions}
\alias{find_code_transitions}
\title{Find transitions between codes}
\usage{
find_code_transitions(annotations)
}
\arguments{
\item{annotations}{Data frame of sorted annotations with columns:
\itemize{
\item start: numeric, starting position
\item end: numeric, ending position
\item code: character, code identifier
}}
}
\value{
List of code transitions, each containing:
\itemize{
\item from: Source code
\item to: Target code
}
}
\description{
Identifies and analyzes transitions between consecutive code applications
to understand coding sequence patterns.
}
\keyword{internal}
