% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R
\name{compare_coverage}
\alias{compare_coverage}
\title{Compare coverage patterns between coding strategies}
\usage{
compare_coverage(coding_strategies)
}
\arguments{
\item{coding_strategies}{List of coding strategies, where each strategy contains:
\itemize{
\item coverage: List containing distribution information
\item frequencies: Table of code frequencies
}}
}
\value{
List containing:
\itemize{
\item total_codes_range: Numeric vector with min and max total codes
\item unique_codes_range: Numeric vector with min and max unique codes
}
}
\description{
Analyzes and compares the coverage patterns between different coding strategies,
including total codes used and unique code counts.
}
\keyword{internal}
