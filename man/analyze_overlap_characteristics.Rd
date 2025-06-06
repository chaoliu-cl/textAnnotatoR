% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R
\name{analyze_overlap_characteristics}
\alias{analyze_overlap_characteristics}
\title{Analyze characteristics of code overlaps}
\usage{
analyze_overlap_characteristics(overlaps)
}
\arguments{
\item{overlaps}{List of overlap information, where each element contains:
\itemize{
\item overlap_start: numeric, starting position of overlap
\item overlap_end: numeric, ending position of overlap
}}
}
\value{
List containing:
\itemize{
\item avg_length: Numeric value of average overlap length
\item total_overlaps: Integer count of total overlapping instances
}
}
\description{
Analyzes the characteristics of overlapping code applications by calculating
various metrics about overlap patterns.
}
\details{
Calculates metrics about code overlaps including the average length of
overlapping regions and the total number of overlaps. Returns empty list
for empty input.
}
\keyword{internal}
