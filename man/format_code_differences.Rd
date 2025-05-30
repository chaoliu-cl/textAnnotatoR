% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R
\name{format_code_differences}
\alias{format_code_differences}
\title{Format code difference analysis results}
\usage{
format_code_differences(differences)
}
\arguments{
\item{differences}{Either a character string containing direct analysis results
or a list containing:
\itemize{
\item pattern_summary: Character string summarizing code pattern differences
}}
}
\value{
Character string containing formatted code analysis results
}
\description{
Formats the results of code difference analysis into a human-readable
string, handling both character and list inputs appropriately.
}
\keyword{internal}
