% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R
\name{calculate_transitions}
\alias{calculate_transitions}
\title{Calculate transitions between consecutive codes}
\usage{
calculate_transitions(annotations)
}
\arguments{
\item{annotations}{Data frame containing annotations with columns:
\itemize{
\item start: numeric, starting position of annotation
\item end: numeric, ending position of annotation
\item code: character, code identifier
}}
}
\value{
List where each element is a named vector containing:
\itemize{
\item from: Character string of the source code
\item to: Character string of the target code
}
}
\description{
Analyzes the sequence of code applications to identify transitions between
consecutive codes in the text. Creates a list of code pairs representing
each transition from one code to another.
}
\details{
Transitions are identified by sorting annotations by position and then
analyzing consecutive pairs of codes. The function handles edge cases
and provides error-safe operation.
}
\keyword{internal}
