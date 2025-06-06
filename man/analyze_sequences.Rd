% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R
\name{analyze_sequences}
\alias{analyze_sequences}
\title{Analyze sequences and transitions between codes}
\usage{
analyze_sequences(annotations)
}
\arguments{
\item{annotations}{Data frame of text annotations with columns:
\itemize{
\item start: numeric, starting position of annotation
\item end: numeric, ending position of annotation
\item code: character, code applied to the annotation
}}
}
\value{
List containing:
\itemize{
\item transitions: List of transitions between consecutive codes
\item patterns: List of identified repeated code sequences
}
}
\description{
Analyzes how codes are sequenced in the text by examining transitions
between consecutive codes and identifying repeated patterns.
}
\keyword{internal}
