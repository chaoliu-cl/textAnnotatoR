% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R, R/project.R
\name{save_as_text}
\alias{save_as_text}
\title{Save annotated text as plain text}
\usage{
save_as_text(filename, rv)

save_as_text(filename, rv)
}
\arguments{
\item{filename}{Character string specifying output file path}

\item{rv}{ReactiveValues object containing:
\itemize{
\item text: Original text content
\item annotations: Data frame of annotations
}}
}
\value{
Invisible NULL, called for side effect

Invisible NULL, called for side effect
}
\description{
Creates a plain text file containing the annotated text with code markers.

Creates a plain text file containing the annotated text with code markers.
}
\keyword{internal}
