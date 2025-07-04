% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R
\name{sync_codes_with_hierarchy}
\alias{sync_codes_with_hierarchy}
\title{Sync codes with hierarchy}
\usage{
sync_codes_with_hierarchy(rv)

sync_codes_with_hierarchy(rv)
}
\arguments{
\item{rv}{ReactiveValues object containing codes and code_tree}
}
\value{
Updated rv object

Updated rv object
}
\description{
Ensures all codes in rv$codes are represented in the code hierarchy.
Adds any missing codes to the root node automatically.

Ensures all codes in rv$codes are represented in the code hierarchy.
Adds any missing codes to the root node automatically.
}
\keyword{internal}
