% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R
\name{add_code_to_theme}
\alias{add_code_to_theme}
\title{Add code to theme in hierarchy}
\usage{
add_code_to_theme(node, code_name, theme_path, description = "")
}
\arguments{
\item{node}{Root node of the hierarchy tree}

\item{code_name}{Character string specifying the name of the code to add}

\item{theme_path}{Character vector specifying the path to the target theme}

\item{description}{Optional character string providing a description of the code}
}
\value{
Updated node with new code added
}
\description{
Adds a new code to a specific theme in the code hierarchy. The code can be added
to the root level or nested within existing themes.
}
\keyword{internal}
