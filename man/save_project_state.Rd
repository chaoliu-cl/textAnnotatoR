% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R, R/project.R
\name{save_project_state}
\alias{save_project_state}
\title{Save and manage project state}
\usage{
save_project_state(state, filename)

save_project_state(state, filename)
}
\arguments{
\item{state}{List containing project state elements:
\itemize{
\item text: Character string of the current text
\item annotations: Data frame of annotations
\item codes: Character vector of codes
\item code_tree: Node object representing code hierarchy
\item code_colors: Named vector of code colors
\item memos: List of memos
\item code_descriptions: List of code descriptions
}}

\item{filename}{Character string specifying the filename for saving}
}
\value{
Invisible NULL, called for side effect of saving project state

Invisible NULL. Called for side effect of saving project state.
}
\description{
Saves the current state of a text annotation project, including annotations,
codes, and memos. Creates necessary directories and handles file operations
safely.

Saves the current project state including text, annotations, codes, code tree,
and other metadata to an RDS file. Creates the projects directory if it doesn't exist.
}
\keyword{internal}
