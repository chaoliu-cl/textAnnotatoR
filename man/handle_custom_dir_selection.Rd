% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/directory_management.R
\name{handle_custom_dir_selection}
\alias{handle_custom_dir_selection}
\title{Handle custom directory selection}
\usage{
handle_custom_dir_selection(input, output, session, roots)
}
\arguments{
\item{input}{Shiny input object}

\item{output}{Shiny output object}

\item{session}{Shiny session object}

\item{roots}{List of root directories for selection}
}
\value{
Character string containing the selected directory path
}
\description{
Opens a directory selection dialog and updates the UI with the selected path.
}
\keyword{internal}
