% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R, R/hierarchy.R
\name{add_theme}
\alias{add_theme}
\title{Add theme to code hierarchy}
\usage{
add_theme(node, theme_name, description = "")

add_theme(node, theme_name, description = "")
}
\arguments{
\item{node}{Root node of the hierarchy tree}

\item{theme_name}{Character string specifying the name of the new theme}

\item{description}{Optional character string providing a description of the theme}
}
\value{
Updated node with new theme added

Updated node with new theme added
}
\description{
Adds a new theme to the code hierarchy tree. Themes can be used to organize and
group related codes in a hierarchical structure.

Adds a new theme to the code hierarchy tree. Themes can be used to organize and
group related codes in a hierarchical structure.
}
\examples{
\dontrun{
# Create root node
root <- Node$new("Root")

# Add a new theme
root <- add_theme(root, "Methods", "Research methodology codes")

# Add a sub-theme
root <- add_theme(root, "Qualitative Methods", "Qualitative research approaches")
}

\dontrun{
# Create root node
root <- Node$new("Root")

# Add a new theme
root <- add_theme(root, "Methods", "Research methodology codes")

# Add a sub-theme
root <- add_theme(root, "Qualitative Methods", "Qualitative research approaches")
}

}
\keyword{internal}
