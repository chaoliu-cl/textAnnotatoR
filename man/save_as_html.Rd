% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R, R/project.R
\name{save_as_html}
\alias{save_as_html}
\title{Save annotated text as HTML document}
\usage{
save_as_html(filename, rv)

save_as_html(filename, rv)
}
\arguments{
\item{filename}{Character string specifying output file path}

\item{rv}{ReactiveValues object containing:
\itemize{
\item text: Original text content
\item annotations: Data frame of annotations
\item code_colors: Named character vector of code colors
}}
}
\value{
Invisible NULL, called for side effect

Invisible NULL, called for side effect
}
\description{
Creates an HTML document containing the annotated text with proper styling
for code highlights and formatting.

Creates an HTML document containing the annotated text with proper styling
for code highlights and formatting.
}
\examples{
\dontrun{
# Create HTML with annotations
rv <- list(
  text = "Sample text",
  annotations = data.frame(
    start = 1, end = 6,
    code = "code1"
  ),
  code_colors = c(code1 = "#FF0000")
)
save_as_html("output.html", rv)
}
\dontrun{
# Create HTML with annotations
rv <- list(
  text = "Sample text",
  annotations = data.frame(
    start = 1, end = 6,
    code = "code1"
  ),
  code_colors = c(code1 = "#FF0000")
)
save_as_html("output.html", rv)
}
}
\keyword{internal}
