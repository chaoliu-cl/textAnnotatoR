% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R, R/visualization.R
\name{plot_code_distribution}
\alias{plot_code_distribution}
\title{Plot code distribution visualization}
\usage{
plot_code_distribution(distribution, main = "", ...)

plot_code_distribution(distribution, main = "", ...)
}
\arguments{
\item{distribution}{List containing code distribution information:
\itemize{
\item frequencies: Named numeric vector containing code frequencies
}}

\item{main}{Character string specifying the plot title}

\item{...}{Additional arguments passed to barplot()}
}
\value{
Invisible NULL, called for side effect of creating plot

Invisible NULL, called for side effect of creating plot
}
\description{
Creates a barplot visualization showing the distribution of codes in the annotations.
The plot includes rotated labels for better readability and handles empty or NULL
input data gracefully.

Creates a barplot visualization showing the distribution of codes in the annotations.
The plot includes rotated labels for better readability and handles empty or NULL
input data gracefully.
}
\examples{
\dontrun{
# Create sample distribution data
dist <- list(
  frequencies = c(
    "Code1" = 5,
    "Code2" = 3,
    "Code3" = 7
  )
)

# Create basic plot
plot_code_distribution(dist)

# Create plot with custom title
plot_code_distribution(dist, main = "Code Distribution")

# Handle empty distribution
plot_code_distribution(list(frequencies = integer(0)))
}
\dontrun{
# Create sample distribution data
dist <- list(
  frequencies = c(
    "Code1" = 5,
    "Code2" = 3,
    "Code3" = 7
  )
)

# Create basic plot
plot_code_distribution(dist)

# Create plot with custom title
plot_code_distribution(dist, main = "Code Distribution")

# Handle empty distribution
plot_code_distribution(list(frequencies = integer(0)))
}
}
\keyword{internal}
