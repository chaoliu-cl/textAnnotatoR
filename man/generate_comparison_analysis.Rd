% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R, R/comparison.R
\name{generate_comparison_analysis}
\alias{generate_comparison_analysis}
\title{Compare coding patterns between different documents or coders}
\usage{
generate_comparison_analysis(annotations_list)

generate_comparison_analysis(annotations_list)
}
\arguments{
\item{annotations_list}{A list of data frames, where each data frame contains
annotations with columns:
\itemize{
\item start: numeric, starting position of annotation
\item end: numeric, ending position of annotation
\item code: character, code applied to the annotation
}}
}
\value{
A list containing comparison results and analysis:
\itemize{
\item coding_strategies: list of analyzed coding patterns for each input
\item comparison: list of comparative analyses between coding patterns
}

A list containing comparison results and analysis:
\itemize{
\item coding_strategies: list of analyzed coding patterns for each input
\item comparison: list of comparative analyses between coding patterns
}
}
\description{
Performs a comprehensive comparison of coding patterns between different sets of
annotations, analyzing differences in coverage, code application, overlaps, and
code sequences.

Performs a comprehensive comparison of coding patterns between different sets of
annotations, analyzing differences in coverage, code application, overlaps, and
code sequences.
}
\examples{
\dontrun{
# Create sample annotation sets
annotations1 <- data.frame(
  start = c(1, 10),
  end = c(5, 15),
  code = c("code1", "code2")
)
annotations2 <- data.frame(
  start = c(2, 12),
  end = c(6, 18),
  code = c("code1", "code3")
)
results <- generate_comparison_analysis(list(annotations1, annotations2))
}
\dontrun{
# Create sample annotation sets
annotations1 <- data.frame(
  start = c(1, 10),
  end = c(5, 15),
  code = c("code1", "code2")
)
annotations2 <- data.frame(
  start = c(2, 12),
  end = c(6, 18),
  code = c("code1", "code3")
)
results <- generate_comparison_analysis(list(annotations1, annotations2))
}
}
\keyword{internal}
