% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R, R/comparison.R
\name{process_comparison_file}
\alias{process_comparison_file}
\title{Process comparison file}
\usage{
process_comparison_file(filepath)

process_comparison_file(filepath)
}
\arguments{
\item{filepath}{Character string specifying the path to the comparison file}
}
\value{
Data frame containing processed annotations with columns:
\itemize{
\item start: numeric, starting position of annotation
\item end: numeric, ending position of annotation
\item code: character, code applied to the annotation
}

Data frame containing processed annotations with columns:
\itemize{
\item start: numeric, starting position of annotation
\item end: numeric, ending position of annotation
\item code: character, code applied to the annotation
}
}
\description{
Processes uploaded comparison files, handling different file formats (CSV, JSON)
and ensuring proper data structure and types for comparison analysis.

Processes uploaded comparison files, handling different file formats (CSV, JSON)
and ensuring proper data structure and types for comparison analysis.
}
\examples{
\dontrun{
# Process a CSV file
annotations_csv <- process_comparison_file("annotations.csv")

# Process a JSON file
annotations_json <- process_comparison_file("annotations.json")
}

\dontrun{
# Process a CSV file
annotations_csv <- process_comparison_file("annotations.csv")

# Process a JSON file
annotations_json <- process_comparison_file("annotations.json")
}

}
\keyword{internal}
