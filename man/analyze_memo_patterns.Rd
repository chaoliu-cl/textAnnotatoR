% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R
\name{analyze_memo_patterns}
\alias{analyze_memo_patterns}
\title{Analyze memo usage patterns}
\usage{
analyze_memo_patterns(code_anns)
}
\arguments{
\item{code_anns}{Data frame containing code annotations with columns:
\itemize{
\item memo: character, memo text associated with annotation
\item code: character, code identifier
}}
}
\value{
List containing:
\itemize{
\item memo_frequency: Proportion of annotations with memos
\item has_memos: Logical vector indicating memo presence
}
}
\description{
Examines how memos are used with codes by analyzing memo frequency,
content, and patterns in memo application across code instances.
}
\keyword{internal}
