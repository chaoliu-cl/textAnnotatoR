% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/annotate_gui.R
\name{generate_text_summary}
\alias{generate_text_summary}
\title{Generate text summary statistics}
\usage{
generate_text_summary(text, annotations)
}
\arguments{
\item{text}{Character string containing the text being analyzed}

\item{annotations}{Data frame of annotations with columns:
\itemize{
\item start: numeric, starting position of annotation
\item end: numeric, ending position of annotation
\item code: character, code applied to the annotation
}}
}
\value{
A list containing summary statistics:
\itemize{
\item total_words: total number of words in the text
\item total_characters: total number of characters
\item total_sentences: number of sentences (approximated by punctuation)
\item total_paragraphs: number of paragraphs (non-empty lines)
\item total_annotations: number of annotations
\item unique_codes: number of unique codes used
}
}
\description{
Calculates basic summary statistics for the annotated text, including word counts,
character counts, annotation counts, and unique code counts.
}
\keyword{internal}
