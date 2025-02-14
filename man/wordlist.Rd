% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_scr.R
\name{wordlist}
\alias{wordlist}
\title{Wordlist}
\usage{
wordlist(
  M,
  Field,
  n = n_max_network,
  measure = "identity",
  ngrams = 1,
  remove.terms = NULL,
  synonyms = NULL
)
}
\arguments{
\item{M}{A data frame with the data.}

\item{Field}{Field to be used.}

\item{n}{The number of words to be used.}

\item{measure}{The measure to be used.}

\item{ngrams}{The number of ngrams to be used.}

\item{remove.terms}{Terms to be removed.}

\item{synonyms}{Synonyms to be used.}
}
\value{
A list with the wordlist.
}
\description{
This function is used to create a wordlist to be used in the Word Cloud.
}
\note{
This function was copied from Bibliometrix package, available at:
https://cran.r-project.org/web/packages/bibliometrix/bibliometrix.pdf
}
