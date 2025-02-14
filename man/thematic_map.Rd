% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/other_analys.R
\name{thematic_map}
\alias{thematic_map}
\title{Thematic Map}
\usage{
thematic_map(M, field = "DE", db_params)
}
\arguments{
\item{M}{A data frame with the data}

\item{field}{What field to use for the thematic map}

\item{db_params}{Parameters for the database}
}
\value{
Does not return anything, but writes the results of the analysis to
      the output folder
}
\description{
It creates a thematic map based on the data frame M and the field specified
}
