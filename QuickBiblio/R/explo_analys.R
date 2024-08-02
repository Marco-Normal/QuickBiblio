#' Explorative Analysis
#'
#' @param M A bibliometric database
#' @param db_params A list of parameters for the database
#' @param analysis_params A list of parameters for the analysis
#'
#' @description
#' This function performs an explorative analysis of the bibliometric database
#' provided. It includes the following analyses:
#' \itemize{
#' \item Summary of the data
#' \item Most cited articles and authors
#' \item Local citation analysis
#' \item Authors dominance
#' \item Authors h-index
#'}
#'
#'
#' @return Does not return anything, but writes the results of the analysis to
#'         the output folder
#' @export
#'
#' @examples
#' explorative_analysis(M, db_params, analysis_params)
#'
explorative_analysis <- function(M, db_params, analysis_params) {
  ## Explorative analysis
  ## Summary of the data

  results <- biblioAnalysis(M, sep = db_params$delim)
  results_sum <- summary(results, k = analysis_params$n_max, pause = FALSE)
  sink(file = file.path(db_params$out_path_summaries, "summary.txt"))
  print(results_sum)
  sink()

  ## Tables

  ## Citation Analysis
  most_cited_art <- citations(M, sep = db_params$delim, field = "article")
  most_cited_auth <- citations(M, sep = db_params$delim, field = "author")

  most_cited_art <- most_cited_art$Cited[1:analysis_params$n_max]
  most_cited_auth <- most_cited_auth$Cited[1:analysis_params$n_max]

  write.csv(most_cited_art, file = file.path(db_params$out_path_summaries, "most_cited_articles.csv"))
  write.csv(most_cited_auth, file = file.path(db_params$out_path_summaries, "most_cited_authors.csv"))

  ## Local Citation Analysis

  local_cit <- localCitations(M, sep = db_params$delim)
  local_cit_auth <- local_cit$Authors[1:analysis_params$n_max, ]
  local_cit_art <- local_cit$Papers[1:analysis_params$n_max, ]

  write.csv(local_cit_auth, file = file.path(db_params$out_path_summaries, "local_cit_authors.csv"))
  write.csv(local_cit_art, file = file.path(db_params$out_path_summaries, "local_cit_articles.csv"))

  ## Authors Dominance

  authors_dom <- dominance(results, k = analysis_params$n_max)
  write.csv(authors_dom, file = file.path(db_params$out_path_summaries, "authors_dominance.csv"))

  ## Authors h-index

  authors <- gsub(",", " ", names(results$Authors)[1:analysis_params$n_max])
  authors_hindex <- Hindex(
    M,
    field = "author",
    elements = authors,
    sep = db_params$delim,
    years = analysis_params$years
  )
  write.csv(authors_hindex$H, file = file.path(db_params$out_path_summaries, "authors_hindex.csv"))

}
