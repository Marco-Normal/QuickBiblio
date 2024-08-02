#' Summary Graphs
#'
#' @param M A data frame with the data
#' @param db_params Parameters for the database
#' @param analysis_params Parameters for the analysis
#'
#' @description
#' It creates a summary of the analysis based on the data frame M
#' and the parameters specified
#'
#' @return Does not return anything, but writes the results of the analysis to
#'       the output folder
#' @export
#'
summary_graphs <- function(M, db_params = db_params, analysis_params = analysis_params) {
  ## Graphs
  results <- biblioAnalysis(M, sep = db_params$delim)

  ## Graph of summary

  graph_summary <- plot(results, pause = FALSE)
  for (eco in names(graph_summary)) {
    ggsave(file = file.path(db_params$out_path_figs, paste0("summary_", eco, ".png")), graph_summary[[eco]],
           width = 7, height = 5, scale = 0.9)
  }

  ### Top Authors Productivity over Time
  top_authors <- authorProdOverTime(M, k = analysis_params$n_max_graph, graph = TRUE)
  ggsave(file = file.path(db_params$out_path_figs, "top_authors_prod.png"),
         top_authors$graph, width = 7, height = 5, scale = 0.9)


  ### Bradford Law

  bradford_law <- bradford(M)
  ggsave(file = file.path(db_params$out_path_figs, "bradford.png"), bradford_law$graph,
         width = 7, height = 5, scale = 0.9)

}
