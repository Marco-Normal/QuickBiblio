#' Plot_network
#'
#' @param M A data frame with the data
#' @param analysis The type of analysis that will be performed
#' @param network The type of network that will be plotted
#' @param db_params Parameters for the database
#' @param default_params_network Parameters for the network
#'
#' @description
#' This function creates a network plot for the specified analysis and network
#' type. It also saves the plot as a PNG and a html file.
#'
#' @return Does not return anything, but saves a PNG and a html, if specified
#' to the output folder
#' @export
#'

plot_network <- function(M, analysis, network, db_params = db_params,
                         default_params_network = default_params_network){
  switch (network,
          "countries" = {
            M <- metaTagExtraction(M, Field = "AU_CO", sep = db_params$delim)
          },
          "sources" = {
            M <- metaTagExtraction(M, Field = "CR_SO", sep = db_params$delim)
          },
          "authors" = {
            M <- metaTagExtraction(M, Field = "CR_AU", sep = db_params$delim)
          },

          {M <- M}

  )
  net <- biblioNetwork(
    M,
    analysis = analysis,
    network = network
  )
  net_plt <- do.call(networkPlot, c(
    list(NetMatrix = net, Title = paste(analysis, network, "Network", sep = "")),
    default_params_network
  ))
  plot_net(net_plt,
               gsub(" ", "",paste("net_",analysis,"_",network,".html")), db_params)
}



#' Collaborative Networks
#'
#' @param M A data frame with the data
#' @param db_params A list of parameters for the database
#' @param default_params_network A list of parameters for the network
#'
#' @description
#' This function creates collaborative networks for the authors, universities,
#' and countries fields.
#'
#'
#' @return Does not return anything, but writes the results of the analysis to
#'        the output folder
#' @export
#'

collab_networks <- function(M, db_params,
                            default_params_network){
  analysis <- "collaboration"
  networks <- c("authors", "universities", "countries")
  for (network in networks){
    plot_network(M, analysis, network, db_params, default_params_network)
  }
}
#' Cocitation Networks
#'
#' @param M A data frame with the data
#' @param db_params A list of parameters for the database
#' @param default_params_network A list of parameters for the network
#'
#' @description
#'  This function creates co-citation networks for the references, sources, and
#'  authors fields.
#'
#' @return Does not return anything, but writes the results of the analysis to
#'        the output folder
#' @export
#'

cocit_networks <- function(M, db_params,
                           default_params_network){
  ### Co-citation Network
  #### References
  analysis <- "co-citation"
  networks <- c("references", "sources", "authors")
  for (network in networks){
    plot_network(M, analysis, network, db_params, default_params_network)
  }

}
#' Coupling Networks
#'
#' @param M A data frame with the data
#' @param db_params A list of parameters for the database
#' @param default_params_network A list of parameters for the network
#'
#' @description
#' This function creates coupling networks for the authors, references, sources,
#' and countries fields.
#'
#'
#' @return Does not return anything, but writes the results of the analysis to
#'        the output folder
#' @export
#'

coupling_networks <- function(M, db_params,
                              default_params_network){
  ### Coupling Network
  #### Authors
  analysis <- "coupling"
  networks <- c("authors", "references", "sources", "countries")


  for (network in networks){
    plot_network(M, analysis, network, db_params, default_params_network)
  }
}
#' Co-Ocurrence Networks
#'
#' @param M A data frame with the data
#' @param db_params A list of parameters for the database
#' @param default_params_network A list of parameters for the network
#'
#' @description
#' This function creates co-occurrence networks for the authors, keywords, and
#' author-keywords fields. It also tries to create networks for the titles,
#' abstracts, and sources fields, but it does not work.
#'
#'
#' @return Does not return anything, but writes the results of the analysis to
#'        the output folder
#' @export
#'

coocurrence_networks <- function(M, db_params,
                                 default_params_network){
  ### Co-occurrence Network
  #### Authors

  analysis <- "co-occurrences"
  networks <- c("authors", "keywords", "author_keywords")#, "titles", "abstracts", "sources")

  for (network in networks){
    plot_network(M, analysis, network, db_params, default_params_network)
  }

  ### titles
  ### Does not work
  # net_cooccurrence_titles <- do.call(biblioNetwork, c(
  #   list(
  #     M,
  #     analysis = "co-occurrences",
  #     network = "titles"
  #   ),
  #   default_params_network
  # ))
  # net_cooccurrence_titles <- do.call(networkPlot, c(
  #   list(NetMatrix = net_cooccurrence_titles, Title = "Titles Co-Occurrence
  # Network"),
  #   default_params_graph
  # ))
  # plot_net(
  #   net_cooccurrence_titles,
  #   file.path(out_path_figs_web, "net_coocorre_titles.html")
  # )


  ### abstracts
  ### Does not work
  # net_cooccurrence_abstracts <- do.call(biblioNetwork, c(
  #     list(
  #       M,
  #       analysis = "co-occurrences",
  #       network = "abstracts"
  #     ),
  #     default_params_network
  #   ))
  # net_cooccurrence_abstracts <- do.call(networkPlot, c(
  #   list(NetMatrix = net_cooccurrence_abstracts, Title =
  #          "Abstracts Co-Occurrence Network"),
  #   default_params_graph
  # ))
  # plot_net(
  #   net_cooccurrence_abstracts,
  #   file.path(out_path_figs_web, "net_coocorre_abstracts.html")
  # )

  ### sources
  #### Does not work
  # net_cooccurrence_sources <- do.call(biblioNetwork, c(
  #   list(M, analysis = "co-occurrences", network = "sources"),
  #   default_params_network
  # ))
  #
  # net_cooccurrence_sources <- do.call(networkPlot, c(
  #   list(NetMatrix =
  #          net_cooccurrence_sources, Title = "Sources Co-Occurrence Network"),
  #   default_params_graph
  # ))
  # plot_net(
  #   net_cooccurrence_sources,
  #   file.path(out_path_figs_web, "net_coocorre_sources.html")
  # )

}

