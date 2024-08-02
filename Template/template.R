# Need to install the package first

library("bibliometrix")
library("broom")
library("ggplot2")
library("Matrix")
library("igraph")
library("dplyr")
library("visNetwork")
library("webshot2")
library("wordcloud2")
library("utils")
library("QuickBiblio")
## Set the parameters for the database
db_params <- c()
db_params$raw_data_path <- "your_path_to_the_biblio.bib"
db_params$out_path <- "your_path_to_the_output_folder"
db_params$origin <- "wos"
db_params$ext <- ".bib"
db_params$delim <- ";"
db_params$format <- "bibtex"
db_params$html_file <- TRUE
M <- biblio_ds(db_params)


## Set the parameters for the analysis
n_max_network <- 50
analysis_params <- list(
  n_max = 100,
  n_max_graph = 50,
)

default_params_network <- list(
  cluster = "walktrap",
  n = 50,
  community.repulsion = 0.1,
  remove.isolates = FALSE,
  edges.min = 1,
  verbose = FALSE,
  remove.multiple = FALSE,
  label.cex = TRUE,
  size = 3,
  size.cex = FALSE,
  curved = TRUE,
  alpha = 0.75,
  edgesize = 1
)
explorative_analysis(M, db_params, analysis_params)
summary_graphs(M, db_params, default_params_network)
collab_networks(M, db_params, default_params_network)
cocit_networks(M, db_params, default_params_network)
coocurrence_networks(M, db_params, default_params_network)
coupling_networks(M, db_params, default_params_network)
word_cloud(M, field = "DE", n = 50, db_params)
thematic_map(M, field = "DE", db_params)
