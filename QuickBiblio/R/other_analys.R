#' Thematic Map
#'
#' @param M A data frame with the data
#' @param field What field to use for the thematic map
#' @param db_params Parameters for the database
#'
#' @description
#' It creates a thematic map based on the data frame M and the field specified
#'
#'
#'
#' @return Does not return anything, but writes the results of the analysis to
#'       the output folder
#' @export
#'
#'
thematic_map <- function(M, field = "DE", db_params) {
  map <- thematicMap(M, field = field, n = 200)
  plot_net(map$net, "thematic_map.html",
           db_params)
}

#' Word Cloud
#'
#' @param M A data frame with the data
#' @param field What field to use for the word cloud
#' @param n Number of words to include in the word cloud
#' @param db_params Parameters for the database
#'
#' @description
#' It creates a word cloud based on the data frame M and the field specified
#'
#'
#' @return Does not return anything, but writes the results of the analysis to
#'      the output folder
#' @export
#'
#'
word_cloud <- function(M, field = "DE", n = 100, db_params){
  ### Words Cloud
  #### Perguntar depois por que da diferença
  resW <- wordlist(M, Field = field, n = n, ngrams = 2)
  nuvem <- wordcloud2(resW$W, size = 0.5, shape = "circle", backgroundColor = "white",
                      fontFamily = "Arial", color = "random-dark",
                      rotateRatio = 0, minSize = 0.5, gridSize = 10, fontWeight = "bold")
  tmp <- tempfile(fileext = ".html")
  saveWidget(nuvem, tmp, selfcontained = TRUE)
  webshot(
    tmp,
    vwidth = 1000,
    vheight = 800,
    file = file.path(db_params$out_path_figs, "wordcloud.png"),
    delay = 0.5,
  )
}
