#' Copyright (c) 2021, Bibliometrix
#' Network Layout
#'
#' @param type The type of layout to be used. Options are:
#' \itemize{
#' \item{"auto"}{Automatic layout}
#' \item{"circle"}{Circular layout}
#' \item{"mds"}{Multidimensional scaling layout}
#' \item{"star"}{Star layout}
#' \item{"sphere"}{Sphere layout}
#' \item{"fruchterman"}{Fruchterman layout}
#' \item{"kamada"}{Kamada layout}
#' }
#'
#' @description
#' This function is used to define the layout of the network.
#' The layout can be defined as automatic, circular, multidimensional scaling,
#' star, sphere, Fruchterman, or Kamada.
#' The function returns the layout to be used in the visNetwork function.
#'
#' @note
#' This function was copied from Bibliometrix package, available at:
#' https://cran.r-project.org/web/packages/bibliometrix/bibliometrix.pdf
#'
#' @return The layout to be used in the visNetwork function.
#' @export
#'
#'
netLayout <- function(type) {
  switch(
    type,
    auto = {
      l <- "layout_nicely"
    },
    circle = {
      l <- "layout_in_circle"
    },
    mds = {
      l <- "layout_with_mds"
    },
    star = {
      l <- "layout_as_star"
    },

    sphere = {
      l <- "layout_on_sphere"
    },
    fruchterman = {
      l <- "layout_with_fr"
    },
    kamada = {
      l <- "layout_with_kk"
    }
  )
  return(l)
}

#' Igraph To visNetwork
#'
#' @param g A igraph network
#' @param curved Logical. If TRUE, the edges are curved.
#'  If FALSE, the edges are straight.
#' @param labelsize The size of the labels.
#' @param opacity The opacity of the nodes.
#' @param type The type of layout to be used. Options are:
#' \itemize{
#' \item{"auto"}{Automatic layout}
#' \item{"circle"}{Circular layout}
#' \item{"mds"}{Multidimensional scaling layout}
#' \item{"star"}{Star layout}
#' \item{"sphere"}{Sphere layout}
#' \item{"fruchterman"}{Fruchterman layout}
#' \item{"kamada"}{Kamada layout}
#' }
#' @param shape The shape of the nodes.
#' @param net The network to plot.
#' @param shadow Logical. If TRUE, the nodes have shadow.
#' @param edgesize The size of the edges.
#' @param threshold The threshold to avoid label overlaps.
#'
#' @description
#' This function is used to convert a igraph network to a visNetwork network.
#' The function returns the visNetwork network.
#'
#' @note
#' This function was copied from Bibliometrix package, available at:
#' https://cran.r-project.org/web/packages/bibliometrix/bibliometrix.pdf
#'
#' @return The visNetwork network.
#' @export
#'
#'
igraph2vis <- function(g,
                       curved = FALSE,
                       labelsize = 3,
                       opacity = 1,
                       type = "auto",
                       shape = "dot",
                       net,
                       shadow = TRUE,
                       edgesize = 10,
                       threshold = 0.05) {
  LABEL = igraph::V(g)$name

  LABEL[igraph::V(g)$labelsize == 0] = ""

  vn <- visNetwork::toVisNetworkData(g)

  vn$nodes$label = LABEL
  vn$edges$num = 1
  vn$edges$dashes = FALSE
  vn$edges$dashes[vn$edges$lty == 2] = TRUE

  ## opacity
  vn$nodes$color = adjustcolor(vn$nodes$color, alpha.f = min(c(opacity, 1)))
  ## set a darkest gray for iter-cluster edges
  vn$edges$color <- paste(substr(vn$edges$color, 1, 7), "90", sep = "")
  vn$edges$color[substr(vn$edges$color, 1, 7) == "#B3B3B3"] <- "#69696960"
  vn$edges$color <- adjustcolor(vn$edges$color, alpha.f = opacity)

  ## removing multiple edges
  vn$edges <- unique(vn$edges)

  vn$edges$width <- vn$edges$width ^ 2 / (max(vn$edges$width ^ 2)) * (10 +
                                                                        edgesize)

  # if (edgesize==0){
  #   vn$edges$hidden <- TRUE
  #   }else{vn$edges$hidden <- FALSE}

  ## labelsize
  vn$nodes$font.size <- vn$nodes$deg
  scalemin <- 20
  scalemax <- 150
  Min <- min(vn$nodes$font.size)
  Max <- max(vn$nodes$font.size)
  if (Max > Min) {
    size = (vn$nodes$font.size - Min) / (Max - Min) * 15 * labelsize + 10
  } else {
    size = 10 * labelsize
  }
  size[size < scalemin] = scalemin
  size[size > scalemax] = scalemax
  vn$nodes$font.size = size
  l <- netLayout(type)

  ### TO ADD SHAPE AND FONT COLOR OPTIONS
  coords <- net$layout

  vn$nodes$size <- vn$nodes$font.size * 0.7

  #vn$nodes$font.color <- adjustcolor("black", alpha.f = min(c(opacity,1)))

  if (shape %in% c("dot", "square")) {
    vn$nodes$font.vadjust <- -0.7 * vn$nodes$font.size
  } else{
    vn$nodes$font.vadjust <- 0
  }

  opacity_font <- sqrt((vn$nodes$font.size - min(vn$nodes$font.size)) /
                         diff(range(vn$nodes$font.size))) * opacity + 0.3
  if (is.nan(opacity_font[1]))
    opacity_font <- rep(0.3, length(opacity_font))

  if (labelsize > 0) {
    vn$nodes$font.color <- unlist(lapply(opacity_font, function(x)
      adjustcolor("black", alpha.f = x)))
  } else{
    vn$nodes$font.color <- adjustcolor("black", alpha.f = 0)
  }

  ## avoid label overlaps
  threshold <- 0.02
  ymax <- diff(range(coords[, 2]))
  xmax <- diff(range(coords[, 1]))
  threshold2 <- threshold * mean(xmax, ymax)
  w <- data.frame(
    x = coords[, 1],
    y = coords[, 2],
    labelToPlot = vn$nodes$label,
    dotSize = size,
    row.names = vn$nodes$label
  )
  labelToRemove <- avoidNetOverlaps(w, threshold = threshold2)
  vn$nodes <- vn$nodes %>%
    mutate(label = ifelse(label %in% labelToRemove, "", label),
           title = id)
  ##

  VIS <-
    visNetwork::visNetwork(
      nodes = vn$nodes,
      edges = vn$edges,
      type = "full",
      smooth = TRUE,
      physics = FALSE,
      width = "1920px",
      height = "1080px"
    ) %>%
    visNetwork::visNodes(
      shadow = shadow,
      shape = shape,
      font = list(
        color = vn$nodes$font.color,
        size = vn$nodes$font.size,
        vadjust = vn$nodes$font.vadjust
      )
    ) %>%
    visNetwork::visIgraphLayout(layout = "layout.norm",
                                layoutMatrix = coords,
                                type = "full") %>%
    visNetwork::visEdges(smooth = list(type = "horizontal")) %>%
    visNetwork::visOptions(
      highlightNearest = list(
        enabled = T,
        hover = T,
        degree = 1
      ),
      nodesIdSelection = T
    ) %>%
    visNetwork::visInteraction(
      dragNodes = TRUE,
      navigationButtons = F,
      hideEdgesOnDrag = TRUE,
      zoomSpeed = 0.4
    ) %>%
    visNetwork::visOptions(manipulation = curved,
                           height = "100%",
                           width = "100%")

  return(list(
    VIS = VIS,
    vn = vn,
    type = type,
    l = l,
    curved = curved
  ))
}

#' Avoid Overlaps in the Network
#'
#' @param w  A data frame with the x and y coordinates of the nodes.
#' @param threshold The threshold to avoid label overlaps.
#'
#' @description
#'  This function is used to avoid label overlaps in the network.
#'  The function returns the labels to be removed.
#'
#' @note
#' This function was copied from Bibliometrix package, available at:
#' https://cran.r-project.org/web/packages/bibliometrix/bibliometrix.pdf
#'
#' @return The labels to be removed.
#' @export
#'
#'
avoidNetOverlaps <- function(w, threshold = 0.1) {
  w[, 2] <- w[, 2] / 2

  Ds <- dist(
    w %>%
      dplyr::filter(labelToPlot != "") %>%
      select(1:2),
    method = "manhattan",
    upper = T
  ) %>%
    dist2df() %>%
    rename(from = row, to = col, dist = value) %>%
    left_join(
      w %>% dplyr::filter(labelToPlot != "") %>%
        select(labelToPlot, dotSize),
      by = c("from" = "labelToPlot")
    ) %>%
    rename(w_from = dotSize) %>%
    left_join(
      w %>% dplyr::filter(labelToPlot != "") %>%
        select(labelToPlot, dotSize),
      by = c("to" = "labelToPlot")
    ) %>%
    rename(w_to = dotSize) %>%
    filter(dist < threshold)

  if (nrow(Ds) > 0) {
    st <- TRUE
    i <- 1
    label <- NULL
    case <- "n"

    while (isTRUE(st)) {
      if (Ds$w_from[i] > Ds$w_to[i] & Ds$dist[i] < threshold) {
        case <- "y"
        lab <- Ds$to[i]

      } else if (Ds$w_from[i] <= Ds$w_to[i] &
                 Ds$dist[i] < threshold) {
        case <- "y"
        lab <- Ds$from[i]
      }

      switch(case, "y" = {
        Ds <- Ds[Ds$from != lab, ]
        Ds <- Ds[Ds$to != lab, ]
        label <- c(label, lab)
      }, "n" = {
        Ds <- Ds[-1, ]
      })

      if (i >= nrow(Ds)) {
        st <- FALSE
      }
      case <- "n"
      #print(nrow(Ds))
    }
  } else {
    label = NULL
  }
  label

}

#' Table Tag
#'
#' @param inDist A matrix with the data.
#'
#' @description
#' This function is used to create a table with the data.
#'
#' @note
#' This function was copied from Bibliometrix package, available at:
#' https://cran.r-project.org/web/packages/bibliometrix/bibliometrix.pdf
#'
#' @return A table with the data.
#' @export
#'
#'
dist2df <- function(inDist) {
  if (class(inDist) != "dist")
    stop("wrong input type")
  A <- attr(inDist, "Size")
  B <- if (is.null(attr(inDist, "Labels")))
    sequence(A)
  else
    attr(inDist, "Labels")
  if (isTRUE(attr(inDist, "Diag")))
    attr(inDist, "Diag") <- FALSE
  if (isTRUE(attr(inDist, "Upper")))
    attr(inDist, "Upper") <- FALSE
  data.frame(row = B[unlist(lapply(sequence(A)[-1], function(x)
    x:A))],
    col = rep(B[-length(B)], (length(B) - 1):1),
    value = as.vector(inDist))
}

#' Plot and save the network
#'
#' @param net Network to plot.
#' @param filename Output filename.
#' @param db_params Parameters to save the file.
#'
#' @description
#' This function is used to plot and save the network. It uses webshot2
#' to take a picture of the network.
#'
#'
#'
#' @return Does not return anything, but saves the network as a PNG and a html file.
#'
#' @export
#'
#'
plot_net <- function(net, filename, db_params) {

  network1 <- igraph2vis(net$graph, net = net)
  if (db_params$html_file) {
    file <- file.path(db_params$out_path_figs_web, filename)
    visSave(network1$VIS, file = file, selfcontained = TRUE)
   }
  else {
  file <- file.path(tempdir(), "tmp.html")
  # comment the line above to not save the html file
  visSave(network1$VIS, file = file, selfcontained = TRUE)
  }
  webshot(
    file,
    file = file.path(db_params$out_path_figs,gsub(".html", ".png", filename)),
    #zoom = 1.75,
    vwidth = 1920,
    vheight = 1080,
    # cliprect = "viewport",
    delay = 0.15,
  )
}

#' Wordlist
#'
#' @param M A data frame with the data.
#' @param Field Field to be used.
#' @param n The number of words to be used.
#' @param measure The measure to be used.
#' @param ngrams The number of ngrams to be used.
#' @param remove.terms Terms to be removed.
#' @param synonyms Synonyms to be used.
#'
#' @description
#' This function is used to create a wordlist to be used in the Word Cloud.
#'
#' @note
#' This function was copied from Bibliometrix package, available at:
#' https://cran.r-project.org/web/packages/bibliometrix/bibliometrix.pdf
#'
#' @return A list with the wordlist.
#'
#' @export
#'
#'
wordlist <- function(M, Field, n = n_max_network, measure = "identity", ngrams = 1, remove.terms=NULL, synonyms=NULL){
  switch(Field,
         ID={v=tableTag(M,"ID", remove.terms  = remove.terms, synonyms = synonyms)},
         DE={v=tableTag(M,"DE", remove.terms = remove.terms, synonyms = synonyms)},
         TI={
           if (!("TI_TM" %in% names(M))){
             v=tableTag(M,"TI", ngrams=ngrams, remove.terms=remove.terms, synonyms = synonyms)

           }},
         AB={if (!("AB_TM" %in% names(M))){
           v=tableTag(M,"AB", ngrams=ngrams, remove.terms = remove.terms, synonyms = synonyms)
         }},
         WC={
           v=tableTag(M,"WC")
         }
  )
  names(v)=tolower(names(v))
  #v=tableTag(values$M,"ID")
  n=min(c(n,length(v)))
  Words=data.frame(Terms=names(v)[1:n], Frequency=(as.numeric(v)[1:n]), stringsAsFactors = FALSE)
  W=Words
  switch(measure,
         identity={},
         sqrt={W$Frequency=sqrt(W$Frequency)},
         log={W$Frequency=log(W$Frequency+1)},
         log10={W$Frequency=log10(W$Frequency+1)}
  )

  results=list(v=v,W=W, Words=Words)
  return(results)
}
