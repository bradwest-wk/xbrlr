#' Plot Taxonomy Tree Graph
#'
#' Saves a png image of a network tree (from an igraph object) at a user
#' specified size with the given plot tile.
#'
#' @param edgelist A csv consisting of two columns (parent and children) of
#'   relationships between elements
#' @param usage_file An optional csv of usage counts for the elements with two
#'   columns, the element and the count for the given time period
#' @param out_file The output file for the png image
#' @param roots An optional vector of element names that should act as roots.
#'   Their label text size will be increased relative to other elements
#' @param out_width the size of the output width in pixels
#' @param out_height output height in pixels
#' @param title title of the plot
#' @return A png image of the graph
adv_tree_plot <- function(edgelist,
                          usage_file,
                          out_file,
                          roots,
                          out_width = 5760,
                          out_height = 5760,
                          title){
    # paste0("Preliminary Model: ", Sys.Date())

  # read edgelist
  if (endsWith(edgelist, ".csv")){
      edges <- readr::read_csv(file = edgelist, col_names = TRUE)
  } else if (endsWith(edgelist, ".Rdata")){
      edges <- load(edgelist)
  } else {
      stop("Please enter either csv or Rdata file")
  }
  # create graph
  g <- igraph::graph_from_edgelist(as.matrix(edges), directed = T)

  # find roots
  root <- which(igraph::degree(g, v = igraph::V(g), mode = "in")==0)

  # read usage
  usage <- readr::read_csv(usage_file, col_names = T)
  colnames(usage) <- c("element_name", "count")
  usage[is.na(usage$count),2] <- 0
  # cut count variable into 20 levels
  breaks <- quantile(usage$count, probs = seq(0,1,0.05))
  usage <- usage %>%
      dplyr::mutate(count_level = cut(usage$count, breaks = breaks,
                                      labels = F))
  # usage$count_level[nrow(usage)] <- 1

  # Set usage variable
  igraph::V(g)$usage_raw <- NA
  igraph::V(g)$usage_level <- NA
  for(i in seq(igraph::V(g))){
    if (igraph::V(g)$name[i] %in% usage$element_name){
      ind <- which(usage$element_name==igraph::V(g)$name[i])
      igraph::V(g)$usage_raw[i] <- usage$count[ind]
      igraph::V(g)$usage_level[i] <- usage$count_level[ind]
    }
  }

  # Color vertices based off of element usage
  igraph::V(g)$color <- NA
  for(i in 1:length(igraph::V(g)$color)){
    igraph::V(g)$color[i] <- heat.colors(20, alpha = 0.8)[21 - igraph::V(g)$
                                                              usage_level[i]]
  }
  # if there are any elements without usages
  igraph::V(g)$color[is.na(igraph::V(g)$color)] <- "white"

  # set Vertex and Edge color attributes
  darkgrey <- col2rgb("darkgrey")
  igraph::E(g)$color <- rgb(darkgrey[1,1], darkgrey[2,1],
                    darkgrey[3,1], alpha = 255, max = 255)
  framecolor <- rgb(darkgrey[1,1], darkgrey[2,1],
                    darkgrey[3,1], alpha = 75, max = 255)

  # Set vertex size based off of level in tree (for ease of viewing)
  igraph::V(g)$label.cex <- 1.25
  igraph::V(g)$label.cex[root[which(names(root) %in% roots)]] <- 2.5
  igraph::V(g)$label.cex[root[which(!(names(root) %in% roots))]] <- 1.75

  png(filename = out_file, width = out_width, height = out_height, res = 100)
  plot.new()
  # layout(matrix(c(1,2), nrow = 2), heights = c(5760, 200), widths = c(5760,
  # 1000))
  par(fig = c(0, 1, .01, 1), cex = 1, cex.main = 5,
      cex.sub = 4, srt = 270, mar = c(20,4,10,4),
      font.lab = 2, new = TRUE)
  igraph::plot.igraph(
      g,
      layout = igraph::layout_as_tree(
          g,
          root = root,
          rootlevel = if_else(names(root) %in% roots, 1, 2)
      ),
      vertex.label.cex = igraph::V(g)$label.cex,
      vertex.label = igraph::V(g)$name,
      vertex.color = igraph::V(g)$color,
      vertex.frame.color = framecolor,
      vertex.shape = "circle",
      vertex.size = 1.5,
      vertex.label.dist = .1,
      vertex.label.degree = pi / 2,
      vertex.label.color = "black",
      edge.arrow.size = .6,
      edge.arrow.width = 1,
      asp = 0,
      edge.curved = F
  )
  text(x = 1.05, y = 0, labels = title, srt = 270, font = 2, cex = 5)
  # Title for the heatmap legend
  text(
      x = .2,
      y = -1.2,
      labels = "Element Usage",
      srt = 0,
      font = 2,
      cex = 3,
      pos = 4
  )
  # Insert heatmap legend
  par(
      fig = c(0, 1, 0, .01),
      cex = 3,
      cex.main = 5,
      cex.sub = 4,
      srt = 270,
      par(mar = c(.5, 50, 0, 20)),
      font.lab = 2,
      new = TRUE
  )
  m <- matrix(1:20, 20, 1)
  image(z = m, col = rev(heat.colors(20, alpha = 0.7)), axes = F)
  axis(side = 3, at = seq(0, 1, 1/19),
       labels = format(breaks[-1], digits = 0), srt = 90)
  dev.off()
}
