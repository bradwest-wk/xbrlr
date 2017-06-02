# function for plotting revenue tree with usage data
# Colors vertices (elements) with heat colors according
# to usage.

# Inputs: edgelist (after data cleaning), other plotting params
# Output: png. file

adv_tree_plot <- function(edgelist, usage_file, roots, out_width = 5760, out_height = 5760,
                          out_name = "usage_plot.png", title = paste0("Preliminary Model: ", Sys.Date())){
  # Load igraph
  if (!require("igraph", character.only = T)){
    install.packages("igraph", dependencies = T)
    library(igraph)
  }
  # load tidyverse
  if (!require("tidyverse", character.only = T)){
    install.packages("tidyverse", dependencies = T)
    library(tidyverse)
  }
  
  # read edgelist
  edges <- read_csv(file = edgelist, col_names = TRUE)
  # create graph
  g <- graph_from_edgelist(as.matrix(edges), directed = T)
  
  # create a figures directory in the working directory
  # will be named ./figures
  if (!dir.exists(paste0("./", "figures"))){
    dir.create(paste0("./", "figures"))
  }

  # find roots
  root <- which(degree(g, v = V(g), mode = "in")==0)
  
  # read usage
  usage <- read_csv(usage_file, col_names = T)
  colnames(usage) <- c("element_name", "count")
  usage[is.na(usage$count),2] <- 0
  # cut count variable into 20 levels
  breaks <- quantile(usage$count, probs = seq(0,1,0.05))
  usage <- usage %>% mutate(count_level = cut(usage$count, breaks = breaks, labels = F))
  # usage$count_level[nrow(usage)] <- 1
  
  # Set usage variable
  V(g)$usage_raw <- NA
  V(g)$usage_level <- NA
  for(i in seq(V(g))){
    if (V(g)$name[i] %in% usage$element_name){
      ind <- which(usage$element_name==V(g)$name[i])
      V(g)$usage_raw[i] <- usage$count[ind]
      V(g)$usage_level[i] <- usage$count_level[ind]
    }
  }
  
  # Color vertices based off of element usage
  V(g)$color <- NA
  for(i in 1:length(V(g)$color)){
    V(g)$color[i] <- heat.colors(20, alpha = 0.8)[21 - V(g)$usage_level[i]]
  }
  # if there are any elements without usages 
  V(g)$color[is.na(V(g)$color)] <- "white"
  
  # set Vertex and Edge color attributes
  darkgrey <- col2rgb("darkgrey")
  E(g)$color <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 255, max = 255)
  framecolor <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 75, max = 255)
  
  # Set vertex size based off of level in tree (for ease of viewing)
  V(g)$label.cex <- 1.25
  V(g)$label.cex[root[which(names(root) %in% roots)]] <- 2.5
  V(g)$label.cex[root[which(!(names(root) %in% roots))]] <- 1.75
  
  png(filename = paste0("./", "figures", "/", out_name), width = out_width, height = out_height, res = 100)
  plot.new()
  # layout(matrix(c(1,2), nrow = 2), heights = c(5760, 200), widths = c(5760, 1000))
  par(fig = c(0, 1, .01, 1), cex = 1, cex.main = 5, cex.sub = 4, srt = 270, mar = c(20,4,10,4),
      font.lab = 2, new = TRUE)
  plot(g, layout = layout_as_tree(g, root = root, rootlevel = if_else(names(root) %in% roots, 1, 2)),
       vertex.label.cex = V(g)$label.cex, vertex.label = V(g)$name, vertex.color = V(g)$color,
       vertex.frame.color = framecolor, vertex.shape = "circle", vertex.size = 1.5,
       vertex.label.dist = .1, vertex.label.degree = pi/2, vertex.label.color = "black",
       edge.arrow.size = .6, edge.arrow.width = 1, asp = 0, edge.curved = F)
  text(x = 1.05, y = 0, labels = title, srt = 270, font = 2, cex = 5)
  # Title for the heatmap legend
  text(x = .2, y = -1.2, labels = "Element Usage", srt = 0, font = 2, cex = 3, pos = 4)
  # Insert heatmap legend
  par(fig = c(0, 1, 0, .01), cex = 3, cex.main = 5, cex.sub = 4, srt = 270, par(mar = c(.5,50,0,20)),
      font.lab = 2, new = TRUE)
  m <- matrix(1:20, 20, 1)
  image(z = m, col = rev(heat.colors(20, alpha = 0.7)), axes = F)
  axis(side = 3, at = seq(0, 1, 1/19),
       labels = format(breaks[-1], digits = 0), srt = 90)
  dev.off()
}
