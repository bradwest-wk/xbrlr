# ----------- Tree Structure Change Animation --------------- #

# This script animates the change in tree structure.
# To show changes in tree structure we need to create
# png images after each change. Start with the current
# edgelist of the taxonomy and systematically remove elements
# from it that are not a part of the revised taxonomy.  Then add
# in the NEW elements.  Use ffmpeg or animation to turn
# png's into a movie file.

library(tidyverse) # Data Wrangling
library(igraph) # Graph data structure

directory <- "/Users/bradwest/Google_Drive/Projects/taxonomy_rebuild/revenue_calc_vis"
setwd(directory)

current_rev_edges <- read_csv("./Revenues_edges.csv")
new_rev_edges <- read_csv("./Revenue_revised_170411.csv")

# Create graph for Current Taxonomy
g <- graph_from_edgelist(as.matrix(current_rev_edges), directed = T)
# Get root
root <- which(degree(g, v = V(g), mode = "in")==0)
# Calculate level
V(g)$level <- NA
for (i in seq(V(g))){
  # root is level 0
  V(g)$level[i] <- length(shortest_paths(g, from = root, to = i)$vpath[[1]]) - 1
}

# Create graph for New Taxonomy
g_new <- graph_from_edgelist(as.matrix(new_rev_edges), directed = T)
# Get root
root_new <- which(degree(g_new, v = V(g_new), mode = "in")==0)
# Calculate level
V(g_new)$level <- NA
for (i in seq(V(g_new))){
  # root is level 0
  V(g_new)$level[i] <- length(shortest_paths(g_new, from = root_new, to = i)$vpath[[1]]) - 1
}

# ---------------- Basic plotting function ------------------- #
plot_tree_animation <- function(graph, file_name = "./basic_plot.png", title = paste0("Preliminary Revenue Model: ", Sys.Date()), roots = "Revenues"){
  
  root <- which(degree(g, v = V(g), mode = "in")==0)
  
  # set Vertex and Edge color attributes
  darkgrey <- col2rgb("darkgrey")
  SkyBlue2 <- col2rgb("SkyBlue2")
  E(graph)$color <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 250, max = 255)
  V(graph)$color <- rgb(SkyBlue2[1,1], SkyBlue2[2,1], SkyBlue2[3,1], alpha = 200, max = 255)
  framecolor <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 75, max = 255)
  
  # Create PNG
  png(filename = file_name, width = 5760, height = 5760, res = 100)
  par(cex = 1, cex.main = 5, cex.sub = 4, srt = 270, mar = c(20,4,10,4), font.lab = 2)
  plot(graph, layout = layout_as_tree(graph, root = root, rootlevel = if_else(names(root) %in% roots, 1, 2)),
  vertex.label.cex = 2, vertex.color = V(graph)$color,
       vertex.frame.color = framecolor, vertex.shape = "circle", vertex.size = 2,
       vertex.label.dist = .1, vertex.label.degree = pi/2, vertex.label.color = "black",
       # vertex.label = V(graph)$name,
       vertex.label = NA,
       edge.arrow.size = .4, edge.arrow.width = 1, asp = 0, edge.curved = F)
  text(x = 1.05, y = 0, labels = title,
       srt = 270, font = 2, cex = 5)
  dev.off()
}

# ----------------- Create PNG Files of Intermediate Trees ---------------- #

# First delete multiple edges by deleting those with degree > 1
# Then delete nodes that are separated off, so that just the revenues
# tree remains.  Either do this by deleting all children from the
# nodes that are to be deleted (if degree(out) > 1)
# Then add nodes back in via above loops.

# order by level
old_tx <- cbind.data.frame(V(g)$name, V(g)$level)
colnames(old_tx) <- c("element", "level")
old_tx$element <- as.character(old_tx$element)
old_tx$level <- as.integer(old_tx$level)
old_tx <- old_tx[order(-old_tx$level),]

new_tx <- cbind.data.frame(V(g_new)$name, V(g_new)$level)
colnames(new_tx) <- c("element", "level")
new_tx$element <- as.character(new_tx$element)
new_tx$level <- as.integer(new_tx$level)
new_tx <- new_tx[order(new_tx$level),]

for (i in 1:nrow(old_tx)){
  out_name <- paste0("/Users/bradwest/Google_Drive/Projects/taxonomy_rebuild/revenue_calc_vis/figures_revenue_vis/animation_plots/rev_animation_", i, ".png")
  if (old_tx[i,1] %in% V(g)$name){
    if (degree(g, v = old_tx[i,1], mode = "in") > 1 | !(old_tx[i,1] %in% V(g_new)$name)){
      g <- delete_vertices(g, old_tx[i,1])
    }
  }
  if (length(which(degree(g, v = V(g), mode = "in")==0 & V(g)$name != "Revenues")) > 0){
    g <- delete_vertices(g, which(degree(g, v = V(g), mode = "in")==0 & V(g)$name != "Revenues"))
  }
  plot_tree_animation(graph = g, file_name = out_name, title = "")
}

# Plot New Elements
for (i in 2:nrow(new_tx)){
  out_name <- paste0("/Users/bradwest/Google_Drive/Projects/taxonomy_rebuild/revenue_calc_vis/figures_revenue_vis/animation_plots/rev_animation_", i+280-1, ".png")
  if (new_tx[i,1] %in% V(g)$name) {
    g <- delete_vertices(g, new_tx[i,1])
    g <- add_vertices(g, 1, name = new_tx[i,1])
    g <- add_edges(g, edges = c(which(V(g)$name == head_of(g_new, E(g_new)[[to(new_tx[i,1])]])$name), which(V(g)$name == new_tx[i,1])))
    # g <- add_edges(g, edges = c(which(V(g)$name == new_tx[i,1]), which(V(g)$name == tail_of(g_new, E(g_new)[[from(new_tx[i,1])]])$name)))
  } else {
    g <- add_vertices(g, 1, name = new_tx[i,1])
    g <- add_edges(g, edges = c(which(V(g)$name == head_of(g_new, E(g_new)[[to(new_tx[i,1])]])$name), which(V(g)$name == new_tx[i,1])))
  }
  if (length(which(degree(g, v = V(g), mode = "in")==0 & V(g)$name != "Revenues")) > 0){
    g <- delete_vertices(g, which(degree(g, v = V(g), mode = "in")==0 & V(g)$name != "Revenues"))
  }
  plot_tree_animation(graph = g, file_name = out_name, title = "")
}

# ---------------- Animate ------------------- #

# ---------------- PNG Files Try 2 ------------------ #

# We have two vectors of names that represent two separate vertex sets
# Need to go from vertex set 1 to vertex set 2 incrementally
# Problems with set 1 are the multiple edges, and multiple 0-degree edges
# (those other than Revenues). If we start with the set 1 tree then
# we can either increment up the tree, or down the tree, testing if the 
# element i is in set 2. Working up from the root is true to the 
# process of rebuilding the taxonomy, but may perhaps be harder.

# First, delete those edges that are not true to the structure (i.e. those
# that are not directed up the tree).

for(i in length(E(g)):1) {
  if (tail_of(g, E(g)[i])$level <= head_of(g, E(g)[i])$level) {
    g <- delete_edges(g, E(g)[i])
  }
}

# Get rid of improper edges for those with two edges in
# not sure if this is what I want. Not handling multiple
# edges well -- Yielding disconnected nodes
sum(degree(g, V(g), mode = "in") > 1)
degree2 <- which(degree(g, V(g), mode = "in") > 1)
for (i in rev(degree2)){
  edges <- which(tail_of(g, E(g))$name == V(g)[i]$name)
  for(j in edges){
    if (!(tail_of(g, E(g)[j])$name %in% V(g_new)$names | head_of(g, E(g)[j])$name %in% V(g_new)$names)){
      g <- delete_edges(g, E(g)[j])
    }
  } 
}

plot_tree_animation(g)

# Construct function for transfering nodes from existing to new, generating
# an image at each step.
V(g)$index <- 1:length(V(g)$name)
g2 <- g 
for(i in V(g2)$name){
  if (!(V(g)[i] %in% V(g_new))){
    # Delete that vertex if not in g_new
    g <- delete_vertices(g, V(g)[i])
  } else {
    # check to see if the outgoing edges are correct
    edges <- which(head_of(g_new, E(g_new))$name == V(g)[i]$name)
    for(j in edges){
      head <- head_of(g_new, j)$name
      tail <- tail_of(g_new, j)$name
      # if not, add the requisite vertex
      if(!(tail %in% V(g)$name)){
        g <- add_vertices(g, 1, attr = list(name = tail))
        g <- add_edges(g, c(V(g)[head], V(g)[tail]))
      }
    }
  }
  out_name <- paste0("/Users/bradwest/Google_Drive/Projects/taxonomy_rebuild/revenue_calc_vis/figures_revenue_vis/animation_plots/rev_animation_",
                     V(g2)[i]$index, ".png")
  plot_tree_animation(graph = g, file_name = out_name, title = "", roots = "Revenues")
}


# ------------ Toy dataset --------------- #

edge_old <- data.frame(parent = c("Revenues", "Revenues", "Revenues", "FinancialServicesRevenue", "FinancialServicesRevenue", "NetInvestmentIncome",
                                  "OtherIncome", "FeesAndCommissions", "GainsLossesOnSalesOfAssets"),
                       child = c("FinancialServicesRevenue", "NetInvestmentIncome",
                                 "OtherIncome", "FeesAndCommissions", "GainsLossesOnSalesOfAssets",
                                 "InterestIncomeOperating", "GainsLossesOnSalesOfAssets",  "ServicingFeesNet", "GainsLossesOnSaleOfDerivatives"
                                 ))

edge_new <- data.frame(parent = c("Revenues", "Revenues", "InsuranceCompaniesRevenueNet",  "InsuranceCompaniesRevenueNet",
                                  "NetInvestmentIncome", "PremiumsEarnedNet", "BankHoldingCompaniesRevenue", "BankHoldingCompaniesNonInterestIncome"),
                       child = c("BankHoldingCompaniesRevenue", "InsuranceCompaniesRevenueNet","NetInvestmentIncome",
                                 "PremiumsEarnedNet", "GrossInvestmentIncomeOperating", "InsuranceCommissionsAndFees",
                                 "BankHoldingCompaniesNonInterestIncome", "BankHoldingCompaniesFeesAndCommissions"
                                 ))

g_old <- graph_from_edgelist(as.matrix(edge_old))
g_new <- graph_from_edgelist(as.matrix(edge_new))

par(mfrow= c(1,2), srt = 0)
plot(g_old, layout = layout_as_tree(g_old), vertex.label.cex = 1)
plot(g_new, layout = layout_as_tree(g_new), vertex.label.cex = 1)

# start with revenues

# for the , 
    
    # if the same, break

    # otherwise check if g_old is in g_new
    
        # if it is, make sure it has the correct edges, starting at the base of it's subcomponent

        # if it's not, then delete it after placing it's subcomponents in the correct place

for (name in V(g_old)$name){
    print(name)
}


subcomponent(g_old, "FinancialServicesRevenue", mode = "out")
