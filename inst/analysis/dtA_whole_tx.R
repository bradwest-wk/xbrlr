# visualize entire 2017 calculation link taxonomy

library(xbrlr)
library(tidyverse)
library(igraph)
library(stringr)
library(RColorBrewer)
library(visNetwork)

source("./inst/shiny/xbrlviz/whole_tx.R")

# =============================================================================

plot_graph_local <- function(g, filename, title) {
    darkgrey <- col2rgb("darkgrey")
    framecolor <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 75,
                      max = 255)
    roots <- which(igraph::degree(g, v = igraph::V(g), mode = "in")==0)
    png(filename = filename, width = 11520, height = 11520, res = 100)
    par(cex = 1, cex.main = 5, cex.sub = 4, srt = 270, mar = c(20,4,10,4),
        font.lab = 2, adj = 0)
    igraph::plot.igraph(
        g, layout = igraph::layout_with_drl(
            g, weights = NULL, options = drl_defaults$final),
        vertex.label.cex = igraph::V(g)$label.cex,
        vertex.label = NA, # igraph::V(g)$name,
        # vertex.color = igraph::V(g)$color,
        vertex.frame.color = framecolor, vertex.shape = "circle",
        vertex.size = 0.4,
        vertex.label.dist = .1, vertex.label.degree = pi/2,
        vertex.label.color = "black",
        edge.arrow.size = .4, edge.arrow.width = 1, asp = 0, edge.curved = F
        # edge.color = igraph::E(g)$color
        )
    text(x = 1.05, y = 0, labels = title, srt = 270, font = 2, cex = 5)
    # legend(x = "topleft", c("Addition", "Subtraction"), lty = 1, lwd = 2, col
    # = c(brewer.pal(n = 3, name = "Set2")[1], brewer.pal(n = 3, name =
    # "Set2")[2]), cex = 5)
    dev.off()
}


# =============================================================================
# Run it
load("./data/calc_link_dirty.Rdata")
calc_link_dirty <- calc_link_dirty[!is.na(calc_link_dirty$prefix), ]
statements_dirty <- get_desired_sheets(calc_link_dirty, "Statement")
statements_2017 <- taxonomy_load(statements_dirty, "Calculation", "Statement")
g <- create_graph(statements_2017)
g <- add_groups(g, statements_dirty)
g <- group_count(g)

# plot some temp graphs
plot_graph_local(g, "/tmp/vis_fr.png", "FR TEST") # NOPE. looks bad.
plot_graph_local(g, "/tmp/vis_kk.png", "KK TEST") # NOPE. can't run algo
plot_graph_local(g, "/tmp/vis_lgl.png", "LGL TEST") # NOPE. complete mess
plot_graph_local(g, "/tmp/vis_drl.png", "DRL TEST") # YES. weights = NULL must be set
plot_graph_local(g, "/tmp/vis_dh.png", "DH TEST") # NOPE. Too slow
plot_graph_local(g, "/tmp/vis_gem.png", "GEM TEST") # NOPE. Too slow
plot_graph_local(g, "/tmp/vis_graphopt.png", "GRAPHOPT TEST") # MAYBE??
plot_graph_local(g, "/tmp/vis_mds.png", "MDS TEST") # INTERESTING
plot_graph_local(g, "/tmp/vis_rt.png", "RT TEST") # Perhaps if you can turn off groups

# test drl layout with different parameter settings:
# drl_defaults$default drl_defaults$coarsen drl_defaults$coarsest drl_defaults$refine drl_defaults$final
plot_graph_local(g, "/tmp/vis_drl_coarsen.png", "DRL TEST (coarsen)")
plot_graph_local(g, "/tmp/vis_drl_coarsest.png", "DRL TEST (coarsest)")
plot_graph_local(g, "/tmp/vis_drl_refine.png", "DRL TEST (refine)")
plot_graph_local(g, "/tmp/vis_drl_final.png", "DRL TEST (final)")

# DRL, MDS, and Radial Tree seem like the best graphs to pursue

# =============================================================================

