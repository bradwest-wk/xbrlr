# visualize entire 2017 calculation link taxonomy

library(xbrlr)
library(tidyverse)
library(igraph)

# load("./data/calc_link_amended_tx.Rdata")
# head(calc_link_amended_tx)
load("./data/calc_link_dirty.Rdata")
# head(calc_link_dirty)
# get just the statement names
stmt_names <- get_stmt_names(calc_link_dirty, "Calculation")
clink <- find_statement(stmt_names[1], calc_link_dirty, "Calculation")
for (stmt in stmt_names[2:length(stmt_names)]) {
    stmt_of_interest <- find_statement(stmt, calc_link_dirty, "Calculation")
    clink <- bind_rows(clink, stmt_of_interest)
}
# remove us-gaap prefix
# clink <- clink %>%
#     rename(child = name) %>%
#     select(parent, child, label, depth, weight)

# =============================================================================
# SECTION UNNECESSARY
# graph edges
edges <- as.matrix(cbind(clink$parent, clink$child))
# need to get rid of all the NA's
sum(is.na(edges[,1]))
sum(is.na(edges[,2]))
# edges[is.na(edges[,2]),]
edges <- edges[!is.na(edges[,2]),]
edges <- edges[!is.na(edges[,1]),]

# create graph
g <- graph_from_edgelist(edges, directed = TRUE)
nrow(edges)
length(V(g))

# 427 roots
sum(igraph::degree(g, v = igraph::V(g), mode = "in")==0)
# =============================================================================

# xbrlr function
g2 <- create_graph(clink)
length(V(g2))
sum(igraph::degree(g2, v = igraph::V(g2), mode = "in")==0)
# test if the same, yes
# identical(V(g2)$name, V(g)$name)

# 5886 nodes in this graph, from an edgelist that is 6516 edges. Meaning 630
# edges are incident on nodes that already have edges (i.e. multiple edge
# issues).
# 427 roots

# plot

plot_graph_local <- function(g, filename, title) {
    darkgrey <- col2rgb("darkgrey")
    framecolor <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 75,
                      max = 255)
    roots <- which(igraph::degree(g, v = igraph::V(g), mode = "in")==0)
    png(filename = filename, width = 5760, height = 5760, res = 100)
    par(cex = 1, cex.main = 5, cex.sub = 4, srt = 270, mar = c(20,4,10,4),
        font.lab = 2, adj = 0)
    igraph::plot.igraph(g, layout = igraph::layout_with_drl(
        g),
        vertex.label.cex = igraph::V(g)$label.cex,
        vertex.label = NA, # igraph::V(g)$name,
        vertex.color = igraph::V(g)$color,
        vertex.frame.color = framecolor, vertex.shape = "circle",
        vertex.size = 0.4,
        vertex.label.dist = .1, vertex.label.degree = pi/2,
        vertex.label.color = "black",
        edge.arrow.size = .4, edge.arrow.width = 1, asp = 0, edge.curved = F,
        edge.color = igraph::E(g)$color)
    # text(x = 1.05, y = 0, labels = title,
    #      srt = 270, font = 2, cex = 5)
    # legend(x = "topleft", c("Addition", "Subtraction"), lty = 1, lwd = 2, col
    # = c(brewer.pal(n = 3, name = "Set2")[1], brewer.pal(n = 3, name =
    # "Set2")[2]), cex = 5)
    dev.off()
}

plot_graph_local(g2, "./inst/vis/whole_tx_drl.png",
                 "2017 Calculation Link Taxonomy: Test")

# =============================================================================
# lets look at the comprehensive statements:
load("./data/calc_link_dirty.Rdata")
stmts <- get_stmt_names(calc_link_dirty,"Statement")
stmts <- stmts[c(1,2,5,7,8,9,11)]
clink <- find_statement(stmts[1], calc_link_dirty, "Calculation")
for (stmt in stmts[2:length(stmts)]) {
    stmt_of_interest <- find_statement(stmt, calc_link_dirty, "Calculation")
    clink <- bind_rows(clink, stmt_of_interest)
}
g2 <- create_graph(clink)

multiple_col <- brewer.pal(5, "Set2")[4]
for (i in length(V(g2))) {
    if (i %% 2 == 0 {
        V(g2)[i]$color <- multiple_col
    })
    # if (degree(g2, V(g2)[i], mode = "out") > 1 |
    #     (degree(g2, V(g2)[i], mode = "in") > 1)) {
    #     V(g2)[i]$color <- multiple_col
    # }
}

# This is what you need to do:

plot_graph_local <- function(g, filename, title) {
    darkgrey <- col2rgb("darkgrey")
    framecolor <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 75,
                      max = 255)
    roots <- which(igraph::degree(g, v = igraph::V(g), mode = "in")==0)
    png(filename = filename, width = 5760, height = 5760, res = 100)
    par(cex = 1, cex.main = 5, cex.sub = 4, srt = 270, mar = c(20,4,10,4),
        font.lab = 2, adj = 0)
    igraph::plot.igraph(g, layout = igraph::layout_as_tree(g, root = roots, circular = T),
        vertex.label.cex = igraph::V(g)$label.cex,
        # vertex.label = NA,  igraph::V(g)$name,
        vertex.color = igraph::V(g)$color,
        vertex.frame.color = framecolor, vertex.shape = "circle",
        vertex.size = 0.4,
        vertex.label.dist = .1, vertex.label.degree = pi/2,
        vertex.label.color = "black",
        edge.arrow.size = .4, edge.arrow.width = 1, asp = 0, edge.curved = F,
        edge.color = igraph::E(g)$color)
    # text(x = 1.05, y = 0, labels = title,
    #      srt = 270, font = 2, cex = 5)
    # legend(x = "topleft", c("Addition", "Subtraction"), lty = 1, lwd = 2, col
    # = c(brewer.pal(n = 3, name = "Set2")[1], brewer.pal(n = 3, name =
    # "Set2")[2]), cex = 5)
    dev.off()
}

plot_graph_local(g2, "/tmp/tc.png",
                 "2017 Calculation Link Taxonomy: Test")
