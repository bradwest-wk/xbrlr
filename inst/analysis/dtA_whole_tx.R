# visualize entire 2017 calculation link taxonomy

library(xbrlr)
library(tidyverse)
library(igraph)
library(stringr)
library(RColorBrewer)

# =============================================================================
# Set-up: Load data

#' Load Whole Taxonomy
#'
#' Loads the entire taxonomy from a dirty Rdata file. Dirty in the sense that
#' it contains statement names, linkRoles, and prefix headers.
#' @param taxonomy the dirty taxonomy (a dataframe)
#' @param link Calculation or Presentation link c("Calculation", "Presentation")
#' @param sheet_type A string, one of the following:
#' c("Statement", "Disclosure", "both"), specifying whether just statements,
#' just disclosures, or both should be loaded
#' @return A clean dataframe
taxonomy_load <- function(taxonomy, link = "Calculation", sheet_type = "both") {
    if (sheet_type == "both") {
        tmp_stmt <- get_stmt_names(taxonomy, type = "Statement")
        tmp_disclosure <- get_stmt_names(taxonomy, type = "Disclosure")
        stmt_names <- c(tmp_stmt, tmp_disclosure)
    } else {
        stmt_names <- get_stmt_names(taxonomy, type = sheet_type)
    }
    out_df <- find_statement(stmt_names[1], taxonomy, link)
    for (stmt in stmt_names[2:length(stmt_names)]) {
        stmt_of_interest <- find_statement(stmt, taxonomy, link)
        out_df <- bind_rows(out_df, stmt_of_interest)
    }
    return(out_df)
}

# =============================================================================
# Add groups to graph.  A node can have multiple groups if it appears in more
# than one statement

#' Add Group Attribute
#'
#' Adds a group attribute to each graph vertex.  A vertex can be in one or more
#' groups (statements), depending on whether it is in one or more statements
#'
#' @param g The graph with verticies
#' @param dirty_tax The dirty taxonomy
#' @return The graph with the group attribute added
add_groups <- function(g, dirty_tax) {
    dirty_tax <- statement_column(dirty_tax)
    for (i in 1:gorder(g)) {
        V(g)$group[i] <-
            paste(dirty_tax$statement[which(dirty_tax$name==V(g)$name[i])],
                  collapse = ", ")
    }
    return(g)
}


#' Helper: Get statement column in dirty taxonomy
#'
#' @param dirty_tax The dirty taxonomy to create the column in
statement_column <- function(dirty_tax) {
    dirty_tax$statement <- rep(NA, nrow(dirty_tax))
    for (i in 2:nrow(dirty_tax)) {
        if (dirty_tax$prefix[i]=="Definition") {
            curr_stmt <- str_replace(dirty_tax$name[i], ",", "")
        }
        if (dirty_tax$prefix[i]=="us-gaap") {
            dirty_tax$statement[i] <- curr_stmt
        }
    }
    return(dirty_tax)
}

#' Color Nodes based on grpup length
#'
#' @param g A graph with a group vertex attribute
#' @return A graph with an updated color and group attribute based on number of
#' groups
group_count <- function(g) {
    colors <- brewer.pal(11, "Spectral")
    for (i in 1:gorder(g)) {
        V(g)[i]$group_count <- length(str_split(V(g)[i]$group, ",")[[1]])
        if (V(g)[i]$group_count == 1) {
            V(g)[i]$color <- colors[11]
        } else if (V(g)[i]$group_count == 2) {
            V(g)[i]$color <- colors[10]
        } else if (V(g)[i]$group_count == 3) {
            V(g)[i]$color <- colors[9]
        } else {
            V(g)[i]$color <- colors[8]
        }
    }
    return(g)
}

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
statements_2017 <- taxonomy_load(calc_link_dirty, "Calculation", "Statement")
g <- create_graph(statements_2017)
g <- add_groups(g, calc_link_dirty)
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

