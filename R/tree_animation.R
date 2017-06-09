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

directory <-
    "/Users/bradwest/Google_Drive/Projects/taxonomy_rebuild/revenue_calc_vis"
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


# Toy dataset =================================================================

#' Generate and Plot Toy dataset
generate_toy <- function(){
    edge_old <- data.frame(
        parent = c(
            "Revenues", "Revenues","Revenues", "FinancialServicesRevenue",
            "FinancialServicesRevenue",
            "FinancialServicesRevenue", "NetInvestmentIncome",
            "OtherIncome", "InterestIncomeOperating", "FeesAndCommissions",
            "GainsLossesOnSalesOfAssets"
        ),
        child = c(
            "FinancialServicesRevenue", "NetInvestmentIncome", "OtherIncome",
            "FeesAndCommissions", "GainsLossesOnSalesOfAssets",
            "BankHoldingCompaniesRevenue",
            "InterestIncomeOperating","GainsLossesOnSalesOfAssets",
            "BankHoldingCompaniesFeesAndCommissions", "ServicingFeesNet",
            "GainsLossesOnSaleOfDerivatives"
        )
    )

    edge_new <- data.frame(
        parent = c("Revenues", "Revenues", "InsuranceCompaniesRevenueNet",
                   "InsuranceCompaniesRevenueNet", "NetInvestmentIncome",
                   "NetInvestmentIncome",
                   "PremiumsEarnedNet", "BankHoldingCompaniesRevenue",
                   "BankHoldingCompaniesNonInterestIncome"
        ),
        child = c("BankHoldingCompaniesRevenue", "InsuranceCompaniesRevenueNet",
                  "NetInvestmentIncome","PremiumsEarnedNet",
                  "GrossInvestmentIncomeOperating", "InterestIncomeOperating",
                  "InsuranceCommissionsAndFees",
                  "BankHoldingCompaniesNonInterestIncome",
                  "BankHoldingCompaniesFeesAndCommissions"
        )
    )

    g_old <- igraph::graph_from_edgelist(as.matrix(edge_old))
    g_new <- igraph::graph_from_edgelist(as.matrix(edge_new))

    vertex_color_old <- RColorBrewer::brewer.pal(3, "Set2")[3]
    edge_color_old <- RColorBrewer::brewer.pal(3, "Set2")[1]
    vertex_color_new <- RColorBrewer::brewer.pal(5, "Set2")[4]
    edge_color_new <- RColorBrewer::brewer.pal(5, "Set2")[5]

    par(mfrow= c(1,2), srt = 0, cex = .5)
    igraph::plot.igraph(g_old, layout = igraph::layout_as_tree(g_old),
                        vertex.label.cex = 1, vertex.color = vertex_color_old,
                        edge.color = edge_color_old)
    igraph::plot.igraph(g_new, layout = igraph::layout_as_tree(g_new),
                        vertex.label.cex = 1, vertex.color = vertex_color_new,
                        edge.color = edge_color_new)
}

# Functions for Plotting ======================================================

# Potential methods:
# vertex_decision -- makes a decision whether to keep the current vertex or not.
# If it is in the new graph
# vertex_parents -- returns the parents of the node
# correct_parents -- Boolean if the parent is correct
# parent_in_current -- Boolean if parent is in current ## NOT NEEDED
# create_parent -- returns newly created parent vertex
# children_desired -- boolean if all children should be kept
# delete_vetex -- if not in current and doesn't have children
# need to recusively check the parents before attaching a degree 0 vertex to a
# parent

# topological sorting of an igraph object:
## NOT NEEDED
igraph::topo_sort(g_new, mode = "out")
igraph::topo_sort(g_old, mode = "out")

#' Vertex Parents
#'
#' Finds the parents of the current vertex.
#'
#' @param g an igraph graph
#' @param v an igraph vertex name
#' @return a list containing the parent vertices (igraph.vs object) and parent
#'   edges (igraph.es object)
vertex_parents <- function(g, v) {
    parent <- list(vertices = c(), edges = c())
    parent$vertices <- igraph::neighbors(g, v, mode = "in")
    parent$edges <- igraph::incident(g, v, mode = "in")
    parent
}


#' Is Parent Correct?
#'
#' Checks if the current vertex has the correct parents.
#'
#' @param g_old an igraph graph
#' @param v an igraph vertex name in both graphs g_old and g_new
#' @param g_new an igraph graph in which to check if the parent is correct
#' @return boolean; if the parent is correct: true, else: false
correct_parent <- function(g_old, g_new, v) {
    if (!(v %in% igraph::V(g_new)$name)) {
        return(FALSE)
        # stop("Vertex not found in new graph")
    } else if (length(vertex_parents(g_old, v)$vertices) > 1) {
        return(FALSE)
        # stop("More than one parent found in new graph")
    } else if (is.null(igraph::neighbors(g_old, v, mode = "in")$name) &
               is.null(igraph::neighbors(g_new, v, mode = "in")$name)) {
        return(TRUE)
    } else if (xor(is.null(igraph::neighbors(g_old, v, mode = "in")$name),
               is.null(igraph::neighbors(g_new, v, mode = "in")$name))) {
        return(FALSE)
    } else if ( length(igraph::neighbors(g_old, v, mode = "in")$name) !=
                length(igraph::neighbors(g_new, v, mode = "in")$name) ) {
        return(FALSE)
    } else if ( identical(igraph::neighbors(g_old, v, mode = "in")$name,
                         igraph::neighbors(g_new, v, mode = "in")$name) ) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}


#' Create a New Parent Vertex
#'
#' Creates a new vertex and places accordingly, checking if its parents are in
#' are in the graph.  Works recursively
#'
#' @param g_old an igraph graph, the working graph
#' @param g_new an igraph graph, the desired graph
#' @param v an igraph vertex name, the vertex whose parent must be created
#' @param filename The output filename
#' @return a new igraph graph with the correct parent in place
create_parent <- function(g_old, g_new, v, filename) {
    v_tmp <- v
    desired_parent <- igraph::neighbors(g_new, v, mode = "in")$name
    # delete current income edges to v
    g_old <-
        igraph::delete_edges(
            g_old, igraph::incident(g_old, v, mode = "in"
        ))
    g_old <-
        igraph::add_vertices(g_old, 1, attr = list(name = desired_parent
        ))
    added_vertex <- tail(igraph::V(g_old), n = 1)$name
    # added_vertex_name <- tail(igraph::V(g_old), n = 1)$name
    # added_vertex <- igraph::V(g_old)[igraph::V(g_old)$name ==
    #                                      desired_parent$name]
    # v_num <- which(igraph::V(g_old)$name == name)
    g_old <- igraph::add_edges(
        g_old, edges = c(
            added_vertex, name
            ))
    # plot_tree(g_old, filename) ####
        # igraph::add_edges(g_old, edges = c(
        #     which(igraph::V(g_old)$name == added_vertex$name),
        #     which(igraph::V(g_old)$name == v$name)
        # ))
    # v <- igraph::V(g_old)[igraph::V(g_old)$name == added_vertex_name]
    desired_parent <- igraph::neighbors(g_new, added_vertex, mode = "in")$name
    if ( !(desired_parent %in% igraph::V(g_old)$name) ) {
        g_old <- create_parent(g_old, g_new, added_vertex)
    } else {
        # desired_parent <- igraph::V(g_old)[
            # igraph::V(g_old)$name == desired_parent$name]
        g_old <-
            igraph::add_edges(
                g_old, edges = c(desired_parent, added_vertex)
            # igraph::add_edges(g_old, edges = c(desired_parent, v)
                # which(igraph::V(g_old)$name == desired_parent$name),
                # which(igraph::V(g_old)$name == added_vertex$name)
            )
        # plot_tree(g_old, filename) ####
    }
    g_old <- create_children(g_old, g_new, v)
    return(g_old)
}


#' Should Children be Kept?
#'
#' Determines if the children of a vertex are desired or not.
#'
#' @param g_old igraph graph, the working graph
#' @param g_new igraph graph, the desired graph
#' @param v an igraph vertex name whose children will be evaluated
#' @return boolean, True if at least one child is in the desired graph, False
#'   otherwise
children_desired <- function(g_old, g_new, v) {
    for (i in igraph::subcomponent(g_old, v, mode = "out")) {
        # print(igraph::V(g_old)[i]$name)
        if ( igraph::V(g_old)[i]$name %in% igraph::V(g_new)$name ) {
            return(TRUE)
        }
    }
    return(FALSE)
}


#' Recursively Check Parents
#'
#' Method for checking parent path all the way to root node.
#'
#' @param g_old an igraph graph, the working graph
#' @param g_new an igraph graph, the desired graph
#' @param v an igraph vertex name present in the old graph, the end node in the path
#' @return boolean; True if all parents are desired and in correct order, False
#'   otherwise
check_parent_path <- function(g_old, g_new, v) {
    # v_name <- v$name
    root_old <- igraph::V(g_old)[which(
        igraph::degree(g_old, v = igraph::V(g_old), mode = "in")==0)]
    root_new <- igraph::V(g_new)[which(
        igraph::degree(g_new, v = igraph::V(g_new), mode = "in")==0)]
    if ( root_old != root_new ) {
        return(FALSE)
    }
    old_path <- igraph::get.shortest.paths(g_old, root_old, v)$vpath[[1]]
    new_path <- igraph::get.shortest.paths(g_new, root_new, v)$vpath[[1]]
    if ( length(old_path) != length(new_path) )  {
        return(FALSE)
    } else if ( length(old_path)<=2 ) {
        return(TRUE)
    } else {
        for (i in 2:length(old_path)) {
            if (old_path[i]$name != new_path[i]$name) {
                return(FALSE)
            }
        }
    }
    return(TRUE)
}


#' Create New Child Vertex
#'
#' Creates a new child vertex, working recursively
#'
#' @param g_old an igraph graph, the working graph
#' @param g_new an igraph graph, the desired graph
#' @param v an igraph vertex name present in the working graph, whose children will
#'   be created
#' @param filename The output filename
#' @return an igraph graph, with the correct children nodes in place.
create_children <- function(g_old, g_new, v, filename) {
    # name = v$name
    desired_subgraph <- igraph::subcomponent(g_new, v, mode = "out")
    for (i in 2:length(desired_subgraph)) {
        v_name <- desired_subgraph[i]$name
        if ( v_name %in% igraph::V(g_old)$name ) {
            # possibly add a condition for if the degree of node is zero
            # or can just add to that current node
            next
        } else {
            g_old <-
                igraph::add_vertices(
                    g_old, 1, attr = list(name = v_name
                    ))
        }
        # the alternative to above:
        # this will add new nodes to existing nodes even if those existing do
        # not have the correct path to the root node.
        # if ( !(v_name %in% igraph::V(g_old)$name)) {
        #     g_old <-
        #         igraph::add_vertices(
        #             g_old, 1, attr = list(name = v_name
        #             ))
        # }

        desired_parent <- igraph::neighbors(g_new, v_name, mode = "in")[1]$name
        if ( !(desired_parent %in% igraph::V(g_old)$name)) {
            warning("No parent exists in working graph, cannot add edge")
            # plot_tree(g_old, filename) ####
        } else {
            g_old <- igraph::add_edges(
                        g_old, edges = c(desired_parent, v_name))
            # plot_tree(g_old, filename) ####
        }
    }
    return(g_old)
}


#' Retain or Delete Vertex
#'
#' Method for retaining or deleting a vertex given its presence in the desired
#' graph
#'
#' @param g_old an igraph graph, the working graph
#' @param g_new an igraph graph, the desired graph
#' @param v an igraph vertex name present in the "old" graph
#' @param file The output filename for the image
#' @return an igraph graph with the appropriate edges deleted
vertex_decision <- function(g_old, g_new, v, file) {
    # v <- igraph::V(g_old)[igraph::V(g_old)$name == v$name]
    # not desired
    if ( !(v %in% igraph::V(g_new)$name) ) {
        # desired children
        if ( children_desired(g_old, g_new, v) ) {
            for (i in igraph::subcomponent(g_old, v, mode = "out")) {
                g_old <- vertex_decision(g_old, g_new, igraph::V(g_old)[i]$name)
            }
        # un-desired children
        } else {
            g_old <- igraph::delete_vertices(
                g_old, igraph::subcomponent(g_old, v, mode = "out"
                ))
        }
    # desired node
    } else {
        # incorrect parent
        if ( !correct_parent(g_old, g_new, v) ) {
            for (i in 1:length(igraph::neighbors(g_old, v, mode = "in"))) {
                # delete incorrect incident edges
                    igraph::neighbors(g_new, v, mode = "in")[1]$name
                if ( igraph::neighbors(g_old, v, mode = "in")[i]$name !=
                         desired_parent ) {
                    g_old <-
                        igraph::delete_edges(
                            g_old, igraph::E(g_old, c(
                                igraph::neighbors(
                                    g_old, v, mode = "in")[i], v)
                                ))
                }
            }
        }
        # if the degree in is one, then that's the correct parent, although the
        # parent path could be wrong. Otherwise it's a lone node that needs a
        # parent created.
        # v <- igraph::V(g_old)[igraph::V(g_old)$name == v$name]
        if ( igraph::degree(g_old, v, mode = "in")==0 ) {
            # parent in current graph?
            parent <- igraph::neighbors(g_new, v, mode = "in")$name
            if (length(parent) > 0) {
                if ( parent %in% igraph::V(g_old)$name ) {
                    # Check recursively here if parents are good
                    ##
                    # parent_vertex <-
                        # igraph::neighbors(g_new, v, mode = "in")$name
                    # parent_vertex <- igraph::V(g_old)[
                        # igraph::V(g_old)$name == parent_name]
                    g_old <- igraph::add_edges(
                        g_old, edges = c(
                            parent, v))
                    # plot_tree(g_old, file) ####
                    if ( !check_parent_path(g_old, g_new, v) ) {
                        root_old <- igraph::V(g_old)[which(
                            igraph::degree(
                                g_old, v = igraph::V(g_old), mode = "in")==0)]
                        old_path <- igraph::get.shortest.paths(
                            g_old, root_old, v)$vpath[[1]]
                        # should I reverse?
                        for (i in 2:length(old_path)) {
                            v_name <- rev(old_path)[i]$name
                            # vertex <- igraph::V(g_old)[
                            #     igraph::V(g_old)$name == v_name]
                            g_old <- vertex_decision(g_old, g_new, v_name)
                        }
                    }
                } else {
                    g_old <- create_parent(g_old, g_new, v, file)
                    if ( !check_parent_path(g_old, g_new, v) ) {
                        # name <- v$name
                        root_old <- igraph::V(g_old)[which(
                            igraph::degree(
                                g_old, v = igraph::V(g_old), mode = "in")==0)]
                        old_path <- igraph::get.shortest.paths(
                            g_old, root_old, v)$vpath[[1]]
                        # should I reverse?
                        for (i in 2:length(old_path)) {
                            v_name <- rev(old_path)[i]$name
                            g_old <- vertex_decision(g_old, g_new, v_name)
                        }
                    }
                }
            }
        }
    }
    # correct parent we now have
    return(g_old)
}


#' Plot Tree
#'
#' Need to create the equivalent of a static variable so that whenever this
#' function is run, it increments the counter by one.  That way filenames
#' will be sequential
#'
#' Function for plotting tree with certain color and plotting parameters
#'
#'  @param g The graph to plot.
#'  @param outname Name of the output file.
plot_tree <- function(g, outname) {
    vertex_color_old <- RColorBrewer::brewer.pal(3, "Set2")[3]
    edge_color_old <- RColorBrewer::brewer.pal(3, "Set2")[1]
    # vertex_color_new <- RColorBrewer::brewer.pal(5, "Set2")[4]
    # edge_color_new <- RColorBrewer::brewer.pal(5, "Set2")[5]

    # png(filename = filename, width = 5760, height = 5760, res = 100)
    # par(cex = 1, cex.main = 5, cex.sub = 4, srt = 270, mar = c(20,4,10,4),
    #     font.lab = 2)
    png(filename = outname)
    par(mfrow= c(1,2), srt = 0, cex = .5)
    igraph::plot.igraph(g, layout = igraph::layout_as_tree(g),
                        vertex.label.cex = 1, vertex.color = vertex_color_old,
                        edge.color = edge_color_old)
    dev.off()
}

igraph::plot.igraph(g_old, layout = igraph::layout_as_tree(g_old))
igraph::plot.igraph(g_new, layout = igraph::layout_as_tree(g_new),
                    vertex.label.cex = 1, vertex.color = vertex_color_new,
                    edge.color = edge_color_new)

# file <-

# delete vertex:
# delete_vertices(graph, v) (where v is a vertex sequence)

# v <- igraph::V(g_old)[igraph::V(g_old)$name == "InsuranceCompaniesRevenueNet"]
# v <- igraph::V(g_old)[igraph::V(g_old)$name == "NetInvestmentIncome"]
# v <- igraph::V(g_old)[igraph::V(g_old)$name == "FinancialServicesRevenue"]
# v <- igraph::V(g_old)[igraph::V(g_old)$name == "BankHoldingCompaniesRevenue"]
# v <- igraph::V(g_old)[igraph::V(g_old)$name == "Revenues"]
# v <- igraph::V(g_old)[igraph::V(g_old)$name == "BankHoldingCompaniesFeesAndCommissions"]
# igraph::plot.igraph(g_test, layout = igraph::layout_as_tree(g_test))

# igraph::incident(g_old, "InterestIncomeOperating", mode = "in") %in% igraph::E(g_new)
