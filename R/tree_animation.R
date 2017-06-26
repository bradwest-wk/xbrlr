# Functions for creating png images of tree plots =============================


# Toy dataset =================================================================

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
            "BankHoldingCompaniesNonInterestIncome", "ServicingFeesNet",
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

    # par(mfrow= c(1,2), srt = 0, cex = .75)
    # igraph::plot.igraph(g_old, layout = igraph::layout_as_tree(g_old),
    #                     vertex.label.cex = 1, vertex.color = vertex_color_old,
    #                     edge.color = edge_color_old)
    # igraph::plot.igraph(g_new, layout = igraph::layout_as_tree(g_new),
    #                     vertex.label.cex = 1, vertex.color = vertex_color_new,
    #                     edge.color = edge_color_new)

# Functions for Plotting ======================================================


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
    } else if (length(vertex_parents(g_old, v)$vertices) > 1) {
        return(FALSE)
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
    g_old <- igraph::add_edges(
        g_old, edges = c(
            added_vertex, v
            ))
    plot_tree(g_old, outname = filename) ####
    desired_parent <- igraph::neighbors(g_new, added_vertex, mode = "in")$name
    if ( !(desired_parent %in% igraph::V(g_old)$name) ) {
        g_old <- create_parent(g_old, g_new, added_vertex, filename = filename)
    } else {
        g_old <-
            igraph::add_edges(
                g_old, edges = c(desired_parent, added_vertex)
            )
        plot_tree(g_old, outname = filename) ####
    }
    g_old <- create_children(g_old, g_new, v, filename = filename)
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
#' @param v an igraph vertex name present in the old graph, the end node in the
#' path
#' @return boolean; True if all parents are desired and in correct order, False
#'   otherwise
check_parent_path <- function(g_old, g_new, v) {
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


#' Recursively Check Children
#'
#' Method for checking child path for desired node.
#'
#' @param g_old an igraph graph, the working graph
#' @param g_new an igraph graph, the desired graph
#' @param v an igraph vertex name present in the old graph, the end node in the
#'  path
#' @return boolean; True if all parents are esired and in correct order, False
#' otherwise
check_child_path <- function(g_old, g_new, v) {
    old_children <- igraph::subcomponent(g_old, v, mode = "out")$name
    desired_children <- igraph::subcomponent(g_new, v, mode = "out")$name
    if ( setequal(old_children, desired_children) ) {
        return(TRUE)
    }
    return(FALSE)
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
    desired_subgraph <- igraph::subcomponent(g_new, v, mode = "out")
    if ( length(desired_subgraph) > 1 ) {
        for (i in 2:length(desired_subgraph)) {
            v_name <- desired_subgraph[i]$name
            if ( v_name %in% igraph::V(g_old)$name ) {
                next
            } else {
                g_old <-
                    igraph::add_vertices(
                        g_old, 1, attr = list(name = v_name
                        ))
            }
            desired_parent <- igraph::neighbors(g_new, v_name, mode = "in")[1]$name
            if ( !(desired_parent %in% igraph::V(g_old)$name)) {
                warning("No parent exists in working graph, cannot add edge")
                plot_tree(g_old, filename) ####
            } else {
                g_old <- igraph::add_edges(
                            g_old, edges = c(desired_parent, v_name))
            }
            plot_tree(g_old, outname = filename) ####
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
    if ( !(v %in% igraph::V(g_old)$name) ) {
        return(g_old)
    }
    # not desired
    if ( !(v %in% igraph::V(g_new)$name) ) {
        # desired children
        if ( children_desired(g_old, g_new, v) ) {
            old_children <- igraph::subcomponent(g_old, v, mode = "out")
            for (i in 1:(length(old_children) - 1)) {
                v_name <- rev(old_children)[i]$name
                g_old <- vertex_decision(
                    g_old, g_new, v_name, file = file)
            }
        # un-desired children
        }
        g_old <- igraph::delete_vertices(
            g_old, igraph::subcomponent(g_old, v, mode = "out"))
        plot_tree(g_old, file) ####
    # desired node
    } else {
        # incorrect parent
        if ( !correct_parent(g_old, g_new, v) ) {
            for (i in 1:length(igraph::neighbors(g_old, v, mode = "in"))) {
                # delete incorrect incident edges
                    desired_parent <-
                        igraph::neighbors(g_new, v, mode = "in")[1]$name
                if ( igraph::neighbors(g_old, v, mode = "in")[i]$name !=
                         desired_parent ) {
                    g_old <-
                        igraph::delete_edges(
                            g_old, igraph::E(g_old, c(
                                igraph::neighbors(
                                    g_old, v, mode = "in")[i]$name, v)
                                ))
                }
            }
        }
        # if the degree in is one, then that's the correct parent, although the
        # parent path could be wrong. Otherwise it's a lone node that needs a
        # parent created.
        if ( igraph::degree(g_old, v, mode = "in")==0 ) {
            # parent in current graph?
            parent <- igraph::neighbors(g_new, v, mode = "in")$name
            if (length(parent) > 0) {
                if ( parent %in% igraph::V(g_old)$name ) {
                    g_old <- igraph::add_edges(
                        g_old, edges = c(
                            parent, v))
                    plot_tree(g_old, file) ####
                    if ( !check_parent_path(g_old, g_new, v) ) {
                        root_old <- igraph::V(g_old)[which(
                            igraph::degree(
                                g_old, v = igraph::V(g_old), mode = "in")==0)]
                        old_path <- igraph::get.shortest.paths(
                            g_old, root_old, v)$vpath[[1]]
                        for (i in 2:length(old_path)) {
                            v_name <- rev(old_path)[i]$name
                            g_old <- vertex_decision(
                                g_old, g_new, v_name, file = file)
                        }
                    }
                } else {
                    g_old <- create_parent(g_old, g_new, v, filename = file)
                    if ( !check_parent_path(g_old, g_new, v) ) {
                        root_old <- igraph::V(g_old)[which(
                            igraph::degree(
                                g_old, v = igraph::V(g_old), mode = "in")==0)]
                        old_path <- igraph::get.shortest.paths(
                            g_old, root_old, v)$vpath[[1]]
                        for ( i in 1:length(old_path) ) {
                            v_name <- rev(old_path)[i]$name
                            g_old <- vertex_decision(
                                g_old, g_new, v_name, file = file)
                        }
                    }
                }
            }
        }
        if ( !check_child_path(g_old, g_new, v) ) {
            g_old <- create_children(g_old, g_new, v, filename = file)
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
    Sys.sleep(1.5)
    vertex_color_old <- RColorBrewer::brewer.pal(3, "Set2")[3]
    edge_color_old <- RColorBrewer::brewer.pal(3, "Set2")[1]
    png(width = 1366, height = 768, filename = paste0(
        outname, "_", format(Sys.time(), "%H%M%OS2"), ".png"))
    par(mfrow= c(1,2), srt = 0, cex = .75)
    igraph::plot.igraph(g, layout = igraph::layout_as_tree(g),
                        vertex.label.cex = 1, vertex.color = vertex_color_old,
                        edge.color = edge_color_old)
    dev.off()
    Sys.sleep(1.5)
}


#' Transform Tree
#'
#' Entry function for transforming a taxonomy tree from an "old" status to a
#' "new status".
#'
#' @param g_old The old/working igraph
#' @param g_new The desired igraph
#' @param file Image output name
#' @return An igraph object identical to g_new. Creates images and saves them
#' to the respective location.
transform_tree <- function(g_old, g_new, file) {
    old_vrts <- rev(igraph::V(g_old)$name)
    for (i in 1:length(vrts)) {
        g_old <- vertex_decision(g_old, g_new, old_vrts[i], file = file)
    }
    return(g_old)
}


