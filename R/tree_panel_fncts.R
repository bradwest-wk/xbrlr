# Functions for plotting panels of statement trees


#' Draw a Simple Legend
#'
#' This draws a simple legend
#'
#' @export
draw_legend <- function() {
    plot(0:1, 0:1, type="n", axes = F, ann=FALSE)
    # box(which = "plot", lwd = 4)
    abline(h = 0.4, lwd = 10)
    abline(h = 0, lwd = 10)
    points(0.15, 0.3, pch = 21, bg = "#8DA0CB", cex = 35)
    text(0.35, 0.3, labels = "Elements", cex = 15, pos = 4)
    segments(0.05, 0.2, 0.25, 0.2, col = "#66C2A5", lwd = 20)
    text(0.35, 0.2, labels = "Addition Link", cex = 15, pos = 4)
    segments(0.05, 0.1, 0.25, 0.1, col = "#FC8D62", lwd = 20)
    text(0.35, 0.1, labels = "Subtraction Link", cex = 15, pos = 4)
}

#' Plot Graph
#'
#' Plots a single tree graph
#'
#' @export
#' @param g The graph to plot
#' @param title The title of the graph
#' @param title_size Size of title
#' @param display_names Whether or not to display names
#' @param xlimit The xlimit of the plot to zoom to (max is c(-1,1))
#' @param ylimit The ylimit of the plot to zoom to (max is c(-1,1))
#' @param zoom Optional zoom parameter for vertex and label size
#' @param legend True if lengend is desired
#' @return A graph image
plot_single_graph <- function(g, title, title_size = 1,
                              display_names = FALSE,
                              xlimit = c(-1,1),
                              ylimit = c(-1,1),
                              zoom = 1,
                              legend = FALSE) {
    darkgrey <- col2rgb("darkgrey")
    framecolor <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1],
                      alpha = 75,
                      max = 255)
    roots <- which(igraph::degree(g, v = igraph::V(g), mode = "in")==0)
    igraph::plot.igraph(g, layout = igraph::layout_as_tree(
        g, root = roots, rootlevel = c(rep(1, length(roots)))),
        vertex.color = igraph::V(g)$color,
        vertex.label = if(display_names) igraph::V(g)$name else NA,
        vertex.label.cex = 2*log(zoom+3, 20),
        # ifelse(igraph::V(g)$depth<1, igraph::V(g)$name, NA),
        vertex.frame.color = framecolor, vertex.shape = "circle",
        vertex.size = zoom^(-0.5), # 2*log(zoom+3, 10),
        vertex.label.dist = 0, vertex.label.degree = pi/2,
        vertex.label.color = "black",
        edge.arrow.size = .3, edge.arrow.width = 2, asp = 0,
        edge.curved = F,
        edge.color = igraph::E(g)$color,
        edge.width = 2, xlim = xlimit, ylim = ylimit)
    if (legend) {
        legend("bottomright",
               legend = c("Elements", "Addition", "Subtraction"),
               col = c("#8DA0CB", "#66C2A5", "#FC8D62"),
               lwd = c(NA, 4, 4),
               pch = c(21, NA, NA),
               pt.bg = "#8DA0CB",
               pt.cex = 3)
    }
    # title(main = title, cex.main = title_size)
    text(1,1.2, labels = title, cex = title_size, adj = c(1, 1), srt = 0)
}


#' Plot Multiple Graphs
#'
#' Method for plotting panels of statements
#'
#' @export
#' @param graphs a list or vector of graphs to plot
#' @param out_file the output file to save to
#' @param main_title the overall title of the graph
#' @param graph_titles a parallel vector of titles whose indices correspond to
#' @param rows The number of rows
#' @param cols The number of columns
#' the graph vector.
panel_trees <- function(graphs, out_file, main_title, graph_titles, rows, cols){
    if(cols*rows == length(graphs)){
        # in order to plot a legend
        cols <- cols+1
    }
    png(out_file, width = 1920*cols,
        height = 1920*rows)
    par(mfrow=c(rows, cols),
        mar = c(10,10,15,10),
        oma = c(10, 10, 40, 10), srt = 0, xpd = TRUE)
    for (i in 1:length(graphs)) {
        plot_single_graph(graphs[[i]], graph_titles[i])
        box(which = "plot", lwd=4)
    }
    # blank plots so that legend is always bottom right
    if((cols-1)*rows==length(graphs)){
        for (i in (length(graphs)+1):(cols*rows-1)) {
            plot(0:1, 0:1, type="n", axes = F, ann=FALSE)
        }
    }
    draw_legend()
    title(main = main_title, cex.main = 25, outer = TRUE)
    dev.off()
}
