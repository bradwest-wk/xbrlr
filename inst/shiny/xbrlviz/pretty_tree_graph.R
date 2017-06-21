# creates tree plot for xbrlviz

library(xbrlr)

#' Create Pretty Tree Graph
#'
#' Creates a pretty looking network tree graph of a given statement or
#' disclosure
#'
#' @param statement The statement of interest
#' @param df The taxonomy dataframe to use (e.g. calc_link_dirty)
#' @param link The linkbase, either "Calculation" or "Presentation"
#' @param names TRUE if element names should be plotted, FALSE if no names
#' @param xlim The xlimit of the plot to zoom to (max is c(-1,1))
#' @param ylim The ylimit of the plot to zoom to (max is c(-1,1))
#' @param zoom The zoom, useful for interactive plots
#' @return A graph, plotted
pretty_tree_graph <- function(statement, df, link, names,
                              xlim = c(-1,1), ylim = c(-1,1), zoom
                              ) {
    stmt_of_interest <- xbrlr:::find_statement(statement, df, link)
    graph <- xbrlr:::create_graph(stmt_of_interest)
    par(srt = 290)
    xbrlr:::plot_single_graph(graph, statement, title_size = 3,
                              display_names = names,
                              xlimit = xlim,
                              ylimit = ylim,
                              zoom = zoom,
                              legend = FALSE)
}



