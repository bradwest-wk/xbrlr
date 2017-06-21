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
#' @return A graph, plotted
pretty_tree_graph <- function(statement, df, link, names) {
    stmt_of_interest <- find_statement(statement, df, link)
    graph <- create_graph(stmt_of_interest)
    par(srt = 290)
    plot_single_graph(graph, statement, title_size = 3, display_names = names)
}



