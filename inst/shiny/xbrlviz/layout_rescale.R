library(xbrlr)

#' Output Dataframe With Plotting Parameters
#'
#' An igraph is plotted according to a layout matrix. Before plotting, this
#' matrix is rescaled to a plotting window from -1 to 1.  In order to select
#' points in a shiny app, we need to have the correct x and y coordinates
#' associated with the the element.  This function rescales the layout matrix
#' and bind the x and y coordinates to the statement dataframe
#'
#' @param statement The statement of interest
#' @param df The calculation link dataframe
#' @param link The link, "Calculation" or "Presentation"
#' @return A dataframe with the attached x and y coordinates
rescale_layout <- function(statement, df, link) {
    stmt_of_interest <- find_statement(
        statement, df, link)
    graph <- create_graph(stmt_of_interest)
    l = igraph::layout_as_tree(graph)
    # need to rescale to -1 to 1 plotting region
    x <- rescale(l[,1], to = c(-1,1))
    y <- rescale(l[,2], to = c(-1,1))
    # bind the coordinates to the statement
    coord_df <- cbind(x,y,stmt_of_interest)
}



