# creates sideBarPanel for xbrlviz

library(xbrlr)

#' Create sidebar layout
#'
#' Function for creating sidebar layout from a set of statement names
#'
#' @param df The dataframe of the calculation link to use
#' @param link The link to use
#' @return A sidebarPanel object
create_sidebar <- function(df, link) {
    statement_names <- get_stmt_names(df, link)
    selectInput(
        "statement", "Statement:", choices = statement_names,
        multiple = FALSE
    )
}

