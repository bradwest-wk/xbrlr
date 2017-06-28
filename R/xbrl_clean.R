# Functions for cleaning xbrl edgelists, intended to be a comprehensive set of
# functions for creating igraph objects of messy edgeslists and taxonomy trees.

# Author: Brad West

# gimme dem functions
source("./R/xbrl_taxonomy_functions.R")
source("./R/tree_plot_fnct.R")

#' Read and Convert to Dataframe
#'
#' Reads a file in as an excel or a csv and creates an edgelist
#'
#' @export
#' @param edge_file The path to an excel or csv edgefile that contains at
#' minimum two columns, a parent and a child.
#' @param sheet If an excel document, the sheet that contains the edges
#' @param parent_col The column that contains the parent element names
#' @param child_col The column that contains the child element names
#' @param fill Do parent columns need to be filed?
#' @param column_header do column headers exist?
# fill fixes data is in the following format:
#' _________________________#
#' | parent_1 | child_1 |
#' |          | child_2 |
#' |          | child_3 |
#' | parent_2 | child_1 |
#' |          | child_2 |
#' @return A tibble that con be converted to a dataframe and saved as an Rdata
#' file
edge_to_df <- function(edge_file,
                          sheet = 1,
                          parent_col = 1,
                          child_col = 2,
                          fill = TRUE,
                          column_header = TRUE
                          ) {
    if(endsWith(edge_file, ".xlsx") | endsWith(edge_file, ".xls")) {
        edges <- readxl::read_excel(
            edge_file, sheet = sheet, col_names = FALSE,
            skip = ifelse(column_header, 1, 0))
    } else if(endsWith(edge_file, ".csv")) {
        edges <- readr::read_csv(edge_file, col_names = FALSE,
                                 skip = ifelse(column_header, 1, 0))
    } else {
        stop("Could not determine input file type")
    }
    edges <- edges[, c(parent_col, child_col)]
    colnames(edges) <- c("parent", "child")
    if (fill == TRUE) {
        edges <- tidyr::fill(edges, parent)
    }
    # check for duplicated edges
    if ( sum(duplicated(edges)) > 0 ) {
        warning("Duplicated edges in dataframe; investigate further.")
    }
    edges
}


#' Get Statement Names
#'
#' Functionality for obtaining statement names from a given taxonomy, either
#' presentation link, or calculation
#'
#' @export
#' @param df The dataframe of the link containing elements, definitions, and
#' linkroles (e.g. calc_link_dirty)
#' @param type What type of statement to look for, statement or disclosure
#' @return A vector of statement names
get_stmt_names <- function(df, type = "Statement") {
    statement_names <- df[df$prefix=="Definition",]
    statement_names <- statement_names[!is.na(statement_names$prefix),] %>%
        dplyr::select(name)
    statement_names <- statement_names$name
    stat_nums <- which(substr(statement_names, 10, 9+nchar(type))==type)
    statement_names[stat_nums]
}


#' List of graphs
#'
#' Creates a list of igraph objects which can be used to iterate over
#'
#' @export
#' @param statement_names A vector of statement names
#' @param df A dataframe of the link (e.g. calc_link_dirty)
#' @param link The linkbase to use, either Presentation or Calculation
#' @return A list of igraph objects
create_graph_list <- function(statement_names, df, link) {
    graphs <- vector("list", length(statement_names))
    for (i in 1:length(statement_names)) {
        stmt_df <- find_statement(statement_names[i], df, link)
        graphs[[i]] <- create_graph(stmt_df)
    }
}

