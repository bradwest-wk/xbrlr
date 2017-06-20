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

