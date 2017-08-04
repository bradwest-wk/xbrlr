# Creates a visNetwork tree that allows users to select nodes by name,
# define groups, etc in the "upload taxonomy" tab of xbrlviz shiny app.

# PLAN:
# User can select via radio button, whether they want the traditional (igraph)
# plot, or if they want visNetwork plot that adds functionality for selecting
# nodes, highlighting edges, etc.

# For visNet:
# 1. Function to clean data and add groups. Need to output Nodes df with
# id, label, title, group, color, physics, value
# Edge df with:
# from, to, color, width, physics

# 2. Plotting function for visNet plot including
# physics, highlighting nearest neighbors, selection by node id (label)
# and potentially by group

# =============================================================================

#' Get visNet Nodes
#'
#' Cleans input and outputs nodes df for visNetwork graph
#'
#' @param input The input file, usually piped from get_raw_input
#' @param physics_on Should physics be used for nodes
#' @return a nodes dataframe to be used in the visNet graph
#' @export
get_visNet_nodes <- function(input, physics_on = TRUE,
                             layout_direction = 'UD') {
    colnames(input) <- tolower(colnames(input))
    node_vec <- unique(c(input$parent, input$child))
    node_vec <- sort(node_vec)
    nodes <- as_tibble(node_vec) %>% rowid_to_column() %>%
        rename(id = rowid, label = value) %>%
        mutate(title = label, physics = physics_on)
    if ('group' %in% colnames(input)) {
        nodes <- nodes %>%
            left_join(input[c('child','group')], by = c('label'='child'))
    }
    if ('label' %in% colnames(input)) {
        input %>% rename(title = label)
        nodes <- nodes %>% select(-title) %>%
            left_join(input[c('child', 'label')], by = c('label'='child'))
    }
    return(nodes)
}


#' Get visNet Edges
#'
#' Output an edges datafrome for visNetwork to handle
#'
#' @param input The input file, piped from get_raw_input
#' @param nodes The nodes dataframe, output from get_visNet_nodes
#' @return an edges dataframe with from and to columns corresponding to the ids
#' of the nodes
#' @export
get_visNet_edges <- function(input, nodes) {
    from <- c()
    to <- c()
    # get edges
    edges <- input[,c(1,2)]
    colnames(edges) <- c('parent', 'child')
    for (i in 1:nrow(edges)) {
        from <- c(from, nodes$id[which(nodes$label==edges$parent[i])])
        to <- c(to, nodes$id[which(nodes$label==edges$child[i])])
    }
    edges <- data.frame(from, to, stringsAsFactors = FALSE)
    return(edges)
}


#' Dataframe from Excel Input
#'
#' Gets DF from uploaded input in shiny
#'
#' @param input The input file to process.  In shiny this will usually be
#' something like input$in_file. See return values for fileInput for more
#' information
#' @return a raw dataframe, uncleaned except for filling
get_raw_input <- function(input) {
    # need to rename the temp file so that readxl knows how to handle
    file.rename(input$datapath, paste0(input$datapath, '.xlsx', sep=''))
    raw <- readxl::read_excel(paste(input$datapath, '.xlsx', sep=""),
                              sheet = 1, col_names = TRUE)
    # fill down missing values
    filled <- tidyr::fill(data, colnames(raw)[1])
    if ( sum(duplicated(filled)) > 0 ) {
        warning("Duplicated edges in dataframe--investigate further.")
    }
    return(filled)
}


