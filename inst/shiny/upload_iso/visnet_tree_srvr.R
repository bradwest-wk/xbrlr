# =============================================================================
# Server code for generating visNetwork graph
# =============================================================================

#' Get visNet Nodes
#'
#' Cleans input and outputs nodes df for visNetwork graph
#'
#' @param input The input file, usually piped from get_raw_input
#' @param physics_on Should physics be used for nodes
#' @return a nodes dataframe to be used in the visNet graph
#' @export
get_visNet_nodes <- function(input, physics_on = FALSE) {
    colnames(input) <- tolower(colnames(input))
    node_vec <- unique(c(input$parent, input$child))
    node_vec <- sort(node_vec)
    nodes <- tibble::as_tibble(node_vec) %>% tibble::rowid_to_column() %>%
        dplyr::rename(id = rowid, label = value) %>%
        dplyr::mutate(title = label, physics = physics_on)
    if ('group' %in% colnames(input)) {
        nodes <- nodes %>%
            dplyr::left_join(input[c('child','group')], by = c('label'='child'))
    }
    if ('label' %in% colnames(input)) {
        input %>% dplyr::rename(title = label)
        nodes <- nodes %>% dplyr::select(-title) %>%
            dplyr::left_join(
                input[c('child', 'label')], by = c('label'='child'))
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
    colnames(raw)[c(1,2)] <- c('parent', 'child')
    filled <- tidyr::fill(raw, 'parent')
    if ( sum(duplicated(filled)) > 0 ) {
        warning("Duplicated edges in dataframe--investigate further.")
    }
    return(filled)
}

#' Dataframe from CSV Input
#'
#' Gets DF from uploaded input in shiny
#'
#' @param input The input file to process.  In shiny this will usually be
#' something like input$in_file. See return values for fileInput for more
#' information
#' @return a raw dataframe, uncleaned except for filling
get_raw_input_csv <- function(input) {
    # need to rename the temp file so that readxl knows how to handle
    # file.rename(input$datapath, paste0(input$datapath, '.csv', sep=''))
    raw <- readr::read_csv(input$datapath, col_names = TRUE)
    # fill down missing values
    colnames(raw)[c(1,2)] <- c('parent', 'child')
    filled <- tidyr::fill(raw, 'parent')
    if ( sum(duplicated(filled)) > 0 ) {
        warning("Duplicated edges in dataframe--investigate further.")
    }
    return(filled)
}

# =============================================================================
# Server Components
# =============================================================================
# Reactive function to get the network. Instead of fitting inside of
# renderVisNetwork(), which would only render the network useful to the
# ui, and not other server functions
network <- reactive({
    req(rv$data)
    nodes <- get_visNet_nodes(rv$data, physics_on = FALSE)
    edges <- get_visNet_edges(rv$data, nodes)

    network <- visNetwork(nodes, edges, main = input$input_file$name) %>%
        visEdges(width = 0.2, arrows = 'to', arrowStrikethrough = F) %>%
        visNodes(color = list(background = 'rgba(151, 194, 252, 0.65)'),
                 size = 13, font = list(size =13, align = 'left')) %>%
        visOptions(highlightNearest = list(enabled = T, degree = 2, hover = F),
                   nodesIdSelection = list(enabled = T, useLabels = T)) %>%
        visIgraphLayout(layout = 'layout_as_tree')
    # modify the matrix (rotate 180 degrees)
    l <- ncol(nodes)
    network$x$nodes[, l+1] <-
        network$x$nodes[, c(l+1)] + network$x$nodes[, c(l+2)]
    network$x$nodes[, l+2] <-
        network$x$nodes[, c(l+1)] - network$x$nodes[, c(l+2)]
    network$x$nodes[, l+1] <-
        network$x$nodes[, c(l+1)] - network$x$nodes[, c(l+2)]
    network$x$nodes[, l+1] <- network$x$nodes[, l+1] * -1
    network$x$nodes$y <- network$x$nodes$y*1.75
    network
})

# output for generating visNetwork Tree, in rotated form with selection of nodes
output$vis_net <- renderVisNetwork({
    network <- network()
    network
})

# Save Button
# see: https://github.com/datastorm-open/visNetwork/issues/138
output$download_net <- downloadHandler(
    filename = function() {
        paste0(rv$name, '_', Sys.Date(), '.html')
    },
    content = function(file) {
        network <- network()
        # attempt to resize the output network
        network %>%
            visOptions(width='1800px', height='1125px',
                       highlightNearest =
                           list(enabled = T, degree = 2, hover = F),
                       nodesIdSelection = list(enabled = T, useLabels = T)) %>%
            visSave(file)
    }
)


# Redraw network. Requires passing visNetworkProxy
observeEvent(input$redraw_net, {
    output$vis_net <- renderVisNetwork({
        network <- network()
        network
    })
})


