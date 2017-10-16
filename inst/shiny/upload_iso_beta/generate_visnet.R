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
