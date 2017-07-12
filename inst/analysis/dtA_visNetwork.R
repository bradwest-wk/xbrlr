# Experiment with visNetwork

library(visNetwork)
library(xbrlr)
library(tidyverse)
library(RColorBrewer)
library(igraph)
library(stringr)

# =============================================================================
# load whole taxonomy graph from dtA_whole_tx.R, then get a subset of that
# graph
set.seed(244)
# smaller graph for easy render
g2 <- delete_vertices(g, floor(runif(n=3000, min = 1, max = gorder(g))))
g2 <- g

# create a nodes dataframe
nodes <- data.frame(
    id = 1:gorder(g2), title = V(g2)$name, group = V(g2)$group,
    color = V(g2)$color, shape = rep("circle", gorder(g2)), physics = FALSE,
    stringsAsFactors = FALSE)

#' Create Edge ID matrix
#'
#' @param g igraph object
#' @param nodes visNodes dataframe
#' @return the edge dataframe with id's corresponding to the nodes df
get_edge_df <- function(g, nodes) {
    named_edges <- as.data.frame(get.edgelist(g), stringsAsFactors = FALSE)
    from <- c()
    to <- c()
    color <- c()
    value <- c()
    for (i in 1:nrow(named_edges)) {
        from <- c(from, nodes$id[which(nodes$title==named_edges$V1[i])])
        to <- c(to, nodes$id[which(nodes$title==named_edges$V2[i])])
        color <- c(color, E(g)[i]$color)
        value <- c(value, E(g)[i]$weight)
    }
    edges <- cbind.data.frame(from, to, color, value)
    return(edges)
}

#' Get Unique Groups from Nodes Matrix
#'
#' Grabs the unique groups from a vector of groups, some elements of which
#' have more than one group in them
#' @param groups a vector of groups
#' @return a vector of the unique groups in the vector
unique_groups <- function(groups) {
    all <- c()
    for (el in groups) {
        all <- c(all, str_split(el, ", ")[[1]])
    }
    unique(all)
}


edges <- get_edge_df(g2, nodes)
edges$physics <- rep(FALSE, nrow(edges))

# build igraph layouts for passing to visNetwork
roots <- which(igraph::degree(g, v = igraph::V(g), mode = "in")==0)
l_rt <- layout_as_tree(g2, root = numeric(), circular = TRUE)
l_drl <- layout_with_drl(g2, weights = NULL, options = drl_defaults$final)
l_mds <- layout_with_mds(g2)

# get unique groups
the_groups <- unique_groups(nodes$group)

# build graph
visNetwork(nodes, edges, main = "Test") %>%
    visEdges(width = 0.5, arrow = "to", arrowStrikethrough = FALSE) %>%
    visOptions(selectedBy = list(variable = "group", multiple = TRUE)) %>%
    visIgraphLayout(layout = "layout.norm", randomSeed = 222,
                    smooth = TRUE, layoutMatrix = l_rt) %>%
    visGroups(groupname = the_groups[8], shape = "triangle") %>%
    visGroups(groupname = the_groups[6], shape = "square") %>%
    visGroups(groupname = the_groups[11], shape = "diamond")



# =============================================================================
# visNetwork needs at least two pieces of information:
# 1. a nodes data.frame with id column
# 2. an edgef data.frame, with from and to folumne, which make the link with id

# load nodes (elements)
load("./data/revenue_el_2017.Rdata")
sum(duplicated(revenue_el_2017))
el <- cbind.data.frame(1:length(revenue_el_2017$element), revenue_el_2017)
colnames(el) <- c("id", "label")


load("./data/calc_link_2017.Rdata")
rev_edges <- calc_link_2017 %>%
    filter(parent %in% el$name | child %in% el$name)
# problem: some of the child elements or parent elements (but not both) are in
# the Revenue Element names. Not sure how to handle this as it will make
# more disconnected nodes
sum(duplicated(rev_edges))
sum(rev_edges$parent %in% el$name)
sum(rev_edges$child %in% el$name)

# we can gather edges by restricting both parent and child to be in the list
# of revenue edges, but this creates a smaller, less comprehensive dataset
rev_edges2 <- calc_link_2017 %>%
    filter(parent %in% el$name & child %in% el$name)
sum(duplicated(rev_edges2))
sum(rev_edges2$parent %in% el$name)
sum(rev_edges2$child %in% el$name)
sum(duplicated(rev_edges2))

# let's deal with the smaller dataset first
edges <- inner_join(rev_edges2, el, by = c("parent" = "label")) %>%
    rename(from = id) %>% inner_join(el, by = c("child" = "label")) %>%
    rename(to = id) %>% select(from, to, parent, child)

# =============================================================================
# create the network
set.seed(1)
visNetwork(nodes = el, edges = edges) %>%
    visHierarchicalLayout(direction = "LR", levelSeparation = 500)
net <- visNetwork(nodes = el, edges = edges)

# =============================================================================
# cleaner version
load("./data/revenue_el_2017.RData")
els <-  cbind.data.frame(1:length(revenue_el_2017$element), revenue_el_2017)
colnames(els) <- c("id", "label")

load("./data/calc_link_dirty.Rdata")
parent_info <- calc_link_dirty %>% filter(prefix == "us-gaap") %>%
    mutate(parent = sub("us-gaap:", "", parent)) %>%
    inner_join(els, by = c("name" = "label")) %>% filter(!is.na(name))

# nodes <- parent_info %>%
#     rename(label = name, title = label, level = depth, value = weight) %>%
#     mutate(level = as.numeric(level),value = as.numeric(value),
#            group = "Revenues", color = "#8DA0CB",
#            size = ifelse(level == 0, 45, ceiling((level)^(-.4)*40))) %>%
#     select(id, label, title, level, value, group, color, size)
# nodes <- nodes[!duplicated(nodes),]

nodes <- els %>% mutate(color = ifelse(label == "Revenues","#8DA0CB", "#E78AC3"))

edges <- parent_info %>% rename(to = id) %>%
    inner_join(els, by = c("parent" = "label")) %>% rename(from = id) %>%
    mutate(color = ifelse(weight == -1, "#FC8D62", "#66C2A5"),
           dashes = ifelse(weight == -1, TRUE, FALSE)) %>%
    select(from, to, color, dashes)

set.seed(22)
visNetwork(nodes, edges)

visNetwork(nodes, edges) %>%
    visHierarchicalLayout(direction = "LR", levelSeparation = 500)

# =============================================================================
# create graph using igraph

stmt_names <- get_stmt_names(calc_link_dirty, "Calculation")
clink <- find_statement(stmt_names[1], calc_link_dirty, "Calculation")
for (stmt in stmt_names[2:length(stmt_names)]) {
    stmt_of_interest <- find_statement(stmt, calc_link_dirty, "Calculation")
    clink <- bind_rows(clink, stmt_of_interest)
}
g2 <- create_graph(clink)

nodes <- cbind.data.frame(as.numeric(V(g2)), V(g2)$name, V(g2)$color,
                          V(g2)$depth, V(g2)$label)
colnames(nodes) <- c("id", "label", "color", "level", "title")
nodes <-  nodes %>%
    mutate(size = ifelse(level == 0, 45, ceiling((level)^-.4*40)))

# I guess this is not necessary
nodes_dedup <- as_tibble(nodes) %>% arrange(level)
sum(duplicated(nodes_dedup$label))


edges <- cbind.data.frame(as.numeric(head_of(g2, E(g2))),
                          as.numeric(tail_of(g2, E(g2))),
                          as.character(E(g2)$color), stringsAsFactors = FALSE)
colnames(edges) <- c("from", "to", "color")
edges <- as_tibble(edges) %>%
    mutate(dashes = ifelse(color == "#FC8D62", TRUE, FALSE))

visNetwork(nodes, edges)
# =============================================================================


