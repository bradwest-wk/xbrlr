# Experiment with visNetwork

library(visNetwork)
library(xbrlr)
library(tidyverse)
library(RColorBrewer)

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
# lets look at the comprehensive statements:
stmts <- get_stmt_names(calc_link_dirty,"Statement")
