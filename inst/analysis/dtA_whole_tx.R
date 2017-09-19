# visualize entire 2017 calculation link taxonomy

library(xbrlr)
library(tidyverse)
library(igraph)
library(stringr)
library(stringi)
library(RColorBrewer)
library(visNetwork)

source("./inst/shiny/xbrlviz/whole_tx.R")

# =============================================================================
# Test on full taxonomy
# -----------------------------------------------------------------------------
# The stress based algorithms for graph layout are the best. We need to use
# those, with appropriate characteristics like coloring and node sizing to
# properly identify groups.
# =============================================================================
load('./inst/shiny/xbrlviz/data/calc_link_dirty.Rdata')
# remove rows with all NAs
calc_link_dirty <- calc_link_dirty %>%
    filter(rowSums(is.na(calc_link_dirty))!=ncol(calc_link_dirty))
# need to get 'groups' which correspond to statements
t <- calc_link_dirty
t <- statement_column(t) # whole_tx.R

# The individual statements are too diverse to use as grouping variables
# themselves. We should consolidate.
# -----------------------------------------------------------------------------
groups <- list('104000'='financial_position',
               '104050'='financial_position',
               '104100'='financial_position',
               '108000'='financial_position',
               '108050'='financial_position',
               '124000'='income_statement',
               '124001'='income_statement',
               '124003'='income_statement',
               '148400'='comprehensive_income',
               '148400'='comprehensive_income',
               '148410'='comprehensive_income',
               '148411'='comprehensive_income',
               '148412'='comprehensive_income',
               '148413'='comprehensive_income',
               '148600'='shareholders_equity',
               '148610'='shareholders_equity',
               '152000'='partners_capital',
               '152200'='cash_flow',
               '160000'='cash_flow',
               '164000'='cash_flow',
               '172600'='cash_flow',
               '200000'='org_consol_present_fs',
               '210000'='bs_offset',
               '210100'='bs_offset',
               '320000'='rec_loans_notes',
               '320500'='rec_loans_notes',
               '330000'='invest_debt_equity',
               '330001'='invest_debt_equity',
               '340000'='inventory',
               '440000'='rev_recog',
               '450000'='commit_cont',
               '450100'='commit_cont',
               '460000'='debt',
               '470200'='other_liabilities',
               '472000'='noncontrolling_interest',
               '500000'='equity',
               '606004'='rev_from_contract',
               '710000'='compensation_related_costs',
               '730000'='compensation_related_costs',
               '730101'='compensation_related_costs',
               '730102'='compensation_related_costs',
               '730103'='compensation_related_costs',
               '730104'='compensation_related_costs',
               '730105'='compensation_related_costs',
               '730106'='compensation_related_costs',
               '730107'='compensation_related_costs',
               '730108'='compensation_related_costs',
               '730109'='compensation_related_costs',
               '730110'='compensation_related_costs',
               '730111'='compensation_related_costs',
               '770000'='income_taxes',
               '800000'='business_comb',
               '805000'='derivative_instruments',
               '815000'='fair_value_measurements',
               '820000'='foreign_operations',
               '831000'='leases',
               '842100'='leases',
               '842200'='leases',
               '842300'='leases',
               '842400'='leases',
               '842500'='leases',
               '865000'='transfers_servicing',
               '939000'='financial_services',
               '940000'='financial_services',
               '940001'='financial_services',
               '940050'='financial_services',
               '942000'='financial_services',
               '943000'='financial_services',
               '944000'='financial_services',
               '946000'='financial_services',
               '955000'='healthcare_org',
               '965000'='extractive_ind',
               '975000'='real_estate',
               '980000'='regulated_ops',
               '993500'='investment_holdings'
               )
# -----------------------------------------------------------------------------
# create a group field in t, parse parent
node_tmp <- t %>%
    filter(prefix=='us-gaap') %>%
    mutate(group = NA)
for (i in 1:nrow(node_tmp)) {
    node_tmp$group[i] <- groups[[substr(node_tmp$statement[i],1,6)]][[1]]
    node_tmp$parent[i] <- strsplit(node_tmp$parent[i], 'us-gaap:')[[1]][2]
}

# the issue is that we now have nodes that belong to multiple groups.  This makes
# it difficult to plot as a node can only belong to one group
node_tmp <- node_tmp %>%
    select(name, label, group) %>%
    rename(child = name)

# in visNetwork we can have comma separated values for the group variable which
# indicate multiple group affiliations. We need to add this with a function
# similar to whole_tx::add_groups
node_tmp <- node_tmp %>% mutate(group_multiple = NA)
for (i in 1:nrow(node_tmp)) {
    node_tmp$group_multiple[i] <- paste(node_tmp$group[which(
        node_tmp$child==node_tmp$child[i])], collapse = ", ")
}

node_tmp <- node_tmp %>% select(-group)
node_tmp <- node_tmp[!duplicated(node_tmp),]

nodes <- node_tmp %>% arrange(child) %>% rowid_to_column() %>%
    rename(id = rowid, label = child, title = label, group = group_multiple) %>%
    mutate(physics = FALSE, size = 5) %>%
    select(id, label, title, group, size, physics)

raw_edges <- t %>%
    filter(prefix=='us-gaap') %>%
    mutate(group = NA)
for (i in 1:nrow(raw_edges)) {
    raw_edges$parent[i] <- strsplit(raw_edges$parent[i], 'us-gaap:')[[1]][2]
}
raw_edges <- raw_edges %>% rename(child = name) %>%
    select(parent, child) %>% filter(!is.na(parent))

edges <- get_visNet_edges(raw_edges, nodes)

# -----------------------------------------------------------------------------
# Easier to get nodes via the method below (Run It) and then the methods in
# dtA_visNetwork.R

# need to create a color vector for the length of groups list
colors64 <- c("#000000", "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
              "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
              "#5A0007", "#809693", "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
              "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100",
              "#DDEFFF", "#000035", "#7B4F4B", "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F",
              "#372101", "#FFB500", "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
              "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66",
              "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C",
              "#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
              "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00",
              "#7900D7", "#A77500", "#6367A9", "#A05837", "#6B002C", "#772600", "#D790FF", "#9B9700",
              "#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
              "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C")

colors32 <- colors64[2:33]
# groups <- unique(nodes$group)
# for (i in 1:nrow(nodes)) {
#     nodes$color[i] <- colors32[which(groups==nodes$group[i])]
# }

# Get the layout from igraph
g <- graph_from_data_frame(as.matrix(edges))
l_mds <- layout_with_mds(g)
l_drl <- layout_with_drl(g)
roots <- which(igraph::degree(g, v = igraph::V(g), mode = "in")==0)
l_rt <- layout_as_tree(g, root = roots, circular = TRUE)

visNetwork(nodes, edges, main = 'Test') %>%
    visNodes(font = list(size = 1), size = 0.2) %>%
    visEdges(width = 0.01, arrow = list(to = list(scaleFactor = 0.1)) ,
             arrowStrikethrough = FALSE,
             physics = FALSE) %>%
    visOptions(selectedBy = list(variable = 'group', multiple = TRUE),
             highlightNearest = list(enabled = T, degree = 0, hover = F),
             nodesIdSelection = list(enabled = T, useLabels = T),
             autoResize = T, collapse = list(enabled = T, fit = T)) %>%
    visIgraphLayout(layout = 'layout.norm', randomSeed = 222,
                    smooth = TRUE, layoutMatrix = l_rt) %>%
    visGroups(groupname='financial_position', color = colors32[1]) %>%
    visGroups(groupname='income_statement', color = colors32[1]) %>%
    visGroups(groupname='comprehensive_income', color = colors32[1]) %>%
    visGroups(groupname='shareholders_equity', color = colors32[1]) %>%
    visGroups(groupname='partners_capital', color = colors32[1]) %>%
    visGroups(groupname='cash_flow', color = colors32[1]) %>%
    visGroups(groupname='org_consol_present_fs', color = colors32[1]) %>%
    visGroups(groupname='bs_offset', color = colors32[1]) %>%
    visGroups(groupname='rec_loans_notes', color = colors32[1]) %>%
    visGroups(groupname='invest_debt_equity', color = colors32[1]) %>%
    visGroups(groupname='inventory', color = colors32[1]) %>%
    visGroups(groupname='rev_recog', color = colors32[1]) %>%
    visGroups(groupname='commit_cont', color = colors32[1]) %>%
    visGroups(groupname='debt', color = colors32[1]) %>%
    visGroups(groupname='other_liabilities', color = colors32[1]) %>%
    visGroups(groupname='noncontrolling_interest', color = colors32[1]) %>%
    visGroups(groupname='equity', color = colors32[1]) %>%
    visGroups(groupname='rev_from_contract', color = colors32[1]) %>%
    visGroups(groupname='compensation_related_costs', color = colors32[1]) %>%
    visGroups(groupname='income_taxes', color = colors32[1]) %>%
    visGroups(groupname='business_comb', color = colors32[1]) %>%
    visGroups(groupname='derivative_instruments', color = colors32[1]) %>%
    visGroups(groupname='fair_value_measurements', color = colors32[1]) %>%
    visGroups(groupname='foreign_operations', color = colors32[1]) %>%
    visGroups(groupname='leases', color = colors32[1]) %>%
    visGroups(groupname='transfers_servicing', color = colors32[1]) %>%
    visGroups(groupname='financial_services', color = colors32[1]) %>%
    visGroups(groupname='healthcare_org', color = colors32[1]) %>%
    visGroups(groupname='extractive_ind', color = colors32[1]) %>%
    visGroups(groupname='real_estate', color = colors32[1]) %>%
    visGroups(groupname='regulated_ops', color = colors32[1]) %>%
    visGroups(groupname='investment_holdings', color = colors32[1])

# -----------------------------------------------------------------------------

# source whole_tx.R
t <- taxonomy_load(calc_link_dirty) # taxonomy_load from whole_tx.R

edgelist <- cbind.data.frame(t$parent, t$child, stringsAsFactors=FALSE)
colnames(edgelist) <- c('parent', 'child')
edgelist <- edgelist[!(is.na(edgelist[,1])),]

g <- xbrlr::create_graph(t)
dirty_tax <-
    calc_link_dirty[rowSums(is.na(calc_link_dirty))!=ncol(calc_link_dirty), ]
# would need to make a faster function than this (don't do it in a graph, work
# on the dataframe instead)
g <- add_groups(g, dirty_tax)
edges2 <- as.data.frame(igraph::as_edgelist(g))
colnames(edges2) <- c('parent', 'child')

source("./inst/shiny/upload_iso/visnet_tree_srvr.R")
nodes <- get_visNet_nodes(edgelist)
edges <- get_visNet_edges(edgelist, nodes)


# =============================================================================

plot_graph_local <- function(g, filename, title) {
    darkgrey <- col2rgb("darkgrey")
    framecolor <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 75,
                      max = 255)
    roots <- which(igraph::degree(g, v = igraph::V(g), mode = "in")==0)
    png(filename = filename, width = 11520, height = 11520, res = 100)
    par(cex = 1, cex.main = 5, cex.sub = 4, srt = 270, mar = c(20,4,10,4),
        font.lab = 2, adj = 0)
    igraph::plot.igraph(
        g, layout = igraph::layout_with_drl(
            g, weights = NULL, options = drl_defaults$final),
        vertex.label.cex = igraph::V(g)$label.cex,
        vertex.label = NA, # igraph::V(g)$name,
        # vertex.color = igraph::V(g)$color,
        vertex.frame.color = framecolor, vertex.shape = "circle",
        vertex.size = 0.4,
        vertex.label.dist = .1, vertex.label.degree = pi/2,
        vertex.label.color = "black",
        edge.arrow.size = .4, edge.arrow.width = 1, asp = 0, edge.curved = F
        # edge.color = igraph::E(g)$color
        )
    text(x = 1.05, y = 0, labels = title, srt = 270, font = 2, cex = 5)
    # legend(x = "topleft", c("Addition", "Subtraction"), lty = 1, lwd = 2, col
    # = c(brewer.pal(n = 3, name = "Set2")[1], brewer.pal(n = 3, name =
    # "Set2")[2]), cex = 5)
    dev.off()
}


# =============================================================================
# Run it
load("./data/calc_link_dirty.Rdata")
calc_link_dirty <- calc_link_dirty[!is.na(calc_link_dirty$prefix), ]
statements_dirty <- get_desired_sheets(calc_link_dirty, "both")
statements_2017 <- taxonomy_load(statements_dirty, "Calculation", "both")
g <- xbrlr::create_graph(statements_2017)
g <- add_groups(g, statements_dirty)
g <- group_count(g)

# plot some temp graphs
plot_graph_local(g, "/tmp/vis_fr.png", "FR TEST") # NOPE. looks bad.
plot_graph_local(g, "/tmp/vis_kk.png", "KK TEST") # NOPE. can't run algo
plot_graph_local(g, "/tmp/vis_lgl.png", "LGL TEST") # NOPE. complete mess
plot_graph_local(g, "/tmp/vis_drl.png", "DRL TEST") # YES. weights = NULL must be set
plot_graph_local(g, "/tmp/vis_dh.png", "DH TEST") # NOPE. Too slow
plot_graph_local(g, "/tmp/vis_gem.png", "GEM TEST") # NOPE. Too slow
plot_graph_local(g, "/tmp/vis_graphopt.png", "GRAPHOPT TEST") # MAYBE??
plot_graph_local(g, "/tmp/vis_mds.png", "MDS TEST") # INTERESTING
plot_graph_local(g, "/tmp/vis_rt.png", "RT TEST") # Perhaps if you can turn off groups

# test drl layout with different parameter settings:
# drl_defaults$default drl_defaults$coarsen drl_defaults$coarsest drl_defaults$refine drl_defaults$final
plot_graph_local(g, "/tmp/vis_drl_coarsen.png", "DRL TEST (coarsen)")
plot_graph_local(g, "/tmp/vis_drl_coarsest.png", "DRL TEST (coarsest)")
plot_graph_local(g, "/tmp/vis_drl_refine.png", "DRL TEST (refine)")
plot_graph_local(g, "/tmp/vis_drl_final.png", "DRL TEST (final)")

# DRL, MDS, and Radial Tree seem like the best graphs to pursue





