# Generates a png image showing all the independent root nodes in the current
# calculation taxonomy (http://xbrlview.fasb.org/yeti/resources/yeti-gwt/Yeti.jsp#tax~(id~161*v~5016)!net~(a~3191*l~776)!lang~(code~en-us)!rg~(rg~32*p~12))

library(tidyverse)
source("./R/xbrl_taxonomy_functions.R")

# Load from Taxonomy_2017Amended.xlsx
calc_link <-
    excel_to_df("./inst/extdata/Taxonomy_2017Amended.xlsx", "Calculation")
statement_names <- calc_link[calc_link$prefix=="Definition",]
statement_names <- statement_names[!is.na(statement_names$prefix),] %>%
    select(name)
statement_names <- statement_names$name

# create a list of statements and their associated igraphs
statements <- vector("list", length(statement_names))
graphs <- vector("list", length(statement_names))
for (i in 1:length(statement_names)) {
    # print(statement)
        df <- find_statement(statement_names[i], calc_link, "Calculation")
        statements[[i]] <- df
        graphs[[i]] <- create_graph(df)
}

#' Plot Multiple Graphs
#'
#' Method for plotting panels of statements
#'
#' @param graphs a list or vector of graphs to plot
#' @param out_file the output file to save to
#' @param main_title the overall title of the graph
#' @param graph_titles a parallel vector of titles whose indices correspond to
#' @param rows The number of rows
#' @param cols The number of columns
#' the graph vector.
panel_trees <- function(graphs, out_file, main_title, graph_titles, rows, cols){
    png(out_file, width = 1920*cols,
        height = 1920*rows)
    par(mfrow=c(rows, cols),
        mar = c(10,10,15,10),
        oma = c(10, 10, 40, 10), srt = -15)
    for (i in 1:length(graphs)) {
        g <- graphs[[i]]
        darkgrey <- col2rgb("darkgrey")
        framecolor <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1],
                          alpha = 75,
                          max = 255)
        roots <- which(igraph::degree(g, v = igraph::V(g), mode = "in")==0)
        igraph::plot.igraph(g, layout = igraph::layout_as_tree(
            g, root = roots, rootlevel = c(rep(1, length(roots)))),
            vertex.color = igraph::V(g)$color,
            vertex.label = NA,
                # ifelse(igraph::V(g)$depth<1, igraph::V(g)$name, NA),
            vertex.frame.color = framecolor, vertex.shape = "circle",
            vertex.size = 3,
            vertex.label.dist = 0, vertex.label.degree = pi/2,
            vertex.label.color = "black",
            edge.arrow.size = .5, edge.arrow.width = 2, asp = 0,
            edge.curved = F,
            edge.color = igraph::E(g)$color,
            edge.width = 2)
        title(main = graph_titles[i], cex.main = 5)
        box(which = "plot", lwd=4)
    }
    title(main = main_title, cex.main = 20, outer = TRUE)
    dev.off()
}



stat_nums <- which(substr(statement_names, 10, 18)=="Statement")
panel_trees(graphs[stat_nums],
            "./inst/vis/discon_nodes_2017.png",
            main_title = "2017 Statements: Calculation Link",
            graph_titles = statement_names[stat_nums],
            rows = 4,
            cols = 5)
panel_trees(graphs[-stat_nums],
            "./inst/vis/discon_nodes_2017_disc.png",
            main_title = "2017 Disclosures: Calculation Link",
            graph_titles = statement_names[-stat_nums],
            rows = 7,
            cols = 8)


