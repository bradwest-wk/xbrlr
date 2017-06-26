# Functions for Creating Edgelists directly from xls format -------------------
# The current taxonomies can be located on the xbrl website:
# https://xbrl.us/xbrl-taxonomy/2017-us-gaap/ (for example) and downloaded
# in xls format (e.g. Taxonomy_2017Amended.xlsx). These xlsx documents contain
# both the calculation and presentation linkbases (relationships), and
# all GAAP elements, in addition to other sheets.
# The calculation and presentation links contain relationships (i.e. arcroles)
# for elements in a given statement (e.g. statement of income). We can use that
# relationship information (specifically via the depth/level of the element) to
# create network trees (essentially flow charts) of the taxonomy.


# Functions --------------------------------------------------------------------


#' Choose Linkbase
#'
#' A method for choosing the correct Calculation or Presentation linkbase sheet
#' for importing taxonomy relationships.
#'
#' @export
#' @param linkbase Linkbase to choose, either Calculation of Presentation
#' @return Integer value of the excel sheet that needs to be read in
#' @examples
#' choose_linkbase("Calculation")
choose_linkbase <- function(linkbase){
    if(linkbase=="Presentation"){
        2
    } else {
        3
    }
}


#' Excel Sheet to dataframe
#'
#' A method for extracting the correct linkbase from excel sheet and reading in
#' as a dataframe.
#'
#' @export
#' @param path Path to xls/xlsx file
#' @param linkbase Linkbase to choose, either Calculation of Presentation
#' @return dataframe of excel sheet
excel_to_df <- function(path, linkbase){
    readxl::read_excel(path, sheet = choose_linkbase(linkbase), skip = 0)
}


#' Extract Financial Statement of Interest
#'
#' A function for extracting the financial statement of interest from the given
#' taxonomy.
#'
#' @export
#' @param statement Title of statement of interest as string. Incorrect or vague
#'   values will throw error. Ideally the statement argument would be in the
#'   format on
#'   http://xbrlview.fasb.org/yeti/resources/yeti-gwt/Yeti.jsp#tax~(id~161*v~5016)!net~(a~3191*l~776)!lang~(code~en-us)!rg~(rg~32*p~12)
#'    like like '124000 - Statement - Statement of Income.
#' @param taxonomy dataframe of the taxonomy of interest
#' @param linkbase The linkbase to choose, either Calculation or Presentation
#' @return The statement of interest with each row as an element, the
#'   corresponding parent, the element label, depth, and if calculation linkbase
#'   the weight
find_statement <- function(statement, taxonomy, linkbase){
    start_index <- which(taxonomy$name==statement) + 2
    # start_index <- grep(statement, taxonomy$name, fixed = TRUE) + 2
    if(length(start_index) > 1){
        stop("More than one statment of that name")
    } else if (length(start_index) == 0){
        stop("No statement found")
    }
    # Find next taxonomy
    end_index <-
        subset(which(
            taxonomy$prefix=="LinkRole"),
            which(taxonomy$prefix=="LinkRole") > start_index)[1] - 1

    if (is.na(end_index)){
        end_index <- nrow(taxonomy)
    }

    if(linkbase == "Calculation"){
        columns_of_interest <- c(2,3,4,7,8)
        column_names <- c("child", "label", "depth", "weight", "parent")
    } else {
        columns_of_interest <- c(2, 3, 4, 7)
        column_names <- c("child", "label", "depth", "parent")
    }
    # Extract statement
    statement_of_interest <- taxonomy[
        start_index:end_index, columns_of_interest]
    statement_of_interest <-
        statement_of_interest[
            rowSums(is.na(statement_of_interest)) !=
                ncol(statement_of_interest),]
    colnames(statement_of_interest) <- column_names
    if ("weight" %in% colnames(statement_of_interest)){
        statement_of_interest$weight <- as.integer(
            statement_of_interest$weight)
    }
    statement_of_interest$depth <- as.integer(statement_of_interest$depth)
    # remove prefix from parent
    statement_of_interest$parent <- sub("us-gaap:", "",
                                        statement_of_interest$parent)
    statement_of_interest <-
        statement_of_interest[!duplicated(statement_of_interest$child),]
    return(statement_of_interest)
}


#' Create Network Graph
#'
#' A function for creating a network graph from a statement of interest in the
#' form of a dataframe.
#'
#' @export
#' @param statement_of_interest A dataframe of the statement of interest, often
#'   generated from a cal to find_statement. Must have the column names parent
#'   and child.
#' @param root_nodes A character vector of element names to place at the highest
#'   level of the graph. This function increases the root_nodes display size.
#' @return An igraph object from the list of edges in the statement dataframe
create_graph <- function(statement_of_interest = statement, root_nodes = NA){
    colors <- RColorBrewer::brewer.pal(n = 3, name = "Set2")
    edges <- cbind(statement_of_interest$parent, statement_of_interest$child)
    edges <- edges[-which(is.na(edges[,1])),]
    # make graph
    g <- igraph::graph_from_edgelist(edges, directed = T)
    roots <- which(igraph::degree(g, v = igraph::V(g), mode = "in")==0)
    # Add weight and depth attribute to graph
    if("weight" %in% colnames(statement_of_interest)){
        igraph::V(g)$weight <- dplyr::left_join(
            as.data.frame(igraph::V(g)$name, stringsAsFactors = FALSE),
            statement_of_interest[,c("child", "weight")],
            by = c("igraph::V(g)$name"="child"))$weight
    }

    igraph::V(g)$depth <- dplyr::left_join(
        as.data.frame(igraph::V(g)$name, stringsAsFactors = FALSE),
        statement_of_interest[,c("child", "depth")],
        by = c("igraph::V(g)$name"="child"))$depth
    igraph::V(g)$label <- dplyr::left_join(
        as.data.frame(igraph::V(g)$name, stringsAsFactors = FALSE),
        statement_of_interest[,c("child", "label")],
        by = c("igraph::V(g)$name"="child"))$label


    igraph::E(g)$color <- colors[1]
    igraph::V(g)$color <- colors[3]

    # Edge color goes to green if the relationship is a subtraction
    igraph::E(g)[which(igraph::tail_of(
        g, igraph::E(g))$weight == -1)]$color <- colors[2]

    igraph::V(g)$label.cex <- 1.25
    igraph::V(g)$label.cex[roots] <- 2
    if (!is.na(root_nodes) && (root_nodes %in% igraph::V(g)$name)){
        igraph::V(g)$label.cex[root_nodes] <- 2.5
    }
    return(g)
}


#' Plot Network Graph
#'
#' Saves the network graph in a tree layout as a png of a large size (5760x5760
#' pixels)
#'
#' @export
#' @param g Graph to visualize
#' @param filename Output filename
#' @param title Title of plot
#' @return The plot, in png format
plot_graph <- function(g, filename, title){
    darkgrey <- col2rgb("darkgrey")
    framecolor <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 75,
                      max = 255)
    roots <- which(igraph::degree(g, v = igraph::V(g), mode = "in")==0)
    png(filename = filename, width = 5760, height = 5760, res = 100)
    par(cex = 1, cex.main = 5, cex.sub = 4, srt = 270, mar = c(20,4,10,4),
        font.lab = 2)
    igraph::plot.igraph(g, layout = igraph::layout_as_tree(
        g, root = roots, rootlevel = c(rep(1, length(roots)))),
        vertex.label.cex = igraph::V(g)$label.cex, vertex.label
        = igraph::V(g)$name, vertex.color = igraph::V(g)$color,
        vertex.frame.color = framecolor, vertex.shape = "circle",
        vertex.size = 1,
        vertex.label.dist = .1, vertex.label.degree = pi/2,
        vertex.label.color = "black",
         edge.arrow.size = .4, edge.arrow.width = 1, asp = 0, edge.curved = F,
         edge.color = igraph::E(g)$color)
    text(x = 1.05, y = 0, labels = title,
         srt = 270, font = 2, cex = 5)
    # legend(x = "topleft", c("Addition", "Subtraction"), lty = 1, lwd = 2, col
    # = c(brewer.pal(n = 3, name = "Set2")[1], brewer.pal(n = 3, name =
    # "Set2")[2]), cex = 5)
    dev.off()
}


#' Create Statement Tree
#'
#' Creates statement tree from scratch excel file
#'
#' @export
#' @param taxonomy_path Path to xls/xlsx file
#' @param linkbase Linkbase to choose, either Calculation of Presentation
#' @param statement Title of statement of interest as string. Incorrect or vague
#'   values will throw error. Ideally the statement argument would be in the
#'   format on
#'   http://xbrlview.fasb.org/yeti/resources/yeti-gwt/Yeti.jsp#tax~(id~161*v~5016)!net~(a~3191*l~776)!lang~(code~en-us)!rg~(rg~32*p~12)
#'    like like '124000 - Statement - Statement of Income.
#' @param title Title of plot
#' @param image_path Output filename
#' @return Plot, in png format
create_statement_tree <- function(taxonomy_path, linkbase, statement, title,
                                  image_path){
    taxonomy <- excel_to_df(taxonomy_path, linkbase)
    statement_of_interest <- find_statement(statement = statement,
                                            taxonomy = taxonomy,
                                            linkbase = linkbase)
    graph <- create_graph(statement_of_interest)
    plot_graph(g = graph, filename = image_path, title = title)
}


#' Create Subgraph from Root Node
#'
#' Extracts a subgraph from a larger graph by grabbing children elements
#' (recursively) of an element of interest.
#'
#' @export
#' @param graph The parent graph from which to extract a subgraph
#' @param element_of_interest Root node of returned graph as a string. (e.g.
#'   "Revenues")
#' @return A subgraph with root \code{element_of_interest}
extract_subgraph <- function(graph = g, element_of_interest){
    sub_graph_vertices <- igraph::subcomponent(graph, which(
        igraph::V(graph)$name == element_of_interest), mode = "out")
    sub_graph <- igraph::induced_subgraph(graph, sub_graph_vertices)
}


#' Visualize Differences Between Graphs
#'
#' Visualize the differences between two edgelists
#'
#' @export
#' @param edgelist_large Larger edgelist
#' @param edgelist_small smaller edgelist.  Only those elements that are not in
#'   the smaller edgelist will be highlighted
#' @return returns graph with pinkish nodes for those not in other statement
visualize_differences <- function(edgelist_large, edgelist_small){
    colors <- brewer.pal(4, "Set2")
    g_large <- create_graph(edgelist_large)
    g_small <- create_graph(edgelist_small)

    V(g_large)$color[which(!(V(g_large)$name %in% V(g)$name))] <- colors[4]
    E(g_large)$color[which(!E(g_large) %in% E(g))] <- colors[4]

    return(g_large)
}




