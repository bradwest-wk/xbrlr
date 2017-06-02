# ------- Functions for Creating Edgelist directly from xls format ------- #
# The current taxonomies can be located on the xbrl website:
# https://xbrl.us/xbrl-taxonomy/2017-us-gaap/ (for example) and downloaded
# in xls format (e.g. Taxonomy_2017Amended.xlsx).  This document contains
# both the calculation and presentation linkbases (relationships), and 
# all GAAP elements (Also, contains a number of other sheets that I
# do not understand).  The calculation and presentation links contain 
# relationships (element names and depths) for elements in a given
# statement (e.g. statement of income). We can use that relationship
# information to create network trees (essentially flow charts) of the
# taxonomy.

library(igraph) # Graph data structure
library(readxl) # load in taxonomy
library(stringr) # string manipulation
library(RColorBrewer) # colors

# function for reading in Calculation of Presentation Link
choose_linkbase <- function(linkbase = "Calculation"){
    if(linkbase=="Presentation"){
        2
    } else {
        3
    }
}

taxonomy <- read_excel("~/Google_Drive/Projects/taxonomy_rebuild/Taxonomy_2017Amended.xlsx",
                       sheet = choose_linkbase("Calculation"), skip = 0)

# Ideally the statment argument would be in the format on
# http://xbrlview.fasb.org/yeti/resources/yeti-gwt/Yeti.jsp#tax~(id~161*v~5016)!net~(a~3191*l~776)!lang~(code~en-us)!rg~(rg~32*p~12)
# like '124000 - Statement - Statement of Income
find_statement <- function(statement = "Statement of Income (Including Gross Margin)", taxonomy, linkbase = "Calculation"){
    start_index <- grep(statement, taxonomy$name, fixed = TRUE) + 2
    if(length(start_index) > 1){
        stop("More than one statment of that name")
    } else if (length(start_index) == 0){
        stop("No statement found")
    }
    # Find next taxonomy
    end_index <- subset(which(taxonomy$prefix=="LinkRole"), which(taxonomy$prefix=="LinkRole") > start_index)[1] - 1
    if(linkbase == "Calculation"){
        columns_of_interest <- c(2,3,4,7,8)
        column_names <- c("child", "label", "depth", "weight", "parent")
    } else {
        columns_of_interest <- c(2, 3, 4, 7)
        column_names <- c("child", "label", "depth", "parent")
    }
    # Extract statement
    statement_of_interest <- taxonomy[start_index:end_index, columns_of_interest]
    colnames(statement_of_interest) <- column_names
    if ("weight" %in% colnames(statement_of_interest)){
        statement_of_interest$weight <- as.integer(statement_of_interest$weight)
    }
    statement_of_interest$depth <- as.integer(statement_of_interest$depth)
    # remove prefix from parent
    statement_of_interest$parent <- str_replace(statement_of_interest$parent, "us-gaap:", "")
    return(statement_of_interest)
}

create_graph <- function(statement_of_interest = statement, root_nodes = NA){
    colors <- brewer.pal(n = 3, name = "Set2")
    edges <- cbind(statement_of_interest$parent, statement_of_interest$child)
    edges <- edges[-which(is.na(edges[,1])),]
    # make graph
    g <- graph_from_edgelist(edges, directed = T)
    roots <- which(degree(g, v = V(g), mode = "in")==0)
    # Add weight and depth attribute to graph
    if("weight" %in% colnames(statement_of_interest)){
        V(g)$weight <- statement_of_interest$weight[which(V(g)$name==statement_of_interest$child)]
    }
    V(g)$depth <- statement_of_interest$depth[which(V(g)$name==statement_of_interest$child)]
    V(g)$label <- statement_of_interest$label[which(V(g)$name==statement_of_interest$child)]
    
    # Add Color
    # darkgrey <- col2rgb("darkgrey")
    # SkyBlue2 <- col2rgb("SkyBlue2")
    # E(g)$color <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 250, max = 255)
    # V(g)$color <- rgb(SkyBlue2[1,1], SkyBlue2[2,1], SkyBlue2[3,1], alpha = 200, max = 255)
    E(g)$color <- colors[1]
    V(g)$color <- colors[3]
    
    # Edge color goes to green if the relationship is a subtraction
    E(g)[which(tail_of(g, E(g))$weight == -1)]$color <- colors[2]
    
    V(g)$label.cex <- 1.25
    V(g)$label.cex[roots] <- 2
    if (!is.na(root_nodes) && (root_nodes %in% V(g)$name)){
        V(g)$label.cex[root_nodes] <- 2.5
    }

    return(g)
}

plot_graph <- function(g = g, filename = "./statement.png", title = "2017 XBRL Tree"){
    darkgrey <- col2rgb("darkgrey")
    framecolor <- rgb(darkgrey[1,1], darkgrey[2,1], darkgrey[3,1], alpha = 75, max = 255)
    roots <- which(degree(g, v = V(g), mode = "in")==0)
    png(filename = filename, width = 5760, height = 5760, res = 100)
    par(cex = 1, cex.main = 5, cex.sub = 4, srt = 270, mar = c(20,4,10,4), font.lab = 2)
    plot(g, layout = layout_as_tree(g, root = roots, rootlevel = c(rep(1, length(roots)))),
         vertex.label.cex = V(g)$label.cex, vertex.label = V(g)$name, vertex.color = V(g)$color,
         vertex.frame.color = framecolor, vertex.shape = "circle", vertex.size = 1,
         vertex.label.dist = .1, vertex.label.degree = pi/2, vertex.label.color = "black",
         edge.arrow.size = .4, edge.arrow.width = 1, asp = 0, edge.curved = F,
         edge.color = E(g)$color)
    text(x = 1.05, y = 0, labels = title,
         srt = 270, font = 2, cex = 5)
    # legend(x = "topleft", c("Addition", "Subtraction"), lty = 1, lwd = 2,
    #        col = c(brewer.pal(n = 3, name = "Set2")[1], brewer.pal(n = 3, name = "Set2")[2]),
    #        cex = 5)
    dev.off()
}

create_statement_tree <- function(taxonomy_path = "~/Google_Drive/Projects/taxonomy_rebuild/Taxonomy_2017Amended.xlsx",
                                  linkbase = "Calculation", statement = "124000 - Statement - Statement of Income (Including Gross Margin)",
                                  title = "2017 Income Statement (Including Gross Margin) Calculation Link",
                                  image_path = "~/Google_Drive/Projects/taxonomy_rebuild/income_statement_including_gross_margin.png"){
    taxonomy <- read_excel(taxonomy_path, sheet = choose_linkbase(linkbase), skip = 0)
    statement_of_interest <- find_statement(statement = statement, taxonomy = taxonomy, linkbase = linkbase)
    g <- create_graph(statement_of_interest)
    plot_graph(g = g, filename = image_path, title = title)
}

extract_subgraph <- function(graph = g, element_of_interest = "Revenues"){
    sub_graph_vertices <- subcomponent(graph, which(V(graph)$name == element_of_interest), mode = "out")
    sub_graph <- induced_subgraph(graph, sub_graph_vertices)
}

# visualize the differences between two edgelists
# returns graph with pinkish nodes for those not in other statement
visualize_differences <- function(edgelist_large, edgelist_small){
    colors <- brewer.pal(4, "Set2")
    g_large <- create_graph(edgelist_large)
    g_small <- create_graph(edgelist_small)
    
    V(g_large)$color[which(!(V(g_large)$name %in% V(g)$name))] <- colors[4]
    E(g_large)$color[which(!E(g_large) %in% E(g))] <- colors[4]
    
    return(g_large)
}




