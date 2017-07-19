# functions for visualizing whole taxonomy

# Set-up: Load data

#' Load Whole Taxonomy
#'
#' Loads the entire taxonomy from a dirty Rdata file. Dirty in the sense that
#' it contains statement names, linkRoles, and prefix headers.
#' @param taxonomy the dirty taxonomy (a dataframe)
#' @param link Calculation or Presentation link c("Calculation", "Presentation")
#' @param sheet_type A string, one of the following:
#' c("Statement", "Disclosure", "both"), specifying whether just statements,
#' just disclosures, or both should be loaded
#' @return A clean dataframe
taxonomy_load <- function(taxonomy, link = "Calculation", sheet_type = "both") {
    if (sheet_type == "both") {
        tmp_stmt <- xbrlr::get_stmt_names(taxonomy, type = "Statement")
        tmp_disclosure <- xbrlr::get_stmt_names(taxonomy, type = "Disclosure")
        stmt_names <- c(tmp_stmt, tmp_disclosure)
    } else {
        stmt_names <- xbrlr::get_stmt_names(taxonomy, type = sheet_type)
    }
    out_df <- xbrlr::find_statement(stmt_names[1], taxonomy, link)
    for (stmt in stmt_names[2:length(stmt_names)]) {
        stmt_of_interest <- xbrlr::find_statement(stmt, taxonomy, link)
        out_df <- dplyr::bind_rows(out_df, stmt_of_interest)
    }
    return(out_df)
}


#' Get Dirty Statements or Disclosures
#'
#' @param taxonomy The dirty taxonomy of interest
#' @param type One of c("Statement", "Disclosure", "both") specifying which
#' sheet is of interest.  If both function returns without performing
#' operations
#' @return a dirty taxonomy with just the sheets of interest
get_desired_sheets <- function(taxonomy, type) {
    if (type == "both") {
        return(taxonomy)
    } else if (type == "Statement") {
        delete <- "Disclosure"
    } else {
        delete <- "Statement"
    }
    nums <- c(which(taxonomy$prefix == "Definition"), nrow(taxonomy)+2)
    for (i in (length(nums)-1):1) {
        if (substr(taxonomy$name[nums[i]], 10, 9+nchar(delete))==delete)
            taxonomy <- taxonomy[-((nums[i]-1):(nums[i+1]-2)),]
    }
    return(taxonomy)
}

# =============================================================================
# Add groups to graph.  A node can have multiple groups if it appears in more
# than one statement

#' Add Group Attribute
#'
#' Adds a group attribute to each graph vertex.  A vertex can be in one or more
#' groups (statements), depending on whether it is in one or more statements
#'
#' @param g The graph with verticies
#' @param dirty_tax The dirty taxonomy
#' @return The graph with the group attribute added
add_groups <- function(g, dirty_tax) {
    dirty_tax <- statement_column(dirty_tax)
    for (i in 1:igraph::gorder(g)) {
        igraph::V(g)$group[i] <-
            paste(dirty_tax$statement[which(
                dirty_tax$name==igraph::V(g)$name[i])],
                  collapse = ", ")
    }
    return(g)
}


#' Helper: Get statement column in dirty taxonomy
#'
#' @param dirty_tax The dirty taxonomy to create the column in
statement_column <- function(dirty_tax) {
    dirty_tax$statement <- rep(NA, nrow(dirty_tax))
    for (i in 2:nrow(dirty_tax)) {
        if (dirty_tax$prefix[i]=="Definition") {
            curr_stmt <- stringr::str_replace(dirty_tax$name[i], ",", "")
        }
        if (dirty_tax$prefix[i]=="us-gaap") {
            dirty_tax$statement[i] <- curr_stmt
        }
    }
    return(dirty_tax)
}

#' Color Nodes based on grpup length
#'
#' @param g A graph with a group vertex attribute
#' @return A graph with an updated color and group attribute based on number of
#' groups
group_count <- function(g) {
    colors <- RColorBrewer::brewer.pal(11, "RdBu")
    for (i in 1:igraph::gorder(g)) {
        igraph::V(g)[i]$group_count <- length(
            stringr::str_split(V(g)[i]$group, ",")[[1]])
        if (igraph::V(g)[i]$group_count == 1) {
            igraph::V(g)[i]$color <- colors[11]
        } else if (igraph::V(g)[i]$group_count == 2) {
            igraph::V(g)[i]$color <- colors[10]
        } else if (igraph::V(g)[i]$group_count == 3) {
            igraph::V(g)[i]$color <- colors[9]
        } else {
            igraph::V(g)[i]$color <- colors[8]
        }
    }
    return(g)
}

# =============================================================================
# Below for using visNet to visualize whole taxonomy

# order nodes
#' Find Permutation Order
#'
#' Find a permutation order for re-ordering the nodes of an igraph object
#' alphabetically (based on their name)
#' @param g An igraph object
#' @return a numeric vector, the permutation, to be passed to permute igraph
#' function in which the first element is the new id of vertex 1, etc.
find_order <- function(g) {
    order <- order(V(g)$name)
    result <- numeric()
    for (i in 1:igraph::gorder(g)) {
        result <- c(result, which(order==i))
    }
    result
}

#' Create Edge ID matrix
#'
#' @param g igraph object
#' @param nodes visNodes dataframe
#' @return the edge dataframe with id's corresponding to the nodes df
get_edge_df <- function(g, nodes) {
    named_edges <- as.data.frame(
        igraph::get.edgelist(g), stringsAsFactors = FALSE)
    from <- c()
    to <- c()
    color <- c()
    width <- rep(-1, nrow(named_edges))
    physics <- rep(FALSE, nrow(named_edges))
    dashes <- ifelse(igraph::V(g)$weight==1, FALSE, TRUE)
    selectionWidth <- rep(0.5, nrow(named_edges))
    for (i in 1:nrow(named_edges)) {
        from <- c(from, nodes$id[which(nodes$title==named_edges$V1[i])])
        to <- c(to, nodes$id[which(nodes$title==named_edges$V2[i])])
        color <- c(color, igraph::E(g)[i]$color)
    }
    edges <- cbind.data.frame(from, to, color, width, physics)
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
        all <- c(all, stringr::str_split(el, ", ")[[1]])
    }
    unique(all)
}
