# Generates a png image showing all the independent root nodes in the current
# calculation taxonomy (http://xbrlview.fasb.org/yeti/resources/yeti-gwt/Yeti.jsp#tax~(id~161*v~5016)!net~(a~3191*l~776)!lang~(code~en-us)!rg~(rg~32*p~12))

library(tidyverse)
source("./R/xbrl_taxonomy_functions.R")
source("./R/tree_panel_fncts.R")

# Load from Taxonomy_2017Amended.xlsx
#'
calc_link <-
    excel_to_df("./inst/extdata/Taxonomy_2017Amended.xlsx", "Calculation")
statement_names <- calc_link[calc_link$prefix=="Definition",]
statement_names <- statement_names[!is.na(statement_names$prefix),] %>%
    select(name)
statement_names <- statement_names$name

# create a list of statements and their associated igraphs
graphs <- vector("list", length(statement_names))
for (i in 1:length(statement_names)) {
        df <- find_statement(statement_names[i], calc_link, "Calculation")
        graphs[[i]] <- create_graph(df)
}

# plot statements and the disclosures
stat_nums <- which(substr(statement_names, 10, 18)=="Statement")
panel_trees(graphs[stat_nums],
            "./inst/vis/stmnt_nodes_2017.png",
            main_title = "2017 Statements: Calculation Link",
            graph_titles = statement_names[stat_nums],
            rows = 4,
            cols = 5)
panel_trees(graphs[-stat_nums],
            "./inst/vis/disclsr_nodes_2017.png",
            main_title = "2017 Disclosures: Calculation Link",
            graph_titles = statement_names[-stat_nums],
            rows = 7,
            cols = 8)


