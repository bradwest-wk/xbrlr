# server for app tab: whole taxonomy

# Load layouts and nodes
load(file = "./data/nodes_2017_stmts_only_calc.Rdata")
load(file = "./data/edges_2017_stmts_only_calc.Rdata")
load(file = "./data/l_drl_2017_stmts_only_calc.Rdata")
load(file = "./data/l_mds_2017_stmts_only_calc.Rdata")
load(file = "./data/l_rt_2017_stmts_only_calc.Rdata")
source("./whole_tx.R")

output$whole_tx_instructions <-
    renderText("This tab attempts to visualize the 2017 US GAAP taxonomy.
At this time, only the calculation link is valid and we have excluded
Disclosures.  Note that the layout algorithm dramatically changes the
visualization of the data. We must stress that the data remain the same
regardless of the algorithm chosen. Clicking on a node will highlight it and
its nearest neighbors. Use the dropdowns to select individual nodes or nodes by
statement.  The user can drag nodes, and can zoom with mouse scrolling. Nodes
are currently size scaled based on how many statements they appear in.  Node
shapes are tied to certain statements, but this has not been expanded to all
statements. Finally, this app attempts to visualize over 4000 nodes. Be patient,
but if something seems broken, don't hesitate to contact brad dot west at
workiva dot com.")

layoutInput <- reactive({
    req(input$link2, input$layout_algo)
    switch(input$layout_algo,
           "l_rt" = l_rt,
           "l_mds" = l_mds,
           "l_drl" = l_drl)
})

titleInput <- reactive({
    req(input$link2, input$layout_algo)
    switch(input$layout_algo,
           "l_rt" = "2017 Taxonomy - Radial Tree Algorithm",
           "l_mds" = "2017 Taxonomy - Multidimensional Scaling Algorithm",
           "l_drl" = "2017 Taxonomy - DrL Algorithm")
})

output$network <- renderVisNetwork({
    req(input$link2, input$layout_algo)

    # get unique groups
    the_groups <- unique_groups(nodes$group)

    # build the taxonomy
    visNetwork(nodes, edges, main = titleInput()) %>%
        visEdges(width = 0.1, arrow = "to", arrowStrikethrough = FALSE) %>%
        visNodes(label = NULL, font = list(size = 0)) %>%
        visOptions(selectedBy = list(variable = "group", multiple = TRUE),
                   highlightNearest = list(enabled = T, degree = 2, hover = F),
                   nodesIdSelection = list(enabled = T, useLabels = T)) %>%
        visIgraphLayout(layout = "layout.norm", randomSeed = 222,
                        smooth = TRUE, layoutMatrix = layoutInput()) %>%
        visGroups(groupname = the_groups[8], shape = "triangle") %>%
        visGroups(groupname = the_groups[6], shape = "square") %>%
        visGroups(groupname = the_groups[11], shape = "diamond") %>%
        visInteraction(multiselect = TRUE)
})
