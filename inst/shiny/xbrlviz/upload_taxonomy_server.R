# creates server side for app tab: upload and visualize custom taxonomy

output$instructions2 <-
    renderText("Upload a csv file in following format:
               | PARENT      | CHILD     |
               | parent_1    | child_1   |
               | parent_1    | child_2   |
               | parent_2    | child_1   |
               | parent_2    | child_2   |
               | parent_2    | child_3   |
               | ...         | ...       |"
               )

uploadInput <- reactive({
    req(input$input_file)
    inFile <- input$input_file
    # xbrlr:::edge_to_df(inFile$datapath)
     data <- read.csv(inFile$datapath)
     data
})

ranges2 <- reactiveValues(x = c(-1,1), y = c(-1,1))

# Creates a basic graph with correct theme colors
basic_graph <- function(edgelist){
    g <- igraph::graph_from_edgelist(as.matrix(edgelist), directed = T)
    colors <- RColorBrewer::brewer.pal(n = 3, name = "Set2")
    # set Vertex and Edge color attributes
    darkgrey <- col2rgb("darkgrey")
    igraph::E(g)$color <- colors[1]
    igraph::V(g)$color <- colors[3]
    return(g)
}

# output$fname <-
#     renderPrint(
#         print(input$input_file$datapath)
#     )

# output$df <-
#     renderPrint(
#         head(uploadInput)
#     )

output$uploadTree <- renderPlot({
    req(input$input_file)
    zoom_level <- 2 / (ranges2$x[2] - ranges2$x[1])
    # need to call reactive uploadInput function below
    g <- basic_graph(uploadInput())
    par(srt = 290, adj = 0)
    xbrlr:::plot_single_graph(g, title = input$input_file$name, title_size = 2,
                              display_names = input$names2,
                              xlimit = ranges2$x,
                              ylimit = ranges2$y,
                              zoom = zoom_level,
                              legend = FALSE)
})

observeEvent(input$plot_dblclick2, {
    brush2 <- input$plot_brush2
    if (!is.null(brush2)) {
        ranges2$x <- c(brush2$xmin, brush2$xmax)
        ranges2$y <- c(brush2$ymin, brush2$ymax)

    } else {
        ranges2$x <- c(-1,1)
        ranges2$y <- c(-1,1)
    }
})


