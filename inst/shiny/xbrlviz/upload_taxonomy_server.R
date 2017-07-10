# creates server side for app tab: upload and visualize custom taxonomy

output$instructions2 <-
    renderText("Upload an excel file in the below format. Contact brad dot west at workiva dot com with bug reports or questions.
 ________________________________________________
| PARENT      | CHILD     | REFERENCE   | WEIGHT |
| ----------- | --------- | ----------- | ------ |
| parent_1    | child_1   | S-X Rule ** | 1      |
|             | child_2   | ...         | ...    |
| parent_2    | child_1   | ...         | ...    |
|             | child_2   | ...         | ...    |
|             | child_3   | ...         | ...    |
| ...         | ...       | ...         | ...    |"
               )

uploadInput <- reactive({
    req(input$input_file, input$link2)
    inFile <- input$input_file
    # need to rename the file so that read_excel works
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
     raw <- readxl::read_excel(paste(inFile$datapath, ".xlsx", sep=""),
                               sheet = 1, col_names = TRUE)
     if (input$link2 == "Calculation") {
         data <- raw[,c(1,2,4)]
         colnames(data) <- c("parent", "child", "weight")
     } else {
         data <- raw[,c(1,2)]
         colnames(data) <- c("parent", "child")
     }

     data <- tidyr::fill(data, parent)
     if ( sum(duplicated(data)) > 0 ) {
         warning("Duplicated edges in dataframe--investigate further.")
     }
     data
})

ranges2 <- reactiveValues(x = c(-1,1), y = c(-1,1))

# Creates a basic graph with correct theme colors
basic_graph <- function(edgelist){
    g <- igraph::graph_from_edgelist(as.matrix(
        edgelist[,c("parent", "child")]), directed = T)
    colors <- RColorBrewer::brewer.pal(n = 3, name = "Set2")
    if("weight" %in% colnames(edgelist)){
        igraph::V(g)$weight <- dplyr::left_join(
            as.data.frame(igraph::V(g)$name, stringsAsFactors = FALSE),
            edgelist[,c("child", "weight")],
            by = c("igraph::V(g)$name"="child"))$weight
    }
    # set Vertex and Edge color attributes
    darkgrey <- col2rgb("darkgrey")
    igraph::E(g)$color <- colors[1]
    igraph::V(g)$color <- colors[3]
    igraph::E(g)[which(igraph::tail_of(
        g, igraph::E(g))$weight == -1)]$color <- colors[2]
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


