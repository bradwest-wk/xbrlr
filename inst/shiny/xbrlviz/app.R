library(shiny)
library(xbrlr)
library(scales)
library(DT)


source("./side_bar.R")
source("./pretty_tree_graph.R")
source("./layout_rescale.R")

# calculation linkbase
# in the future will need to write if else statement for loading data depending
# on whether Presentation or Calculation is selected. Or have ifelse statement
# in the create sidebar
load("./data/calc_link_dirty.Rdata")

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("XBRL Viewer: 2017 Taxonomy"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
          radioButtons(inputId = "link",
                       label = "Linkbase",
                       choices = c(
                           "Calculation"
                           ),
                       selected = "Calculation"),
          # inputID = "statement"
          create_sidebar(calc_link_dirty, input$link),
          radioButtons(inputId = "names", label = "Show Element Names?",
                       choices = c("Yes" = TRUE, "No" = FALSE),
                       selected = FALSE)
          ),

      mainPanel(
          # height fills 15in retina screen
          plotOutput("tree", width="1300px", height = "800px",
                     click = "plot_click",
                     brush = brushOpts(id = "plot_brush", resetOnNew = TRUE),
                     dblclick = "plot_dblclick"
                     ),
          h3("Clicked Element"),
          DT::dataTableOutput("plot_clicked_points"),
          h3("Selected Elements"),
          DT::dataTableOutput("plot_brushed_points")
      )
   )
)


server <- function(input, output) {

    ranges <- reactiveValues(x = c(-1,1), y = c(-1,1))

    # output$tree <- renderSvgPanZoom({
    #     # plot.window(xlim = if (!is.null(ranges$x)) ranges$x else c(-1,1),
    #     # ylim =  if (!is.null(ranges$y)) ranges$y else c(-1,1))
    #     zoom_level <- 2 / (ranges$x[2] - ranges$x[1])
    #     g <- pretty_tree_graph(input$statement, calc_link_dirty, input$link,
    #                       names = input$names,
    #                       xlim = ranges$x,
    #                       ylim = ranges$y,
    #                       zoom = zoom_level)
    #     svgPanZoom(g, controlIconsEnabled = TRUE)
    # })

    output$tree <- renderPlot({
        # plot.window(xlim = if (!is.null(ranges$x)) ranges$x else c(-1,1),
                    # ylim =  if (!is.null(ranges$y)) ranges$y else c(-1,1))
        zoom_level <- 2 / (ranges$x[2] - ranges$x[1])
        pretty_tree_graph(input$statement, calc_link_dirty, input$link,
                          names = input$names,
                          xlim = ranges$x,
                          ylim = ranges$y,
                          zoom = zoom_level)
    })

    # get the layout of the graph
    # output$info <- renderPrint({
    #     coord_df <- rescale_layout(input$statement, calc_link_dirty,
    #                                input$link)
    #     nearPoints(coord_df, input$plot_click, xvar = "x", yvar = "y")
    # })
    output$plot_clicked_points <- DT::renderDataTable({
        coord_df <- rescale_layout(input$statement, calc_link_dirty,
                                   input$link)
        res <- nearPoints(
            coord_df, input$plot_click, xvar = "x", yvar = "y")[, c(4:7)]
        datatable(res,
                  colnames =
                      c("Element Label",
                        "Depth From Root",
                        "Add/Subtract",
                        "Parent Element Name"))
    })

    output$plot_brushed_points <- DT::renderDataTable({
        coord_df <- rescale_layout(input$statement, calc_link_dirty,
                                   input$link)
        res <- brushedPoints(
            coord_df, input$plot_brush, xvar = "x", yvar = "y")[, c(4:7)]
        datatable(res, colnames =
                      c("Element Label",
                        "Depth From Root",
                        "Add/Subtract",
                        "Parent Element Name"))
    })

    observeEvent(input$plot_dblclick, {
        brush <- input$plot_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)

        } else {
            ranges$x <- c(-1,1)
            ranges$y <- c(-1,1)
        }
    })
}

# Run the application
shinyApp(ui = ui, server = server)

