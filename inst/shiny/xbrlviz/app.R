library(shiny)
library(xbrlr)
library(scales)
library(DT)

# To Do:
# 2. Fix datafrome issue
# -- look at cbind output vs left join/merge


source("./side_bar.R")
source("./pretty_tree_graph.R")
source("./layout_rescale.R")

# calculation linkbase
# in the future will need to write if else statement for loading data depending
# on whether Presentation or Calculation is selected. Or have ifelse statement
# in the create sidebar
load("./data/calc_link_dirty.Rdata")
load("./data/present_link_dirty.Rdata")

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
                           "Calculation",
                           "Presentation"
                           ),
                       selected = "Calculation"),
          uiOutput("selectBar"),
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
          DT::dataTableOutput("plot_brushed_points"),
          verbatimTextOutput("dataset")
      )
   )
)


server <- function(input, output, session) {

    ranges <- reactiveValues(x = c(-1,1), y = c(-1,1))

    datasetInput <- reactive({
        req(input$link)
        switch(input$link,
               "Calculation" = calc_link_dirty,
               "Presentation" = present_link_dirty)
    })

    output$selectBar <- renderUI({
        req(input$link)
        selectInput(
            "statement", "Statement:",
            choices = get_stmt_names(datasetInput(), input$link),
            multiple = FALSE
        )
    })

    output$dataset <- renderPrint({
        req(input$statement, input$link)
        nearPoints(rescale_layout(input$statement, datasetInput(),
                       input$link),
                   input$plot_click,
                   xvar = "x", yvar = "y",
                   threshold = 10)
        })

    output$tree <- renderPlot({
        req(input$statement)
        zoom_level <- 2 / (ranges$x[2] - ranges$x[1])
        pretty_tree_graph(input$statement, datasetInput(), input$link,
                          names = input$names,
                          xlim = ranges$x,
                          ylim = ranges$y,
                          zoom = zoom_level)
    })

    output$plot_clicked_points <- DT::renderDataTable({
        coord_df <- rescale_layout(input$statement, datasetInput(),
                                   input$link)
        res <- nearPoints(
            coord_df, input$plot_click,
            xvar = "x", yvar = "y",
            threshold = 10)[, c(4:ncol(coord_df))]
        datatable(res,
                  colnames = if (input$link=="Calculation"){
                      c("Element Label",
                        "Depth From Root",
                        "Add/Subtract",
                        "Parent Element Name")
                  } else if (input$link=="Presentation"){
                      c("Element Label",
                        "Depth From Root",
                        "Parent Element Name")
                  })

    })

    output$plot_brushed_points <- DT::renderDataTable({
        coord_df <- rescale_layout(input$statement, datasetInput(),
                                   input$link)
        res <- brushedPoints(
            coord_df, input$plot_brush,
            xvar = "x", yvar = "y")[, c(4:ncol(coord_df))]
        datatable(res,
                  colnames = if (input$link=="Calculation"){
                      c("Element Label",
                        "Depth From Root",
                        "Add/Subtract",
                        "Parent Element Name")
                  } else if (input$link=="Presentation"){
                      c("Element Label",
                        "Depth From Root",
                        "Parent Element Name")
                  })
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

