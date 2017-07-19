library(shiny)
library(xbrlr)
library(scales)
library(DT)
library(visNetwork)

# To Do:
# 1. Reset double click on switching input$link
# 2. legend in plot

# source("./side_bar.R")
source("./pretty_tree_graph.R")
source("./layout_rescale.R")

# calculation linkbase
# in the future will need to write if else statement for loading data depending
# on whether Presentation or Calculation is selected. Or have ifelse statement
# in the create sidebar
load("./data/calc_link_dirty.Rdata")
load("./data/present_link_dirty.Rdata")

# Define UI for application that draws a histogram
ui <- navbarPage("XBRL Taxonomy Viewer",
                 tabPanel("2017 Taxonomy",
        fluidPage(
        verticalLayout(
            titlePanel("XBRL Viewer: 2017 Taxonomy"),
            wellPanel(
                fluidRow(
                    column(1, radioButtons(inputId = "link",
                                           label = "Linkbase",
                                           choices = c(
                                               "Calculation",
                                               "Presentation"
                                           ),
                                           selected = "Calculation")
                    ),
                    column(7, uiOutput("selectBar")
                    ),
                    column(1, radioButtons(
                        inputId = "names", label = "Show Element Names?",
                        choices = c("Yes" = TRUE, "No" = FALSE),
                        selected = FALSE)
                    ),
                    column(3,
                           # h5("Notes", align = "center"),
                           htmlOutput("instructions"))
                )
            ),

            plotOutput("tree", width="100%", height = "900px",
                       click = "plot_click",
                       brush = brushOpts(id = "plot_brush", resetOnNew = TRUE),
                       dblclick = "plot_dblclick"
            ),

            # br(),
            # br(),
            # h3("Clicked Element"),
            # DT::dataTableOutput("plot_clicked_points"),
            # br(),
            h3("Selected Elements", align = "center"),
            DT::dataTableOutput("plot_brushed_points"),
            htmlOutput("credit")
        )
    )
                 ),

    tabPanel("Visualize Custom Taxonomy",
             source("./upload_taxonomy_ui.R", local = TRUE)),

    tabPanel("Whole Taxonomy",
             source("./whole_tx_ui.R", local = TRUE)),

    selected = "Visualize Custom Taxonomy"
)


server <- function(input, output, session) {

    ranges <- reactiveValues(x = c(-1,1), y = c(-1,1))

    output$instructions <-
        renderText("Click and drag to select nodes and view element info below.
                   Double click to zoom on selected nodes. Edges colored
                   according to addition/subtraction relationship. Work in
                   progress. Contact brad dot west at workiva dot com with
                   questions or feature requests.")

    datasetInput <- reactive({
        req(input$link)
        switch(input$link,
               "Calculation" = calc_link_dirty,
               "Presentation" = present_link_dirty)
    })

    # output$fname <-
    #     renderPrint(
    #         print(input$inout_f)
    #     )
    #
    # output$df <-
    #     renderPrint(
    #         head(datasetInput)
    #     )

    output$selectBar <- renderUI({
        req(input$link)
        selectInput(
            "statement", "Statement:",
            choices = get_stmt_names(datasetInput()),
            multiple = FALSE
        )
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

    # output$plot_clicked_points <- DT::renderDataTable({
    #     req(input$statement, input$link)
    #     coord_df <- rescale_layout(input$statement, datasetInput(),
    #                                input$link)
    #     res <- nearPoints(
    #         coord_df, input$plot_click,
    #         xvar = "x", yvar = "y",
    #         threshold = 10)[, c(4:ncol(coord_df))]
    #     datatable(res,
    #               colnames = if (input$link=="Calculation"){
    #                   c("Element Label",
    #                     "Depth From Root",
    #                     "Add/Subtract",
    #                     "Parent Element Name")
    #               } else if (input$link=="Presentation"){
    #                   c("Element Label",
    #                     "Depth From Root",
    #                     "Parent Element Name")
    #               })
    #
    # })

    output$plot_brushed_points <- DT::renderDataTable({
        req(input$statement)
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

    source("./upload_taxonomy_server.R", local = TRUE)

    source("./whole_tx_server.R", local = TRUE)

    # output$credit <- renderText("Work in progress.  Contact brad dot west at
    #                             workiva dot com with questions or feature
    #                             requests")
}

# Run the application
shinyApp(ui = ui, server = server)

