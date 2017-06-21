library(shiny)
library(xbrlr)
library(scales)

source("./side_bar.R")
source("./pretty_tree_graph.R")

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
                       selected = FALSE),
          verbatimTextOutput("info")
          ),

      mainPanel(
          # height fills 15in retina screen
          plotOutput("tree", width="1300px", height = "1200px",
                     click = "plot_click",
                     brush = "plot_brush")
      )
   )
)


server <- function(input, output) {
    output$tree <- renderPlot({
        pretty_tree_graph(input$statement, calc_link_dirty, input$link,
                          names = input$names)
    })

    # get the layout of the graph
    output$info <- renderPrint({
        stmt_of_interest <- find_statement(
            input$statement, calc_link_dirty, input$link)
        graph <- create_graph(stmt_of_interest)
        l = igraph::layout_as_tree(graph)
        # need to rescale to -1 to 1 plotting region
        x <- rescale(l[,1], to = c(-1,1))
        y <- rescale(l[,2], to = c(-1,1))
        # bind the coordinates to the statement
        coord_df <- cbind(x,y,stmt_of_interest)
        nearPoints(coord_df, input$plot_click, xvar = "x", yvar = "y")
        brushedPoints(coord_df, input$plot_brush, xvar = "x", yvar = "y")
    })
}

# Run the application
shinyApp(ui = ui, server = server)

