library(shiny)

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
                       selected = FALSE)
          ),

      mainPanel(
          # height fills 15in retina screen
          plotOutput("tree", width="1300px", height = "1200px")
      )
   )
)


server <- function(input, output) {
    output$tree <- renderPlot({
        pretty_tree_graph(input$statement, calc_link_dirty, input$link,
                          names = input$names)
    })
}

# Run the application
shinyApp(ui = ui, server = server)

