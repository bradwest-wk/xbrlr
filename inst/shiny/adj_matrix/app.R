library(shiny)
library(magrittr)
library(dplyr)
library(plotly) # loads ggplot2


# loads matrix creation functions
source("./static_matrix_create.R", local = TRUE)
# source("./inst/shiny/adj_matrix/static_matrix_create.R", local = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(
    # plotly object
    plotlyOutput('matrix', width = '100%', height = '900px'),

    absolutePanel(id = 'controls',
                  class = 'panel panel-default',
                  fixed = FALSE,
                  draggable = TRUE,
                  top = 100,
                  left = 15,
                  right = 'auto',
                  bottom = 'auto',
                  width = 340,
                  height = 'auto',

                  h4('Upload Controls'),

                  radioButtons(inputId = 'which_elements',
                               label = 'Upload New Elements or Select from List',
                               choices = c('Upload New' = 'upload',
                                           'Select From List' = 'select'),
                               inline = TRUE,
                               selected = 'select'),

                  conditionalPanel("input.which_elements == 'upload'",
                                   fileInput('excel_file',
                                             label = 'Upload Elements',
                                             placeholder = 'elements.xlsx')
                                   ),

                  conditionalPanel("input.which_elements == 'select'",
                                   uiOutput('references')
                                   ),

                  actionButton('draw_matrix', 'Draw Matrix')
                  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # =========================================================================
    # example purposes only
    # =========================================================================

    # test <- cf %>% filter(ref == 'asc_230-10-45-12') %>%
    #     select(accession_number, element_name) %>% arrange(accession_number)
    # sim <- relationship_matrix(test, scale = TRUE)
    # =========================================================================
    load("~/drive/projects/DQ_requests/sm_support/data/ref_elements/asc_230_ref_elements/asc_230_10_45_all.Rdata")

    matrix_choices <- reactive({
        choices <- list()
        nms <- unique(cf$ref)
        try(group <- stringr::str_split(nms[2], '[()]')[[1]][1])
        choices[[group]] <- nms
    })

    # update the choices in references
    output$references <- renderUI({
        selectInput(inputId = 'references2',
                    label = 'Choose Element Sets',
                    choices = matrix_choices())
    })

    rv <- reactiveValues(data = NULL, name = NULL)

    observeEvent(input$draw_matrix, {
            if (input$which_elements == 'select') {
                validate(
                    need(input$references2, message = 'Select a set of elements')
                )
                test <- cf %>%
                    filter(ref == input$references2) %>%
                    select(accession_number, element_name) %>%
                    arrange(accession_number)
                sim <- relationship_matrix(test, scale = TRUE)

                rv$data <- sim
                rv$name <- input$references2
            } else if (input$excel_file) {
                validate(
                    need(input$excel_file, message = 'Upload a worksheet of elements')
                )
                rv$name <- strsplit(input$excel_file$name, '.xls')[[1]]
                # TODO: Write function for querying data from database
            }
        })

    output$matrix <- renderPlotly({
        req(rv$data)

        plot <- plot_ggplot_mat(rv$data, main = rv$name)

        ggplotly(plot, tooltip = c('Var1', 'Var2', 'value')) %>%
            layout(
                xaxis =
                    list(
                        showticklabels = TRUE,
                        tickfont = list(family = "'Lato', sans-serif",
                                        size = 10),

                        margin = list,
                        showgrid = TRUE,
                        gridcolor = 'black'
                    ),
                yaxis = list(
                    tickangle = -30,
                    tickfont = list(family = "'Lato', sans-serif",
                                    size = 10),
                    showgrid = TRUE,
                    gridcolor = 'black'
                ),
                margin = list(b = 275, l = 450, pad = 10),
                titlefont = list(family = "'Lato', sans-serif",
                                 size = 28),
                font = list(size = 4)
            )
    })
}

# Run the application
shinyApp(ui = ui, server = server)

# =============================================================================
# Random Notes
# =============================================================================

# Create a plot_ly plot
# output$matrix <- renderPlotly({
#     blues <- RColorBrewer::brewer.pal(9, 'Blues')
#     nms <- colnames(sim)
#     df <- reshape2::melt(frequency_matrix)
#
#     f1 <- list(
#         family = "'Lato', sans-serif",
#         size = 10
#     )
#     title <- list(
#         family = "'Lato', sans-serif",
#         size = 28
#     )
#     ax <- list(
#         showticklabels = TRUE,
#         tickangle = -30,
#         tickfont = f1,
#         margin = list,
#         showgrid = TRUE
#     )
#     ay <- list(
#         tickangle = -30,
#         tickfont = f1,
#         showgrid = TRUE
#     )
#
#     plot_ly(x = nms, y = nms, source = 'matrix') %>%
#         add_trace(z = sim,
#                   key = sim, type = 'heatmap', name = 'Usage') %>%
#         layout(xaxis = ax, yaxis = ay, showlegend = TRUE,
#                margin = list(b = 275, l = 450))
# })
