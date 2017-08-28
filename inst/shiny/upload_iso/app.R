# =============================================================================
#                    Visualize User Developed Taxonomy
# -----------------------------------------------------------------------------
# Author: brad.west@workiva.com.  For internal Workiva use only.
#
# Purpose:
#
# Shiny app for visualizing a user developed taxonomy.  The user can choose to
# upload an excel document or import from google sheets an edgelist of child-
# parent relationships.  The user can choose to visualize the taxonomy with
# a custom function (underlain by igraph) or via visNetwork, an htmlwidgets
# package that used D3.
#
# Upload Format:
#      _____________________________________________
#     | PARENT   | CHILD   | COLUMN_3 | ...COLUMN_X |
#     | -------- | ------- | -------- | ----------- |
#     | parent_1 | child_1 | ...      | ...         |
#     |          | child_2 | ...      | ...         |
#     | parent_2 | child_1 | ...      | ...         |
#     |          | child_2 | ...      | ...         |
#     |          | child_3 | ...      | ...         |
#     | ...      | ...     | ...      | ...         |
#
# Currently only the parent and child columns are used, but future versions may
# support grouping, usage data, etc.  Columns must have headings.
#
# =============================================================================

library(shiny)
library(googlesheets)
library(visNetwork)

# =============================================================================
# UI Design:
#
# Full screen image of the taxonomy with a draggable (and tansparent) absolute
# Panel for selecting options.
#
# Panel: Radiobuttons for selcting the type of image, and import requirements
# (google sheets or excel upload)
# =============================================================================
ui <- fluidPage(

    titlePanel("Taxonomy Visualization"),

    tags$head(
        includeCSS('./styles.css')
        ),

    # the plot
    visNetworkOutput("vis_net", width='100%', height='2400px'),

    absolutePanel(id = 'controls', class = 'panel panel-default', fixed = TRUE,
                  draggable = TRUE, top = 115, left = 15, right = 'auto',
                  bottom = 'auto', width = 340, height = 'auto',

                  h4('Upload Controls - Drag to Move'),

                  radioButtons(inputId = 'upload_type', label = 'Upload Type',
                               choices = c('Google Sheets' = 'gsheets',
                                           'Excel File' = 'excel'),
                               inline = TRUE,
                               selected = 'gsheets'),

                  conditionalPanel("input.upload_type == 'gsheets'",
                                   uiOutput("loginButton"),
                                   br(),
                                   textInput('sheet_title',
                                             "Google Sheet Title"),
                                   textInput("tab_title",
                                             'Worksheet Title',
                                             value = 'Sheet1')),

                  conditionalPanel("input.upload_type == 'excel'",
                                   fileInput('excel_file',
                                             label = "Upload Excel"),
                                   actionButton('reset_inputfile',
                                                label = 'Reset Input')),

                  br(),

                  actionButton('get_edges', 'Import')
                  )
)


server <- function(input, output, session) {

    rv <- reactiveValues(data = NULL)

    source('./import_gsheets_srvr.R', local = TRUE)

    observeEvent(input$get_edges, {
        if (input$upload_type == 'excel') {
            validate(
                need(input$excel_file, message = 'Please choose an excel file')
            )
            # function to assign data from text_input to rv$data
        } else if (input$upload_type == 'gsheets') {
            req(input$sheet_title, input$tab_title)
            validate(
                need(input$sheet_title != ' ',
                     'Please enter a valid Google Sheet title'),
                need(input$tab_title,
                     'Please enter the worksheet title'),
                need(!is.null(access_token()),
                     label = "access_token",
                     message =
                         paste("Click 'Authorize Google Sheets' to redirect to",
                               "a Google page where you will authenticate",
                               "yourself and authorize this app to access",
                               "your Google Sheets and Google Drive.")))
            rv$data <- get_df_reactive()
        }
    })

    observeEvent(input$reset_inputfile, {
        reset('excel_file')
        rv$data <- NULL
    })

    source('./visnet_tree_srvr.R', local = TRUE)

}

# Run the application
shinyApp(ui = ui, server = server)

