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

    # the plots
    visNetworkOutput("vis_net", width='100%', height='2000px'),

    htmlOutput("instructions",
               style =
                  'position: absolute;
                   top: 40%;
                   left: 50%;
                   right: 25%;
                   font-size: 130%;
                   transform: translate(-50%, -50%);'),

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
                                             label = "Upload Excel",
                                             placeholder = 'network.xlsx')),

                  br(),


                  actionButton('get_edges', 'Import'),
                  actionButton('reset_inputfile',
                               label = 'Reset Input')
                  ),

    div(style =
            'position: fixed;
            right: 10px;
            top: 10px;',
        downloadButton("download_net", 'Export Network'))
)


server <- function(input, output, session) {

    rv <- reactiveValues(data = NULL, name = NULL)

    source('./import_gsheets_srvr.R', local = TRUE)

    source('./visnet_tree_srvr.R', local = TRUE)

    # output$instructions <- renderText({
    #     req(is.null(rv$data))
    #     message <-
    #         paste("To import an edgelist from Google Sheets, first authorize
    #               Google Sheets, then enter the Sheet and Worksheet names, and
    #               finally click 'Import'.  Alternatively, you may upload a .xlsx
    #               file from your local machine. Use scroll to zoom, click and
    #               drag to pan, and either 'Select by ID' or clicking on nodes
    #               to select individual nodes.  If you wish to export the network,
    #               click Export Network and a browser download will commence. To
    #               view these instructions again, click 'Reset Input'.  Contact
    #               brad dot g dot west at workiva dot com with bug reports or
    #               questions.")
    # })

    output$instructions <- renderUI({
        str <- paste("The user may choose to import an edgelist from Google Drive or from a local Excel file.",
                     "To import from Sheets: 1. Authorize, 2. Enter spreadsheet and worksheet titles, 3. Import.",
                     "The first two columns of the sheet must define parent-child relationships and contain a header
                     in the first row. Use scroll to zoom, click-drag to pan, and mouse clicks or the dropdown list to select nodes.",
                     "If you wish to export the network, use the button in the upper right and a browser download will commence",
                     "To view these instructions again, click 'Reset Input'. Contact Brad West via Hipchat or brad dot west at workiva
                     dot com with bug reports or questions.", sep ='<br/><br/>')
        HTML(str)
    })

    observeEvent(input$get_edges, {
        if (input$upload_type == 'excel') {
            validate(
                need(input$excel_file, message = 'Please choose an excel file')
            )
            rv$name <- strsplit(input$excel_file$name, '.xls')[[1]]
            rv$data <- get_raw_input(input$excel_file)
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
            rv$name <- input$sheet_title
            rv$data <- get_df_reactive()
        }
    })

    observeEvent(input$reset_inputfile, {
        shinyjs::reset('excel_file')
        shinyjs::reset('sheet_title')
        rv$data <- NULL
    })

}

# Run the application
shinyApp(ui = ui, server = server)

