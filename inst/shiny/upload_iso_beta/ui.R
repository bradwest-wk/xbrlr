library(shiny)
library(googleAuthR)
library(visNetwork)
library(igraph)

ui <- fluidPage(

    titlePanel("Taxonomy Visualization - Multiple Users"),

    tags$head(
        includeCSS('./styles.css')
    ),

    # the plots
    visNetworkOutput("vis_net", width='100%', height='2000px'),



    absolutePanel(id = 'controls', class = 'panel panel-default', fixed = FALSE,
                  draggable = TRUE, top = 115, left = 15, right = 'auto',
                  bottom = 'auto', width = 340, height = 'auto',

                  h4('Upload Controls - Drag to Move'),
                  br(),

                  googleAuthUI('loginButton'),
                  br(),
                  textInput('sheet_url',
                            "Google Sheet URL",
                            value = '',
                            placeholder =
                                'https://docs.google.com/spreadsheets/...'),
                  textInput("tab_title",
                            'Tab Name',
                            value = '',
                            placeholder = 'Sheet1'),
                  br(),
                  actionButton('get_sheet',
                               'Import/Redraw',
                               style = 'padding:6px; font-size:120%'),
                  br(),
                  br(),

                  p("This version allows multiple google users at once.  Enter the URL for the workbook and the tab title.  The URLs periodically change, so if the app isn't working, check that you have access to the URL.  If you encounter issues, feel free to use the app deployed at https://bradwest-wk.shinyapps.io/upload_iso/ and let Brad know that something is wrong.")


    ),

    div(style =
            'position: fixed;
        right: 10px;
        top: 10px;',
        downloadButton("download_net", 'Export Network',
                       style = 'padding:5px; font-size:120%'))
)
