# creates ui for app tab: upload and visualize custom taxonomy

fluidPage(
    useShinyjs(),
    verticalLayout(
        titlePanel("Upload and Visualize Taxonomy (Beta)"),
        wellPanel(
            fluidRow(
                column(3,
                       fileInput('input_file',
                                    label = "Taxonomy upload (Excel)",
                                    width = '100%'),
                       actionButton("reset_input", "Reset Input File")
                       ),
                column(4,
                       textInput('sheet_title', "Google Spreadsheets Title"),
                       br(),
                       textInput("tab_title", 'Worksheet Title', value = 'Sheet1'),
                       br(),
                       uiOutput("loginButton"),
                       br(),
                       actionButton('get_sheet', '2. Get Spreadsheet'),
                       br(),
                       uiOutput("logoutButton")),
                column(1, radioButtons(
                    inputId = "names2", label = "Show Element Names?",
                    choices = c("Yes" = TRUE, "No" = FALSE),
                    selected = FALSE)
                ),
                column(2, downloadButton(
                    "downloadData", "Download Graph Image")
                    )
            )
        ),
        # verbatimTextOutput("instructions2"),
        # verbatimTextOutput("fname"),
        #
        # verbatimTextOutput("df"),
        tabsetPanel(
            tabPanel("Traditional Plot",
                     plotOutput("uploadTree", width="100%", height = "1500px",
                                click = "plot_click2",
                                brush = brushOpts(id = "plot_brush2",
                                                  resetOnNew = TRUE),
                                dblclick = "plot_dblclick2"), value = "std"
        ),
        tabPanel("visNetwork Plot",
                 visNetworkOutput("vis_net", width='100%', height='900px'),
                 value = "visNet"
                 ),
        tabPanel("File View",
                 source("./import_from_google_ui.R", local = TRUE)),
        tabPanel("Directions",
                 wellPanel(
                     htmlOutput("instructions2"),
                     verbatimTextOutput("format")
                 )),
        selected = "Directions"
        )
    )
)
