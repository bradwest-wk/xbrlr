# creates ui for app tab: upload and visualize custom taxonomy

fluidPage(
    useShinyjs(),
    verticalLayout(
        titlePanel("Upload and Visualize Taxonomy (Beta)"),
        wellPanel(
            fluidRow(
                column(1, radioButtons(inputId = "link2",
                                       label = "Linkbase",
                                       choices = c(
                                           "Calculation",
                                           "Presentation"
                                       ),
                                       selected = "Calculation")
                       ),
                column(3, fileInput('input_file',
                                    label = "Taxonomy upload (Excel)",
                                    width = '100%'),
                       actionButton("reset_input", "Reset Input File")
                       ),
                column(1, radioButtons(
                    inputId = "names2", label = "Show Element Names?",
                    choices = c("Yes" = TRUE, "No" = FALSE),
                    selected = FALSE)
                ),
                column(5,verbatimTextOutput("instructions2")
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
                     plotOutput("uploadTree", width="100%", height = "900px",
                                click = "plot_click2",
                                brush = brushOpts(id = "plot_brush2",
                                                  resetOnNew = TRUE),
                                dblclick = "plot_dblclick2"), value = "std"
        ),
        tabPanel("VisNetwork Plot", value = "visNet"),
        selected = "visNet"
        )
    )
)
