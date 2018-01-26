# =============================================================================
# Multiple Google Users in Shiny App
# -----------------------------------------------------------------------------
# Find a way to have multiple Google users in a shiny app at once. This problem
# stems from the fact that the package 'googlesheets' does not have a mechanism
# for multiple users. The secret is tied to the server side, not the client side
# resulting in a disconnection from the server if more than one user attempts
# to load data from G-sheets.
# ====

library(shiny)
library(DT)
library(googleAuthR)

options(
    "googleAuthR.scopes.selected" =
        c("https://www.googleapis.com/auth/spreadsheets.readonly",
          "https://www.googleapis.com/auth/drive.readonly"))
# client id and secret
options(
    "googleAuthR.webapp.client_id" =
        "422259808797-vqt6n3f102gclifroc7083t63ajhg8pf.apps.googleusercontent.com")
options("googleAuthR.webapp.client_secret" = "oPX4y_QlVo_WrX3qetl_RZmd")


# ====
# Define functions for use in server logic
# ====
parse_data <- function(x) {

    data <- as.data.frame(x$values)
    names(data) <- c('parent', 'child')
    data
}

get_data <- function(spreadsheetId, range) {
    l <- googleAuthR::gar_api_generator(
        baseURI = "https://sheets.googleapis.com/v4/",
        http_header = 'GET',
        path_args = list(spreadsheets = spreadsheetId,
                         values = range),
        pars_args = list(majorDimension = 'ROWS',
                         valueRenderOption = 'UNFORMATTED_VALUE'),
        data_parse_function = parse_data)

    req <- l()

    req
}

# ====
# Define UI
# ====
ui <- fluidPage(

   titlePanel("Test Multiple Google Users"),

   sidebarLayout(
      sidebarPanel(
            googleAuthUI('loginButton'), # googleAuthR function for logging in
            textInput(inputId = 'sheet_url',
                      label = 'Sheet URL'),
            textInput(inputId = 'tab_title',
                      label = 'Tab Name'),
            actionButton(inputId = 'get_sheet',
                         label = 'Import Sheet')
      ),


      mainPanel(
        dataTableOutput('tbl')
         # verbatimTextOutput('out_text')
      )
   )
)

# ====
# Define Server
# ====
server <- function(input, output, session) {


    # uses shiny modules to make the login process easy
    access_token <- callModule(googleAuth, "loginButton",
                               approval_prompt = "force",
                               login_text = "Authorize Google",
                               logout_text = "Logout Google")


    get_the_data<- eventReactive(input$get_sheet, {

        m <- regexpr("(?<=(/spreadsheets/d/))([a-zA-Z0-9-_]+)",
                     input$sheet_url, perl = T)
        n <- regexpr("(?<=[#&]gid=)([0-9]+)",
                     input$sheet_url, perl = T)
        spreadsheet_id <- regmatches(input$sheet_url, m)
        sheet_id <- regmatches(input$sheet_url, n)

        with_shiny(f = get_data,
                   shiny_access_token = access_token(),
                   spreadsheetId = spreadsheet_id,
                   range = paste0(input$tab_title, "!A2:B")
        )
    })



    output$tbl = renderDataTable({
        data <- get_the_data()
        data
        })
}

# ====
# Run App
# ====
shinyApp(ui = ui, server = server)

