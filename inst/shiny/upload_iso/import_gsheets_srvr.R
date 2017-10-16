# =============================================================================
# Server code for importing user developed taxonomy from gsheets
# =============================================================================

# Specifying webapp information for deployment to shiny server
# CLIENT_ID <-
#     "808310163336-oeqp8ud6rf93fk6modohb55jebnf7fja.apps.googleusercontent.com"
# CLIENT_SECRET <- "qrVg8_JRFuSTnAHKdMnnEjvd"
# REDIRECT_URI <- "https://bradwest-wk.shinyapps.io/upload_iso/"
#
# options("googlesheets.webapp.client_id" = "178989665258-mbn7q84ai89if6ja59jmh8tqn5aqoe3n.apps.googleusercontent.com")
# options("googlesheets.webapp.client_secret" = "UiF2uCHeMiUH0BeNbSAzzBxL")
# options("googlesheets.webapp.redirect_uri" = REDIRECT_URI)
options("googlesheets.webapp.redirect_uri" = 'http://127.0.0.1:4642')

output$loginButton <- renderUI({
    if (is.null(isolate(access_token()))) {
        tags$a("Authorize Google Sheets",
               href = googlesheets::gs_webapp_auth_url(),
               class = "btn btn-default",
               style = 'padding:6px; font-size:120%')
    } else {
        return()
    }
})


access_token <- reactive({
    # gets all the parameters in the URL. Auth code is one of them
    pars <- parseQueryString(session$clientData$url_search)

    if (length(pars$code) > 0) {
        googlesheets::gs_webapp_get_token(auth_code = pars$code)
    } else {
        NULL
    }
})

# parse_data <- function(x) {
#
#     data <- as.data.frame(x$values)
#     names(data) <- c('parent', 'child')
#     data
# }
#
# get_data <- function(spreadsheetId, range) {
#     l <- googleAuthR::gar_api_generator(
#         baseURI = "https://sheets.googleapis.com/v4/",
#         http_header = 'GET',
#         path_args = list(spreadsheets = spreadsheetId,
#                          values = range),
#         pars_args = list(majorDimension = 'ROWS',
#                          valueRenderOption = 'UNFORMATTED_VALUE'),
#         data_parse_function = parse_data)
#
#     req <- l()
#
#     req
# }


get_df_tx <- function(sheet_title, tab_title = 1) {
    # updated on 9-29-2017 to handle error gracefully
    # not sure why not giving me proper error messages
    sht <- NULL
    try(sht <- googlesheets::gs_title(sheet_title))
    validate(
        need(!is.null(sht),
             "We didn't recognize that workbook title.  Are you sure you entered the title properly?")
    )
    validate(
        need(try(df <- googlesheets::gs_read(sht, ws = tab_title)),
             paste("We got the workbook title, but not the tab name.  Are you sure that's the tab you meant in",
                   sheet_title, "?"))
    )


    return(df)
}


# test get specific sheet
get_df_reactive <- reactive({
    req(input$sheet_title, input$tab_title)
    withProgress(message="Importing data from Google Drive", value = 0.3, {
        data <- get_df_tx(input$sheet_title, input$tab_title)
        colnames(data)[c(1,2)] <- c('parent', 'child')
        filled <- tidyr::fill(data, 'parent')
        if ( sum(duplicated(filled)) > 0 ) {
            warning(
                paste("There are **",
                      sum(duplicated(filled)),
                      "** duplicated edges in dataframe--investigate further."))
        }
        incProgress(0.7, message = "Plotting data")
    })
    filled
})
