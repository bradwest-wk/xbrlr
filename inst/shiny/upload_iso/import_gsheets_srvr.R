# =============================================================================
# Server code for importing user developed taxonomy from gsheets
# =============================================================================

# need to specify these options when deploying to the server
# options("googlesheets.webapp.client_id" = MY_CLIENT_ID)
# options("googlesheets.webapp.client_secret" = MY_CLIENT_SECRET)
# options("googlesheets.webapp.redirect_uri" = MY_REDIRECT_URI)

options("googlesheets.webapp.redirect_uri" = 'http://127.0.0.1:4642')

output$loginButton <- renderUI({
    if (is.null(isolate(access_token()))) {
        tags$a("Authorize Google Sheets",
               href = googlesheets::gs_webapp_auth_url(),
               class = "btn btn-default")
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


get_df_tx <- function(sheet_title, tab_title = 1) {
    sht <- googlesheets::gs_title(sheet_title)
    df <- googlesheets::gs_read(sht, ws = tab_title)
    return(df)
}

# test get specific sheet
get_df_reactive <- reactive({
    req(input$sheet_title, input$tab_title)
    data <- get_df_tx(input$sheet_title, input$tab_title)
    colnames(data)[c(1,2)] <- c('parent', 'child')
    filled <- tidyr::fill(data, 'parent')
    if ( sum(duplicated(filled)) > 0 ) {
        warning("Duplicated edges in dataframe--investigate further.")
    }
    filled
})