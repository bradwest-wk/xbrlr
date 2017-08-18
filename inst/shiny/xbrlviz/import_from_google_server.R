# Adds functionality to the upload taxonomy tab so that user can import
# working taxonomy from google sheets.

# need to specify these options when deploying to the server
# options("googlesheets.webapp.client_id" = MY_CLIENT_ID)
# options("googlesheets.webapp.client_secret" = MY_CLIENT_SECRET)
# options("googlesheets.webapp.redirect_uri" = MY_REDIRECT_URI)

# client secret
# See client secret file
# client-ID
# See google developers console

# you need to declare a redirect_uri for when you logout or return to the app
# for local testing, set the URI to a static local uri, then run the app by
# specifying the port to run it on.  For deploying to shinyapps.io, you'll need
# to change the redirect uri to the webapp's url.
options("googlesheets.webapp.redirect_uri" = 'http://127.0.0.1:4642')

# Button for authorizing with google
output$loginButton <- renderUI({
    if (is.null(isolate(access_token()))) {
        tags$a("1. Authorize Google Sheets",
               href = googlesheets::gs_webapp_auth_url(),
               class = "btn btn-default")
    } else {
        return()
    }
})


output$logoutButton <- renderUI({
    if (!is.null(access_token())) {
        # Revoke the token too? use access_token$revoke()
        tags$a("Revoke Google Sheets",
               href = getOption("googlesheets.webapp.redirect_uri"),
               class = "btn btn-default")
    } else {
        return()
    }
})

# gets the auth code from the return URL
access_token <- reactive({
    # gets all the parameters in the URL. Auth code is one of them
    pars <- parseQueryString(session$clientData$url_search)

    if (length(pars$code) > 0) {
        googlesheets::gs_webapp_get_token(auth_code = pars$code)
    } else {
        NULL
    }
})

# test function to see if authorization works
gsLs <- reactive({
    googlesheets::gs_ls()
})

# get the sheet as df
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

output$listSheets <- DT::renderDataTable({
    dat <- rv$data
    DT::datatable(dat)
})
# test function to see if authorization works


# =============================================================================









