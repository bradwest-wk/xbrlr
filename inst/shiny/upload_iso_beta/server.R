library(shiny)
library(googleAuthR)
library(visNetwork)
library(igraph)

options(
    "googleAuthR.scopes.selected" =
        c("https://www.googleapis.com/auth/spreadsheets.readonly",
          "https://www.googleapis.com/auth/drive.readonly"))
# client id and secret -- see .Rprofile

server <- function(input, output, session) {

    source('./generate_visnet.R')

    access_token <- callModule(googleAuth, "loginButton",
                               approval_prompt = "force",
                               login_text = "Authorize Google",
                               logout_text = "Logout Google")

    # how to parse the data, passed to get_data()
    parse_data <- function(x) {
        data <- as.data.frame(x$values, stringsAsFactors=FALSE)
        names(data) <- c('parent', 'child')
        filled <- tidyr::fill(data, 'parent')

        # calculate roots
        g <- graph_from_edgelist(as.matrix(filled))
        roots <- V(g)[which(igraph::degree(g, v = igraph::V(g), mode = "in")==0)]
        l <- layout_as_tree(g, root = roots, rootlevel = rep(1, length(roots)))
        # print(names(roots))

        nodes <- get_visNet_nodes(filled, physics_on = FALSE)
        edges <- get_visNet_edges(filled, nodes)

        network <- visNetwork(nodes, edges) %>%
            visEdges(width = 0.2, arrows = 'to', arrowStrikethrough = F) %>%
            visNodes(color = list(background = 'rgba(151, 194, 252, 0.65)'),
                     size = 13, font = list(size =13, align = 'left')) %>%
            visOptions(highlightNearest = list(enabled = T, degree = 2, hover = F),
                       nodesIdSelection = list(enabled = T, useLabels = T))
            # visIgraphLayout(layout = "layout_as_tree")
            # visIgraphLayout(layout = 'layout_as_tree',
            #                 root = names(roots),
            #                 rootlevel = rep(1, length(roots)))
        # modify the matrix (rotate 180 degrees)
        network$x$nodes$x <- l[,1]
        network$x$nodes$y <- l[,2]

        # l <- ncol(nodes)
        # network$x$nodes[, l+1] <-
        #     network$x$nodes[, c(l+1)] + network$x$nodes[, c(l+2)]
        # network$x$nodes[, l+2] <-
        #     network$x$nodes[, c(l+1)] - network$x$nodes[, c(l+2)]
        # network$x$nodes[, l+1] <-
        #     network$x$nodes[, c(l+1)] - network$x$nodes[, c(l+2)]
        # network$x$nodes[, l+1] <- network$x$nodes[, l+1] * -1
        # network$x$nodes$y <- network$x$nodes$y*1.75

        network
    }

    # api call
    get_data <- function(spreadsheetId, range) {
        l <- googleAuthR::gar_api_generator(
            baseURI = "https://sheets.googleapis.com/v4/",
            http_header = 'GET',
            # URLencode() range to deal with spaces in tab names
            path_args = list(spreadsheets = spreadsheetId,
                             values = URLencode(range)),
            pars_args = list(majorDimension = 'ROWS',
                             valueRenderOption = 'UNFORMATTED_VALUE'),
            data_parse_function = parse_data)

        req <- l()

        req
    }

    # reactive function for processing input
    get_from_sheets<- eventReactive(input$get_sheet, {

         m <- regexpr("(?<=(/spreadsheets/d/))([a-zA-Z0-9-_]+)",
                      input$sheet_url, perl = T)
         n <- regexpr("(?<=[#&]gid=)([0-9]+)",
                      input$sheet_url, perl = T)
         spreadsheet_id <- regmatches(input$sheet_url, m)
         sheet_id <- regmatches(input$sheet_url, n)

         with_shiny(f = get_data,
                    shiny_access_token = access_token(),
                    spreadsheetId = spreadsheet_id,
                    range = paste0(input$tab_title, "!A2:B"))
    })

    output$download_net <- downloadHandler(
        filename = function() {
            paste0('Network', '_', Sys.Date(), '.html')
        },
        content = function(file) {

            m <- regexpr("(?<=(/spreadsheets/d/))([a-zA-Z0-9-_]+)",
                         input$sheet_url, perl = T)
            n <- regexpr("(?<=[#&]gid=)([0-9]+)",
                         input$sheet_url, perl = T)
            spreadsheet_id <- regmatches(input$sheet_url, m)
            sheet_id <- regmatches(input$sheet_url, n)

            network <- with_shiny(f = get_data,
                                  shiny_access_token = access_token(),
                                  spreadsheetId = spreadsheet_id,
                                  range = paste0(input$tab_title, "!A2:B"))
            # attempt to resize the output network
            network %>%
                visOptions(width='1800px', height='1125px',
                           highlightNearest =
                               list(enabled = T, degree = 2, hover = F),
                           nodesIdSelection = list(enabled = T, useLabels = T)) %>%
                visSave(file)
        }
    )

    output$vis_net <- renderVisNetwork({
        get_from_sheets()
    })

}
