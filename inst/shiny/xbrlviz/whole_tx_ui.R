# ui for app tab: whole taxonomy

fluidPage(
    verticalLayout(
        titlePanel("Visualize Whole Taxonomy (Beta)"),
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
                column(1, radioButtons(inputId = "layout_algo",
                                       label = "Layout Algorithm",
                                       choices = c(
                                           "Radial Tree" = "l_rt",
                                           "DrL" = "l_drl",
                                           "Multidimensional Scaling" = "l_mds"
                                       ))
                       ),
                column(10, htmlOutput("whole_tx_instructions"))
            )
        ),

        visNetworkOutput("network", width="100%", height = "1200px")
    )
)
