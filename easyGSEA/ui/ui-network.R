# ====================== Cytoscape network ======================
bodyNetwork <- tabItem(tabName = "network",
    fluidRow(
      column(
        width = 12,align = "right",
        uiOutput("ui_vis_gear")
      )
    ),
    br(),
    fluidRow(
        box(
            width = 12,
            #<i class="fas fa-chart-network"></i>
            title=span( icon("project-diagram"), "Network view of enriched gene sets"),solidHeader=T,status = "primary",
            div(
                style = "position: absolute; right: 1em; top: 3em;",
                dropdown(
                    size = "xs",
                    icon = icon("download", class = "opt"),
                    up = FALSE,
                    right = TRUE,
                    downloadButton(outputId = "download_vis", label = "Download network")
                )
            ),
            div(
                uiOutput("vis_error")
            ),
            div(
                visNetworkOutput("vis_network", height = "660px")
            )
            
        )
    )
)