# ====================== Cytoscape network ======================
bodyNetwork <- tabItem(tabName = "network",
    # fluidRow(
    #   column(
    #     width = 12, align = "right",
    #     uiOutput("ui_vis_gear")
    #   )
    # ),
    # br(),
    uiOutput("ui_bodyNetwork")
    ,
    fixedPanel(
        style = "z-index:20",
        uiOutput("floating_button_tab3"),
        right = 30,
        bottom = 30
    )
)