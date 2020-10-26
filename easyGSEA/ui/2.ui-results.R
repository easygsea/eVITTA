# ====================== Enrichment Plots ======================
bodyResults <- tabItem(tabName = "kegg",
    uiOutput("ui_bodyResults"),

    div(id = "kegg_reactome_wp",
        uiOutput("kegg_panel_ui"),
        uiOutput("ui_reactome"),
        uiOutput("ui_wp"))
   
    #uiOutput("p_value_help_div")
    # uiOutput("ui_manhattan_table"),
    ,fixedPanel(
        uiOutput("floating_button_tab2"),
        right = 30,
        bottom = 30
    )
)

