# ====================== Enrichment Plots ======================
bodyResults <- tabItem(tabName = "kegg",
    uiOutput("ui_bodyResults"),

    uiOutput("kegg_panel_ui"),
    uiOutput("ui_reactome"),
    uiOutput("ui_wp")
    #uiOutput("p_value_help_div")
    # uiOutput("ui_manhattan_table")
)

