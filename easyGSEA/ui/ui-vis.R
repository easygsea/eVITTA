# ====================== Enrichment Plots ======================
bodyResults <- tabItem(tabName = "kegg",
    fluidRow(
        column(
            width = 6,
            radioGroupButtons(
                inputId = "plot_type",
                choiceNames = list(span(icon("chart-bar"),"Bar plot"),span(icon("first-order-alt"),"Bubble plot"),span(icon("file-word"),"Keywords"),span(icon("braille"),"Manhattan plot"),span(icon("fire-alt"),"Volcano plot")), #,
                choiceValues = list("bar", "bubble","word","manhattan","volcano"), #,
                selected = "bar",
                status = "primary",
                size = "normal",
                direction = "horizontal"
            )
        ),
        column(
            width = 3, offset = 2,
            uiOutput("es_plot_term")
        ),
        column(
            width = 1,
            br(),
            uiOutput("es_plot_term_confirm")
        )
        
    ),
    fluidRow(
        column(
            width = 8,
            fluidRow(
                uiOutput("manhattan_box"),
                uiOutput("bar_box"),
                uiOutput("bubble_box"),
                uiOutput("volcano_box"),
                uiOutput("word_box"),
                uiOutput("kegg_feedback"),
                uiOutput("reactome_feedback"),
                uiOutput("wp_feedback")
            )
        ),
        column(
            width = 4,
            # fluidRow(
                box(
                    title = "Enrichment Statistics",status="primary",solidHeader = TRUE,
                    id = "gs_es_result",
                    style="font-size:75%",
                    width = NULL, height = "300px",
                    div(
                        style = 'overflow-x: scroll', 
                        DT::dataTableOutput("gs_stats_tl")
                    )
                ),
                div(
                    style = "position: relative",
                    uiOutput("ui_gsea_plots"),
                    uiOutput("ui_gsea_plots_radio")
                    
                )
            # )
        )
    ),
    uiOutput("kegg_panel_ui"),
    uiOutput("ui_reactome"),
    uiOutput("ui_wp")
    # uiOutput("ui_manhattan_table")
)

