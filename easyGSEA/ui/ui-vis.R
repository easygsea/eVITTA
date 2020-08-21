# ====================== Enrichment Plots ======================
bodyResults <- tabItem(tabName = "kegg",
    fluidRow(
        column(
            width = 8,
            radioGroupButtons(
                inputId = "plot_type",
                choiceNames = list(span(icon("chart-bar"),"Bar plot"),span(icon("first-order-alt"),"Bubble plot"),span(icon("file-word"),"Keywords"),span(icon("braille"),"Manhattan plot"),span(icon("fire-alt"),"Volcano plot")), #,
                choiceValues = list("bar", "bubble","word","manhattan","volcano"), #,
                selected = "bar",
                status = "primary",
                size = "normal",
                direction = "horizontal"
            ),
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
            fluidRow(
                box(
                    title = "Individual gene set enrichment statistics",status="primary",solidHeader = TRUE,
                    id = "gs_es_result",
                    width = 12, #height = "300px",
                    fluidRow(
                        column(
                            width = 9,
                            uiOutput("es_plot_term")
                        ),
                        column(
                            width = 3, align = "right",
                            br(),
                            uiOutput("es_plot_term_confirm")
                        )
                    ),
                    
                    uiOutput("ui_es")
                )
                
                
            )
        )
    ),
    
    uiOutput("kegg_panel_ui"),
    uiOutput("ui_reactome"),
    uiOutput("ui_wp")
    # uiOutput("ui_manhattan_table")
)

