# ====================== Enrichment Summary ======================
bodySummary <- tabItem(tabName = "summary",
    fluidRow(
      column(
        width = 8,
        uiOutput("ui_gsea_toggle")
      ),
      column(
        width = 4,align = "right",
        uiOutput("summary_cutoffs")
      )
    ),
    br(),
    fluidRow(
      box(
        title = "Table Summary", solidHeader = T, width = 6, status = "primary",
        uiOutput("ui_tables")
      ),
      box(
        title = "Keyword Summary", solidHeader = T, width = 6, status = "primary",
        uiOutput("plot_words")
      )
    )
)