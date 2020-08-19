# ====================== Enrichment Summary ======================
bodySummary <- tabItem(tabName = "summary",
    fluidRow(
      column(
        width = 12,
        uiOutput("summary_cutoffs")
      )
    ),
    br(),
    uiOutput("ui_gsea_toggle"),
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