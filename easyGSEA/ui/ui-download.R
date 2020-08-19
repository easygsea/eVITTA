# ====================== Download page ======================
bodyDownload <- tabItem(tabName = "download",
    fluidRow(
        column(
            width = 3,
            box(
                width = 12, status = "primary",
                uiOutput("menu_download_table")
            )
        ),
        column(
            width = 9,
            box(
                title = "Enrichment Results Table", solidHeader = T, width = 12, status = "primary",
                h4(HTML("Download and proceed to <b>easyVizR</b> for multiple comparisons")),
                
                div(
                    style="display: inline-block;vertical-align:top;",
                    uiOutput("ui_tl_cut")
                ),
                div(
                    style="display: inline-block;vertical-align:top;",
                    downloadButton("gs_tbl_dl",
                                   label = "Download table (.csv)"), br(),br()
                ),
                div(
                    dataTableOutput("selected_es_tables"),style="font-size:75%"
                )
            ),
            box(
                title = "Gene Set Libraries",solidHeader = T,width = 12, status = "primary",
                p("All our gene set libraries (.GMT) are available for download for further analysis and for tool development."),
                p("Please acknowledge our work if you use one of our library files."),
                uiOutput("ui_gmt_download")
            )
        )
        
    )
)