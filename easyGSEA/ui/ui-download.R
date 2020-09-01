# ====================== Download page ======================
bodyDownload <- tabItem(tabName = "download",
    fluidRow(
        column(
            width = 3,
            uiOutput("ui_downloadlist")
        ),
        column(
            width = 9,
            uiOutput("ui_downloadbox"),
            box(
                title = "Gene Set Libraries",solidHeader = T,width = 12, status = "primary",
                p("All our gene set libraries (.GMT) are available for download for further analysis and for tool development."),
                p("Please acknowledge our work if you use one of our library files."),
                uiOutput("ui_gmt_download")
            )
        )
        
    )
)