# ====================== Download page ======================
a_download <- conditionalPanel(
    condition = "input.tabs == 'download'",
    
    uiOutput("ui_downloadlist")
)

bodyDownload <- tabItem(tabName = "download",
    fluidRow(
        # column(
        #     width = 3,
        #     uiOutput("ui_downloadlist")
        # ),
        column(
            width = 7,
            uiOutput("ui_downloadbox")
        ),
        column(5,
               box(
                   title = span(icon("book-open"),"Gene Set Libraries"), width = 12, status = "primary",
                   uiOutput("ui_gmt_download")
               )
            
        )
        
    )
)