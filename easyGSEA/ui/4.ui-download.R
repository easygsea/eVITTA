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
        ,div( id="btn_download",

            style="position:relative;z-index:1000",
            absolutePanel(
                uiOutput("download_b_btn"),
                right = 35,
                top = 10
            )
        )
    ),
    fixedPanel(
        uiOutput("floating_button_tab4"),
        right = 30,
        bottom = 30
    )
)