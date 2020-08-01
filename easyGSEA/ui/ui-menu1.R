# ====================== Body GSEA ======================
bodyGSEA <- tabItem(tabName = "gsea",
    fluidRow(
        column(
            width = 4,
            box(
                title = "Welcome to easyGSEA", solidHeader = T,
                width = 12,align = "left",
                status = "primary",
                
                uiOutput("ui_mode"),
                br(),
                uiOutput("select_species"),
                uiOutput("test_db"),

                fluidRow(
                    column(4,
                           uiOutput("bs_add_db")
                    ),column(4,offset=2,
                             uiOutput("bs_reset_db")
                    )),
                
                
                br(),
                # GSEA UI - uploading RNK file
                uiOutput("ui_rnk"),
                uiOutput("bs_file_reset"),
                
                # GList UI - type in genes
                uiOutput("ui_glist"),
                
                
                br(),br(),
                uiOutput("ui_gsea_par"),
                br(),
                fluidRow(
                    column(
                        width = 4, offset = 6,
                        uiOutput("run_GSEA"),
                        uiOutput("run_GList")
                    )
                )
             )
        ),
        column(
            width = 8,
            fluidRow(
                column(
                    width = 8,
                    uiOutput("radio_buttons")
                )
                
            ),
            fluidRow(
                uiOutput("summary_box"),
                uiOutput("id_box")
            )
            
        )
    )
)