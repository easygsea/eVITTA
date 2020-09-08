# ====================== Mode of analysis ======================
a_mode <- conditionalPanel(
    condition = "input.tabs == 'gsea'",
    fluidRow(
        column(12,
               tags$hr(style="border-color: #48617b;margin: 8px;"),
               
               radioButtons(
                   inputId = "selected_mode",
                   label = "Select analysis mode",
                   choices = run_modes,
                   selected = "gsea"
               ),
               tags$hr(style="border-color: #48617b;margin: 8px;")
        )
    )
)

# ====================== Body GSEA ======================
bodyGSEA <- tabItem(tabName = "gsea",
    fluidRow(
        column(
            width = 4,
            # fluidRow(
                box(
                    title = "RUN Analysis", solidHeader = T,
                    width = 12,align = "left",
                    status = "primary",
                    
                    
                    
                    # select species
                    selectizeInput(
                        "selected_species",
                        "1. Select species that matches your input query:",
                        choices = species_names,
                        options = list(
                            placeholder = 'Type to search ...',
                            onInitialize = I('function() { this.setValue(""); }')
                        )
                    ),
                    
                    # database selection
                    uiOutput("test_db"),
                    
                    
                    uiOutput("bs_add_db"),
                    
                    
                    # UI select identifier
                    radioButtons(
                        "gene_identifier",
                        "2. Gene identifier",
                        choices = gene_identifiers,
                        selected = "other",
                        inline = TRUE
                    ),
                    
                    # GSEA UI - uploading RNK file
                    uiOutput("ui_rnk"),
                    uiOutput("bs_file_reset"),
                    
                    # GList UI - type in genes
                    uiOutput("ui_glist"),
                    
                    
                    # br(),
                    uiOutput("ui_gsea_par"),
                    # br(),
                    fluidRow(
                        column(
                            width = 12, align = "right",
                            uiOutput("run_GSEA"),
                            uiOutput("run_GList")
                        )
                    )
                )
            # )
        ),
        column(
            width = 8,
            fluidRow(
                tabBox(
                    title = "Welcome to easyGSEA", width = 12, #span(img(src = "easygsea_bw.tiff", height = 35),)
                    tabPanel(
                        "Overview",
                        uiOutput("summary_box")
                    ),
                    tabPanel(
                        "ID conversion",
                        uiOutput("id_box")
                    )
                )
            )
            
            # fluidRow(
            #     column(
            #         width = 8,
            #         uiOutput("radio_buttons")
            #     )
            #     
            # ),
            # fluidRow(
            #     uiOutput("summary_box"),
            #     uiOutput("id_box")
            # )
            
        )
    )
)