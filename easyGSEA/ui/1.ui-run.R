# ====================== Mode of analysis ======================
a_mode <- conditionalPanel(
    condition = "input.tabs == 'gsea'",
    fluidRow(
        column(12,
               tags$hr(style="border-color: #48617b;margin: 8px;"),
               
               radioButtons(
                   inputId = "selected_mode",
                   label = NULL,
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
                    title = span(icon("play-circle"),"RUN Analysis"), solidHeader = F,
                    width = 12,align = "left",
                    status = "primary",

                    # select species
                    selectizeInput(
                        "selected_species",
                        HTML(paste0(
                            "1. Select species that matches your input query:",
                            add_help("selected_species_q", style="padding:3px 0 0 0;position:absolute;right:0.4em;")
                            )),
                        
                        choices = species_names,
                        options = list(
                            placeholder = 'Type to search ...',
                            onInitialize = I('function() { this.setValue(""); }')
                        )
                    ),
                    bsTooltip("selected_species_q", HTML("Select species, then <i>click</i> <b>Confirm selection</b> to proceed"), placement = "top"),
                    
                    # database selection
                    uiOutput("test_db"),
                    uiOutput("bs_add_db"),
                    
                    
                    # UI select identifier
                    radioButtons(
                        "gene_identifier",
                        HTML(paste0(
                            "2. Select gene identifier:",
                            add_help("gene_identifier_q", style="padding:3px 0 0 0;position:absolute;right:0.4em;")
                        )),
                        choices = gene_identifiers,
                        selected = "other",
                        inline = TRUE
                    ),
                    bsTooltip("gene_identifier_q", HTML("The identifier of your input genes. If unsure, select <b>Other/Mixed</b>"), placement = "top"),
                    
                    # GSEA UI - uploading RNK file
                    uiOutput("ui_rnk"),
                    uiOutput("bs_file_reset"),
                    uiOutput("feedbacks"),
                    
                    # GList UI - type in genes
                    uiOutput("ui_glist"),
                    
                    
                    fluidRow(
                        column(
                            width = 12, align = "center",
                            uiOutput("run_btn")
                        )
                    )
                    
                ),
            # )
        ),
        column(
            width = 8,
            div(
                style="position:relative;z-index:1000",
                absolutePanel(
                    uiOutput("nav_btn_run"),
                    right = 25,
                    top = 10
                )
            )
            ,
            box(
                title = span(icon("seedling"),"Welcome to easyGSEA"), width = 12, status = "primary", #span(img(src = "easygsea_bw.tiff", height = 40))
                column(
                    12,
                    uiOutput("summary_txt")
                    ,br()
                    
                ),
                column(
                    12,
                    uiOutput("summary_box")
                    
                )
                
            ),
            br(),
            uiOutput("id_box")
        )
    )
)