# ====================== Mode of analysis ======================
a_mode <- conditionalPanel(
    condition = "input.tabs == 'gsea'",
    fluidRow(
        column(12,
               tags$hr(style="border-color: #48617b;margin: 8px;"),
               
               radioButtons(
                   inputId = "selected_mode",
                   label = div(style = "font-weight:400;", p(style="margin-block-end: 2px;",
                                                             "Select mode of analysis:",
                                                             tags$style(type = "text/css", paste0("#","mode_q","{display: inline-block;width: 17px;height: 17px;padding: 0;border-radius: 50%;vertical-align: text-top;margin-left: 3px;font-size: 10px;padding-top: 1px;","vertical-align:baseline !important; ","}")),
                                                             bsButton("mode_q", label = "", icon = icon("question"), style = "info", size = "extra-small", onclick ="window.open('https://tau.cmmt.ubc.ca/eVITTA/documentation/index2.html')"))
                               ),
                   choices = run_modes,
                   selected = "gsea"
                   #demo session for ora analysis, where the default selected mode is "glist" please be careful here
                   # selected = "glist"
               )
               ,bsTooltip("mode_q",HTML('Method for functional enrichment analysis. <b><i>Click</i></b> to visit <a target="_blank" href="https://tau.cmmt.ubc.ca/eVITTA/documentation/index2.html">easyGSEA User Guide</a> for more information')
                          ,placement = "right")
               # ,bsButton("loadx","Example Run")
               ,tags$hr(style="border-color: #48617b;margin: 8px;")
        )
    )
)

# a_example <- conditionalPanel(
#     condition = "input.tabs == 'gsea'",
#     
#     fluidRow(
#         column(12,
#             tags$hr(style="border-color: #48617b;margin: 8px;")
#             ,bsButton("loadx","Example Run")
#         )
#     )
# )

# ====================== Body GSEA ======================
bodyGSEA <- tabItem(tabName = "gsea",
    fluidRow(
        column(
            width = 4,
            # fluidRow(
                box(id= "run_analysis_box",
                    title = span(icon("code"),"RUN Analysis"), solidHeader = F,
                    width = 12,align = "left",
                    status = "primary",

                    # select species
                    div(id="selected_box",
                        selectizeInput(
                        "selected_species",
                        HTML(paste0(
                            "1. Select species that matches your input query:",
                            add_help("selected_species_q", style="padding:3px 0 0 0;position:absolute;right:0.4em;")
                            )),
                        
                        choices = species_names,
                        #the default value of selected for demo session
                        selected = "",
                        options = list(
                            placeholder = 'Type to search ...'
                            ,onInitialize = I('function() { this.setValue(""); }')
                        )
                    )),
                    bsTooltip("selected_species_q", HTML("Select species of interest, or <b>Other (custom GMT)</b> for custom analysis")
                              , placement = "top"), #, then <i>click</i> <b>Confirm selection</b> to proceed
                    
                    # database selection
                    uiOutput("test_db"),
                    uiOutput("gmt_upload"),
                    uiOutput("bs_add_db"),
                    
                    
                    # UI select identifier
                    conditionalPanel(
                        condition = "input.selected_species != 'other'",
                        
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
                        
                    ),
                                        
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
            uiOutput("demo_nav")
            
        ),
        column(
            width = 8,
            div(
                style="position:relative;z-index:21",
                absolutePanel(
                    uiOutput("nav_btn_run"),
                    right = 25,
                    top = 10
                )
            )
            ,
            box(id = "welcome_to_box",
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
            uiOutput("gmt_box"),
            uiOutput("id_box")
        )
    ),
    div(
    fixedPanel(
        style = "z-index:20",
        uiOutput("floating_button_tab1"),
        right = 30,
        bottom = 30
    ))
)