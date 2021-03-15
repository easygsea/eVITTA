source("ui/css_addons.R")


# ====================== Mode of analysis ======================
upload_mode <- conditionalPanel(
  condition = "input.menu1 == 'tab1'",
  fluidRow(
    column(12,
           tags$hr(style="border-color: #48617b;margin: 8px;"),
           
           radioButtons(
             inputId = "selected_mode",
             label = div(style = "font-weight:400;", HTML(paste0("Mode of analysis:",add_help("mode_q")))),
             choices = list("Retrieval by GSE number" = "auto", "Manual uploads" = "manual"),
             selected = "auto"
             # selected = "manual"
           )
           ,bsTooltip("mode_q",HTML("Select the method to analyze NCBI GEO transcriptome data")
                      ,placement = "right")
           ,tags$hr(style="border-color: #48617b;margin: 8px;")
    )
  )
)

sidebar <- dashboardSidebar(
    sidebarMenu(id="menu1",
                menuItem("1. Extract GEO data", tabName="tab1", icon=icon("dashboard")),
                
                upload_mode,
                
                menuItem(text = span(id = "tab3_text", "2. Data matrix"), tabName="tab3", icon=icon("table")),
                
                menuItem(text = span(id = "tab2_text", "3. Filter/review design matrix"), tabName="tab2", icon=icon("pencil-ruler")),
                
                menuItem(text = span(id = "tab4_text", "4. Run DE analysis"), tabName="tab4", icon=icon("calculator")),
                
                menuItem(text = span(id = "tab5_text", "5. Visualize results"), tabName="tab5", icon=icon("chart-area")),
                
                uiOutput("btn_demo")


    )
    ,disconnectMessage(text = "Your session has timed out. Please refresh page and start again. For bug report, email us at evitta@cmmt.ubc.ca. Thank you for your support.")






)




body <- dashboardBody(
  rintrojs::introjsUI(), # introjs
  shinyjs::useShinyjs(),# dependencies
  #shinyjs::extendShinyjs(text = "shinyjs.refresh = function() { location.reload(); }"),# extend by adding a refresh function
  use_waiter(), # dependencies
  waiter_show_on_load(tagList(spin_three_bounce(),h4(loadMsg)), color = "#1976D2"), # shows before anything else

    # apply CSS theme
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),


    # apply specific css adjustments additionally
    css_addons,

  useShinyalert(),  # Set up shinyalert

    # verbatimTextOutput("debug0"),
    tabItems(
        tabItem(tabName = "tab1",
                # fluidRow(column(12,
                #                 box(title=NULL, width = 12, solidHeader=T, status = "primary",
                #                     uiOutput("progress_1")
                #                     )
                #
                #
                #                 )),
                uiOutput("ui_tab1")
                # fluidRow(
                #     column(4,
                # 
                #            box(title=span(HTML("<b>1.1.</b>"),icon("search"), "Input GEO accession"), width = 12, solidHeader=F, status = "primary",
                #                uiOutput("geo_accession_ui"),
                # 
                #            ),
                # 
                #            box(title=span(HTML("<b>1.2.</b>"),icon("hdd"),"Select Platform"), width = 12, solidHeader=F, status = "primary",
                #                uiOutput("geo_platform_ui")
                #            ),
                # 
                #            column(12,align="center",
                #              uiOutput("guide_1a")
                # 
                #            )
                # 
                # 
                #     ),
                # 
                #     column(8,
                # 
                #            tabBox(
                #                title = NULL, width = 12,
                #                id = "summary", height = "250px",
                #                tabPanel("Summary",
                # 
                #                         uiOutput("gse_summary_ui")
                # 
                #                ),
                #                tabPanel("Study info",
                # 
                #                         DT::dataTableOutput("gse_meta_df")
                #                ),
                #                tabPanel("Experiment info",
                # 
                #                         DT::dataTableOutput("gsm_meta_df")
                #                )
                #            ),
                # 
                # 
                # 
                #     )
                # 
                # 
                # 
                # 
                # )                  

                ,
                fixedPanel(
                  uiOutput("floating_button_1"),
                  style = "z-index:9999",
                  right = 30,
                  bottom = 30
                )
        ),

        # ---------------------2. design matrix ---------------------------


        tabItem(tabName = "tab2",
                uiOutput("ui_design"),
                fixedPanel(
                  style = "z-index:9999",
                  uiOutput("floating_button_2"),
                  right = 30,
                  bottom = 30
                )

        ),

        # ---------------------3. data matrix ---------------------------

        tabItem(tabName = "tab3",
                uiOutput("ui_dm"),
                fixedPanel(
                  style = "z-index:9999",
                  uiOutput("floating_button_3"),
                  right = 30,
                  bottom = 30
                )

        ),


        # ---------------------4. run DEG ---------------------------

        tabItem(tabName = "tab4",
                uiOutput("ui_run"),
                fixedPanel(
                  style = "z-index:9999",
                  uiOutput("floating_button_4"),
                  right = 30,
                  bottom = 30
                )
        ),

        # ---------------------5. Visualization ---------------------------
        tabItem(tabName = "tab5",
                uiOutput("ui_vis")
                ,fixedPanel(
                  style = "z-index:9999",
                  uiOutput("floating_button_5"),
                  right = 30,
                  bottom = 30
                )

        )
    )





)

# Put them together into a dashboardPage
shinyUI(
  dashboardPage(
    title="easyGEO - NCBI GEO's gene expression data analysis & visualization",
    dashboardHeader(title = "easyGEO",
                    dropdownMenuOutput("dropdown_menu"),
                    tags$li(class = "dropdown", actionButton("home", "eVITTA Home",icon("home"),
                                                             style="color: #fff; background-color: transparent; border-color: #c0d3e7; margin-top:8px; margin-right:8px; border-radius:2rem; border:0.125rem solid #fff",
                                                             onclick ="location.href='http://tau.cmmt.ubc.ca/eVITTA/';"))
                    ),
    sidebar,
    body


  )
)
