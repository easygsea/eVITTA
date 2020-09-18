source("ui/css_addons.R")



sidebar <- dashboardSidebar(
    sidebarMenu(id="menu1",
                menuItem("1. Extract GEO data", tabName="tab1", icon=icon("dashboard")),
                
                menuItem("2. Data matrix", tabName="tab3", icon=icon("table")),
                
                menuItem("3. Filter/review design matrix", tabName="tab2", icon=icon("pencil-ruler")),

                menuItem("4. Run DEG analysis", tabName="tab4", icon=icon("calculator")),
                
                menuItem("5. Visualize results", tabName="tab5", icon=icon("chart-area"))
                
                
    )
    
    
    
    
    
    
)



body <- dashboardBody(
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
                fluidRow(
                    column(4,
                           
                           box(title=span(HTML("<b>1.1.</b>"),icon("search"), "Input GEO accession"), width = 12, solidHeader=F, status = "primary",
                               uiOutput("geo_accession_ui"),
                               
                           ),
                           
                           box(title=span(HTML("<b>1.2.</b>"),icon("hdd"),"Select Platform"), width = 12, solidHeader=F, status = "primary",
                               uiOutput("geo_platform_ui")
                           ),
                           
                           column(12,align="center",
                             uiOutput("guide_1a")
                             
                           )
                           
                           
                    ),
                    
                    column(8,
                           
                           tabBox(
                               title = NULL, width = 12,
                               id = "summary", height = "250px",
                               tabPanel("Summary", 
                                        
                                        uiOutput("gse_summary_ui")
                                        
                               ),
                               tabPanel("Study info", 
                                        
                                        DT::dataTableOutput("gse_meta_df")
                               ),
                               tabPanel("Experiment info", 
                                        
                                        DT::dataTableOutput("gsm_meta_df")
                               )
                           ),
                           
                           
                           
                    )
                    
                    
                    
                    
                )
        ),
        
        # ---------------------2. design matrix ---------------------------
        
        
        tabItem(tabName = "tab2",
                uiOutput("ui_design")

        ),
        
        # ---------------------3. data matrix ---------------------------
        
        tabItem(tabName = "tab3",
                uiOutput("ui_dm")

        ),
        
        
        # ---------------------4. run DEG ---------------------------
        
        tabItem(tabName = "tab4",
                fluidRow(
                    column(5,
                           box(
                             title=span(HTML("<b>4.1.</b>"),icon("check-square"),HTML("Confirm data matrix")), width = 12, status = "primary",
                             id = "sp",
                             # tabPanel(
                               # span(icon("clipboard-check"),"Check if data matrix is ready"),
                               uiOutput("confirm_matrix_ui")
                             # )
                           ),
                           br(),
                           box(
                             title = span(HTML("<b>4.2.</b>"),icon("mixer"),HTML("Make contrast")), width = 12, status = "primary",
                             # id = "ui_select",
                             radioGroupButtons(
                               inputId = "ui_select",
                               # label = "Select plot type",
                               choiceNames = c("By design matrix", "Manual selection"),
                               choiceValues = c("sp","coerce"),
                               selected = "sp",
                               checkIcon = list(
                                 yes = icon("check-square"),
                                 no = icon("square-o")
                               ),
                               # status = "primary",
                               direction = "horizontal"
                             ),
                             # tabPanel(
                             #   value = "sp",
                             #   span(icon("check-square"),"By design matrix"),
                                    # HTML(paste0("By design matrix",add_help(
                                    #   "fine_q"
                                    # )))
                                    # ), 
                               # bsTooltip("fine_q", "Applicable when design matrix is provided by authors and complete","top"),
                               uiOutput("select_params_ui"),
                             # ),
                             # tabPanel(
                               # value = "coerce",
                                # span(icon("mixer"),"Manual"),
                                     # HTML(paste0("Manual selection",add_help(
                                     #   "coerce_q"
                                     # )))
                                     # ),
                               # bsTooltip("coerce_q", "For any combination of samples","top"),
                               # HTML("<b>Note:</b> \"Manual\" is for any combination of samples. You may manually select samples in the control and the experimental groups. 
                               #       Select the comparisons you're interested in and run DEG analysis."),
                               # hr(),
                               
                                uiOutput("coerce_ui")
                              
                             # )
                           )
                           
                    ),
                    column(7,
                           column(
                             width = 12,
                             uiOutput("confirm_run"),
                             br()
                           ),
                           
                           fluidRow(
                             column(
                               width = 12,
                               uiOutput("run_deg_ui")
                               
                             )
                             
                           )

                    )
                )
        ),
        
        # ---------------------5. Visualization ---------------------------
        tabItem(tabName = "tab5",
                uiOutput("ui_vis")
        )
    )
    
    
    

    
)

# Put them together into a dashboardPage
shinyUI(
  dashboardPage(
    title="easyGEO - GEO expression analysis & visualization",
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
