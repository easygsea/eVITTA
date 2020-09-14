source("ui/css_addons.R")



sidebar <- dashboardSidebar(
    sidebarMenu(id="menu1",
                menuItem("1. Extract GEO Data", tabName="tab1", icon=icon("dashboard")),
                
                menuItem("2. Data matrix", tabName="tab3", icon=icon("table")),
                
                menuItem("(Optional: Design Matrix)", tabName="tab2", icon=icon("pencil-ruler")),

                menuItem("3. Run DEG analysis", tabName="tab4", icon=icon("calculator")),
                
                menuItem("4. Visualize Results", tabName="tab5", icon=icon("chart-area"))
                
                
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
      

    # verbatimTextOutput("debug0"),
    tabItems(
        tabItem(tabName = "tab1",
                fluidRow(
                    column(4,
                           
                           box(title=span(icon("search"), "Input GEO accession"), width = 12, solidHeader=F, status = "primary",
                               uiOutput("geo_accession_ui"),
                               
                           ),
                           
                           box(title=span(icon("hdd"),"Select Platform"), width = 12, solidHeader=F, status = "primary",
                               uiOutput("geo_platform_ui")
                           ),
                           
                           uiOutput("guide_1a")
                           
                           
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
                    column(6,
                           
                           tabBox(
                             id = "ui_select",
                             width = 12,
                             tabPanel(
                               value = "sp",
                               span(icon("check-square"),
                                    HTML(paste0("Fine-tuned sample selection",add_help(
                                      "fine_q"
                                    )))
                                    ), 
                               bsTooltip("fine_q", "Applicable when design matrix is provided by authors and complete","top"),
                               
                               uiOutput("select_params_ui")
                             ),
                             tabPanel(
                               value = "coerce",
                                span(icon("mixer"),
                                     HTML(paste0("Coerce sample selection",add_help(
                                       "coerce_q"
                                     )))
                                     ),
                               bsTooltip("coerce_q", "For any combination of samples","top"),
                               
                                uiOutput("coerce_ui")
                              
                             )
                           ),
                           
                    ),
                    column(6,
                           
                           box(title=span(icon("clipboard-check"),"Confirm data matrix"), width = 12, solidHeader=F, status = "primary", 
                               id = "sp",
                               uiOutput("confirm_matrix_ui")
                               )
                           
                          
                           
                    )
                ),
                
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
