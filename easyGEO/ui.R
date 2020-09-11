source("ui/css_addons.R")



sidebar <- dashboardSidebar(
    sidebarMenu(id="menu1",
                menuItem("Extract GEO Data", tabName="tab1", icon=icon("dashboard")),
                
                menuItem("Data matrix", tabName="tab3", icon=icon("table")),
                
                menuItem("Design Summary (optional)", tabName="tab2", icon=icon("pencil-ruler")),

                menuItem("Run DEG analysis", tabName="tab4", icon=icon("calculator")),
                
                menuItem("Visualize Results", tabName="tab5", icon=icon("chart-area"))
                
                
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
        
        tabItem(tabName = "tab2",
                fluidRow(
                    
                    column(8,
                           
                           
                           tabBox(
                               title = NULL, width = 12,
                               id = "filter",
                               
                               tabPanel("Filter Data", 
                                        
                                        uiOutput("filter_design_ui"),
                               ),

                               tabPanel("Study Design Summary",

                                        uiOutput("design_summary_ui")

                               )
                           ),
                           box(title=span(icon("microscope"),"Filtered Design Matrix"), width = 12, solidHeader=F, status = "primary", 
                               id = "filtered design matrix",
                               
                               fluidRow(
                                 column(12,
                                        box(title=NULL, width = 6, solidHeader=T, status="primary",
                                            radioGroupButtons(
                                              inputId = "fddf_show_rown",
                                              label = "Show column names as:", 
                                              choices = c("GEO accession", "Sample name"),
                                              selected = "GEO accession"
                                            )
                                        ),
                                        
                                        DT::dataTableOutput("filtered_design_df")
                                        
                                 )
                                 
                               )
                               
                               )

                    ),
                    column(4,
                           valueBoxOutput("design_variables", width=12),
                           valueBoxOutput("design_samples", width=12),
                           
                           # tabBox(
                           #     title = NULL, width = 12,
                           #     id = "design_vis",
                           #     
                           #     
                           #     tabPanel("Visualization", 
                           #              
                           #              "Some categorical heatmap/ sunburst visualization here"
                           #     )
                           # ),
                           
                           )
                    
                ),

        ),
        tabItem(tabName = "tab3",
                fluidRow(
                    column(4,
                           
                           box(title=span(icon("download"),"Get data matrix"), width = 12, solidHeader=F, status = "primary", 
                               id = "download_matrix",
                               
                               uiOutput("data_matrix_ui")
                               
                               ),
                           
                           uiOutput("upload_matrix_ui")
                           
                    ),
                    column(8,
                           
                           box(title=span(icon("table"),"Processed data matrix"), width = 12, solidHeader=F, status = "primary", 
                               id = "data_matrix",
                               
                               fluidRow(
                                 column(12,
                                        box(title=NULL, width = 6, solidHeader=T, status="primary",
                                            radioGroupButtons(
                                              inputId = "dmdf_show_coln",
                                              label = "Show column names as:", 
                                              choices = c("GEO accession", "Sample name"),
                                              selected = "GEO accession"
                                            ),
                                            
                                        ),
                                        
                                        uiOutput("dmdf_filter_ui"),
                                        
                                        DT::dataTableOutput("data_matrix_df")
                                        
                                 )
                                 
                                 
                               )
                           
                           )
                           
                    )
                )
                

        ),
        
        
        
        tabItem(tabName = "tab4",
                fluidRow(
                    column(6,
                           
                           tabBox(id = "ui_select",
                             width = 12, #solidHeader=F, status = "primary", 
                             tabPanel(
                               span(icon("check-square"),"Fine-tune Selection"), 
                               id = "sp",
                               
                               uiOutput("select_params_ui")
                             ),
                             tabPanel(id = "coerce",
                              span(icon("mixer"),"Coerce Selection"),
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
        
        # --------------------- Visualization tab ---------------------------
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
