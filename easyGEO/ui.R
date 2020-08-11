#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




sidebar <- dashboardSidebar(
    sidebarMenu(id="menu1",
                menuItem("Extract GEO Data", tabName="tab1", icon=icon("dashboard")),
                
                menuItem("Design Summary", tabName="tab2", icon=icon("pencil-ruler")),
                
                menuItem("Data matrix", tabName="tab3", icon=icon("table")),
                
                menuItem("Run DEG analysis", tabName="tab4", icon=icon("calculator")),
                
                menuItem("Visualize Results", tabName="tab5", icon=icon("chart-area"))
                
                
    )
    
    
    
    
    
    
)

loadMsg = "easyGEO"

body <- dashboardBody(
  use_waiter(), # dependencies
  waiter_show_on_load(tagList(spin_fading_circles(),h4(loadMsg))), # shows before anything else 
    
    tags$head(tags$style(HTML('
      .navbar-custom-menu>.navbar-nav>li>.dropdown-menu {
      width:400px;
      }
      '))),
    

    # verbatimTextOutput("debug0"),
    tabItems(
        tabItem(tabName = "tab1",
                fluidRow(
                    column(4,
                           
                           box(title="Input GEO accession", width = 12, solidHeader=T, status = "primary",
                               uiOutput("geo_accession_ui"),
                               
                           ),
                           
                           box(title="Select Platform", width = 12, solidHeader=T, status = "primary",
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
                           
                           tabBox(
                               title = NULL, width = 12,
                               id = "filtered design matrix",
                               
                               
                               tabPanel("Filtered Design Matrix", 
                                        radioGroupButtons(
                                            inputId = "fddf_show_rown",
                                            label = "Show column names as:", 
                                            choices = c("GEO accession", "Sample name"),
                                            selected = "GEO accession"
                                        ),
                                        
                                        DT::dataTableOutput("filtered_design_df")
                               )
                           ),
                    ),
                    column(4,
                           valueBoxOutput("design_variables", width=12),
                           valueBoxOutput("design_samples", width=12),
                           
                           tabBox(
                               title = NULL, width = 12,
                               id = "design_vis",
                               
                               
                               tabPanel("Visualization", 
                                        
                                        "Some categorical heatmap/ sunburst visualization here"
                               )
                           ),
                           
                           )
                    
                ),

        ),
        tabItem(tabName = "tab3",
                fluidRow(
                    column(4,
                           tabBox(
                               title = NULL, width = 12,
                               id = "download_matrix",
                               tabPanel("Get data matrix", 
                                        
                                        
                                        
                                        uiOutput("data_matrix_ui")
                                        
                               )
                           ),
                           
                           uiOutput("upload_matrix_ui")
                           
                    ),
                    column(8,
                           tabBox(
                               title = NULL, width = 12,
                               id = "data_matrix",
                               tabPanel("Processed data matrix", 
                                        
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
                

        ),
        
        
        
        tabItem(tabName = "tab4",
                fluidRow(
                    column(6,
                           tabBox(
                               title = NULL, width = 12,
                               id = "sp",
                               tabPanel("Select Comparison", 
                                        
                                        uiOutput("select_params_ui")
                                        
                               )
                           )
                    ),
                    column(6,
                           tabBox(
                               title = NULL, width = 12, height = "150px",
                               id = "sp",
                               tabPanel("Confirm data matrix", 
                                        
                                        uiOutput("confirm_matrix_ui")
                                        
                               )
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
        
        
        tabItem(tabName = "tab5",
                tabBox(
                    title = "DEG Visualization", width = 12,
                    id = "visDEG", height = "760px",
                    
                    tabPanel(
                      "Volcano plot",
                      column(
                        width = 8,
                        uiOutput("ui_volcano")
                      ),
                      column(
                        width = 4,
                        uiOutput("vplot_parameters")
                      )
                    ),
                    
                    tabPanel(
                      "Heatmap",
                      column(
                        width = 8,
                        plotlyOutput("heatmap_plot",width = "100%", height = "700px")
                      ),
                      column(
                        width = 4,
                        uiOutput("hplot_parameters")
                      )
                    ),
                    
                    tabPanel(
                      "Explore a gene",
                      column(
                        width = 8,
                        radioGroupButtons(
                          "a_type",
                          NULL,
                          choices = list("Violin plot"="violin","Box plot"="box")
                        ),
                        plotOutput("ui_aplot",width = "100%", height = "650px")
                      ),
                      column(
                        width = 4,
                        uiOutput("aplot_parameters")
                      )
                      
                    )
                )
                
        )
    )
    
    
    

    
)

# Put them together into a dashboardPage
shinyUI(
  dashboardPage(
    title="easyGEO - GEO expression analysis & visualization",
    dashboardHeader(title = "easyGEO", dropdownMenuOutput("dropdown_menu")),
    sidebar,
    body
  )
)
