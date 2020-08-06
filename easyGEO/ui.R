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
                
                menuItem("Analyze Data", tabName="tab3", icon=icon("calculator")),
                
                menuItem("Visualize Data", tabName="tab4", icon=icon("chart-area"))
                
                
    )
    
    
    
    
    
    
)

body <- dashboardBody(

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
                               id = "design",
                               
                               tabPanel("Filter Data", 
                                        
                                        uiOutput("filter_design_ui"),
                               ),
                               
                               tabPanel("Matrix", 
                                        
                                        # DT::dataTableOutput("design_df")
                                        DT::dataTableOutput("filtered_design_df")
                               ),
                               tabPanel("Study Design Summary", 
                                        
                                        uiOutput("design_summary_ui")
                                        
                               )
                           ),
                    ),
                    column(4,
                           valueBoxOutput("design_variables", width=12),
                           valueBoxOutput("design_samples", width=12),
                           
                           )
                    
                ),
                fluidRow(
                    column(6,
                           tabBox(
                               title = NULL, width = 12,
                               id = "sp",
                               tabPanel("Select levels", 
                                        
                                        uiOutput("select_params_ui")
                                        
                               )
                           )
                           ),
                    column(6,
                           tabBox(
                               title = NULL, width = 12,
                               id = "matrix",
                               tabPanel("Data matrix", 
                                        
                                        uiOutput("data_matrix_ui")
                                        
                               )
                           )
                           
                           )
                    
                )
        ),
        tabItem(tabName = "tab3",
                # fluidRow(
                #     column(6,
                # 
                #            box(title="Filter Data", width = 12, solidHeader=T, status = "primary",
                # 
                #                uiOutput("filter_design_ui"),
                # 
                # 
                #            ),
                #            box(title="Select Comparison", width = 12, solidHeader=T, status = "primary",
                #                "Which variable to analyze?",
                #                "Which 2 levels to compare?"
                # 
                #            ),
                #     ),
                # 
                #            tabBox(
                #                title = NULL, width = 12,
                #                id = "summary", height = "150px",
                #                tabPanel("Filtered Matrix",
                # 
                # 
                # 
                #                )
                #            )
                # 
                # 
                # 
                # ),
                # fluidRow(
                # 
                # 
                # 
                # 
                # )
                

        ),
        tabItem(tabName = "tab4",
                h2("Tab 4"),
                tabBox(
                    title = NULL, width = 12,
                    id = "summary", height = "250px",
                    tabPanel("placeholder", 
                             
                             "placeholder"
                             
                    )
                )
                
        )
    )
    
    
    

    
)

# Put them together into a dashboardPage
dashboardPage(
    dashboardHeader(title = "easyGEO"),
    sidebar,
    body
)