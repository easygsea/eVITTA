#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#




sidebar <- dashboardSidebar(
    sidebarMenu(id="tabs",
                menuItem("Organize Files", tabName="tab1", icon=icon("table")),
                
                menuItem("Single Dataset", tabName="tab2", icon=icon("pencil-ruler")),
                
                menuItem("Multiple Datasets", tabName="tab3", icon=icon("table")),
                
                tags$hr(style="border-color: #808080; margin:10px;"),
                
                # initially hide the option panels using shinyjs, to prevent empty white boxes from flashing
                shinyjs::hidden(
                    div(id="sidebar_opt",
                        conditionalPanel(
                            condition = "input.tabs == 'tab2'",
                            fluidRow(
                                column(12,
                                       div(id="x0_1", uiOutput("select_df"))
                                       ,
                                       div(id="x0_2", 
                                           
                                           radioButtons(
                                               inputId = "mode",
                                               label = "Subset data:",
                                               choices = c("All genes", "List of genes"))
                                           
                                       ),
                                       conditionalPanel(
                                           condition = "input.mode == 'List of genes'",
                                           textAreaInput("genelist_p1", "Input gene list (separated by new line):", "")
                                       ),
                                       div(id="x0_3",
                                           
                                           conditionalPanel(
                                               condition = "input.mode == 'All genes'",
                                               actionButton("submit_x", "Visualize!")
                                           ),
                                           conditionalPanel(
                                               condition = "input.mode == 'List of genes'",
                                               actionButton("submit_genelist", "Visualize!")
                                           )
                                           
                                       )
                                       
                                )
                                
                            ),    
                        ),
                        
                        
                        conditionalPanel(
                            condition = "input.tabs == 'tab3'",
                            fluidRow(
                                column(12,
                                       div(id="n0_1", uiOutput("select_df_p2")),
                                       div(id="n0_2", style="height:60px",
                                           uiOutput("n_shared")
                                       ),
                                       
                                       div(id="n0_3",
                                           actionButton("n_use_data", "Visualize!")
                                       )
                                       
                                )
                            ),
                            
                        )
                        )
                    
                )
                
                
                
                
                
    )
    
    
    
    
    
    
)

loadMsg = "easyVizR"

body <- dashboardBody(
    rintrojs::introjsUI(), useShinyjs(),
    # use_waiter(), # dependencies
    # waiter_show_on_load(tagList(spin_fading_circles(),h4(loadMsg))), # shows before anything else

    
    

    # verbatimTextOutput("debug0"),
    tabItems(
        tabItem(tabName = "tab1",
                fixedPanel(
                    div(
                        actionBttn(
                            inputId = "help_organize", label=NULL,
                            icon = icon("question"), style="material-circle", color="primary", size="lg"
                        ),
                    ),
                    right = 30,
                    bottom = 30
                ),
                
                
                fluidRow(
                    column(width = 6,
                           div(id="u_1",
                               fluidRow(column(12,
                                          box(width=12,
                                              title = span( icon("upload"), "Single File Upload"),  status = "primary", solidHeader = TRUE,
                                              id = "tab1",
                                              
                                              fileInput("file", "Upload a single CSV file:",
                                                        accept = c(
                                                            "text/csv",
                                                            "text/comma-separated-values,text/plain",
                                                            ".csv")
                                              ),
                                              
                                              
                                              
                                              uiOutput("uploaded_file"),
                                              uiOutput("uploaded_file_name"),
                                              
                                              # only show these buttons when file is uploaded.
                                              uiOutput("cn_ins"),
                                              uiOutput("cn_1"),
                                              uiOutput("cn_2"),
                                              uiOutput("load_other_cols"),
                                              uiOutput("cn_feedback"),
                                              uiOutput("cn_3"),
                                          ),
                                          
                                          ))
                           ),
                           div(id="u_1b",
                               fluidRow(column(12,
                                               box(width=12,
                                                   title = span( icon("folder-open"), "Batch upload"),  status = "primary", solidHeader = TRUE,
                                                   
                                                   tags$div(class="form-group shiny-input-container", 
                                                            tags$div(tags$label("Upload a folder containing CSV files:")),
                                                            tags$div(tags$label("Select folder", class="btn btn-primary",
                                                                                tags$input(id = "fileIn", webkitdirectory = TRUE, type = "file", style="display: none;", onchange="pressed()"))),
                                                            tags$label("", id = "noFile"), # need to fix
                                                            tags$div(id="fileIn_progress", class="progress progress-striped active shiny-file-input-progress",
                                                                     tags$div(class="progress-bar") # somehow this breaks the upload = =..
                                                            )     
                                                   ),
                                                   uiOutput("batch_feedback_1"),
                                                   uiOutput("batch_opt_1"),
                                                   uiOutput("batch_opt_2"),
                                                   uiOutput("batch_additional_cols"),
                                                   uiOutput("batch_feedback_2"),
                                                   uiOutput("batch_feedback_3"),
                                                   
                                                   uiOutput("ug_3")
                                               ),
                               )),
                           ),
                           
                           div(id="u_1c",
                               fluidRow(column(12,
                                               box(width=12,
                                                   title = span( icon("table"), "From existing dataset"), status = "primary", solidHeader = TRUE,
                                                   collapsible = T,
                                                   tabPanel("Loaded files", 
                                                            uiOutput("add_subset_select"),
                                                            uiOutput("add_subset_opt"),
                                                            uiOutput("add_subset_preview"),
                                                            uiOutput("add_subset_name"),
                                                            
                                                            uiOutput("add_subset_confirm"),
                                                   )
                                               )
                                               ))
                               ),
                           
                    ),
                    
                    column(6,
                           div(id="u_2",
                               fluidRow(column(12,
                                               box(width=12,
                                                   title = span( icon("tasks"), "Manage Datasets"), status = "primary", solidHeader = TRUE,
                                                   id = "tab2",
                                                   tabPanel("Loaded files", 
                                                            uiOutput("delete_deg"),
                                                            uiOutput("delete_deg_confirm"),
                                                   )
                                               )
                                               ))
                               )
                           
                    )
                    
                )
                    
                    
                    
                    
                
        ),
        tabItem(tabName = "tab2",
                
                
                fluidRow(
                    div(
                        id = "single_main_panel",
                        column(
                            width = 12,
                            
                            div(id="x0_4",style="height:400px",
                                uiOutput("x_header"),
                                uiOutput("single_panels")
                            )
                            
                        )
                    )
                )
                ,
                fixedPanel(
                    uiOutput("x_floating_buttons"),
                    right = 30,
                    bottom = 30
                )
                
                
                
                
                
                
        ),
        tabItem(tabName = "tab3",
                
                fluidRow(
                    div(
                        id = "n_main_panel",
                        column(
                            width = 12,
                            div(id="n0_4", style="height:400px",
                                uiOutput("n_header"),
                                uiOutput("n_panels")
                            )
                            
                        )
                    )
                )
                ,
                fixedPanel(
                    uiOutput("n_floating_buttons"),
                    right = 30,
                    bottom = 30
                )
                
                
                
                
                
        )
        
        
    )
    
    
    

    
)

# Put them together into a dashboardPage
shinyUI(
  dashboardPage(
    title="easyVizR",
    dashboardHeader(title = "easyVizR", dropdownMenuOutput("dropdown_menu")),
    sidebar,
    body
  )
)
