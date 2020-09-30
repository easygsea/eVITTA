# -------------- TAB 1 -------------------

body1 <- tabItem(tabName = "tab1",
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
                                                  title = span( strong("1.1a."), icon("upload"), "Single File Upload"),  status = "primary", solidHeader = F,
                                                  id = "tab1",
                                                  
                                                  fileInput("file", 
                                                            label_with_help_bttn("Upload a single CSV file:", "upload_q"),
                                                            accept = c(
                                                              "text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv")
                                                  ),
                                                  
                                                  
                                                  
                                                  
                                                  
                                                  # uiOutput("uploaded_file"),
                                                  # uiOutput("uploaded_file_name"),
                                                  # # # only show these buttons when file is uploaded.
                                                  # uiOutput("cn_ins"),
                                                  # uiOutput("cn_1"),
                                                  # uiOutput("cn_2"),
                                                  # uiOutput("load_other_cols"),
                                                  # uiOutput("cn_feedback"),
                                                  # uiOutput("cn_3"),
                                              ),
                                              
                              ))
                          ),
                          div(id="u_1b",
                              fluidRow(column(12,
                                              box(width=12,
                                                  title = span( strong("1.1b."),icon("folder-open"), "Batch upload"),  status = "primary", solidHeader = F,
                                                  
                                                  tags$div(class="form-group shiny-input-container", 
                                                           tags$div(label_with_help_bttn(tags$label("Upload a folder containing CSV files:"), "upload_batch_q")
                                                             ),
                                                           tags$div(tags$label("Select folder", class="btn btn-primary",
                                                                               tags$input(id = "fileIn", webkitdirectory = TRUE, type = "file", style="display: none;", onchange="pressed()"))),
                                                           tags$label("", id = "noFile"), # need to fix
                                                           tags$div(id="fileIn_progress", class="progress progress-striped active shiny-file-input-progress",
                                                                    tags$div(class="progress-bar") # somehow this breaks the upload = =..
                                                           )     
                                                  ),
                                                  # uiOutput("batch_feedback_1"),
                                                  # uiOutput("batch_opt_1"),
                                                  # uiOutput("batch_opt_2"),
                                                  # uiOutput("batch_additional_cols"),
                                                  # uiOutput("batch_feedback_2"),
                                                  # uiOutput("batch_feedback_3"),
                                                  
                                                  # uiOutput("ug_3")
                                              ),
                              )),
                          ),
                          
                          # div(id="u_1c",
                          #     fluidRow(column(12,
                          #                     box(width=12,
                          #                         title = span( icon("table"), "From existing dataset"), status = "primary", solidHeader = F,
                          #                         collapsible = T,
                          #                         tabPanel("Loaded files", 
                          #                                  uiOutput("add_subset_select"),
                          #                                  uiOutput("add_subset_opt"),
                          #                                  uiOutput("add_subset_preview"),
                          #                                  uiOutput("add_subset_name"),
                          #                                  
                          #                                  uiOutput("add_subset_confirm"),
                          #                         )
                          #                     )
                          #     ))
                          # ),
                          
                          uiOutput("guide_1a"),
                          
                   ),
                   
                   column(6,
                          div(id="u_2",
                              fluidRow(column(12,
                                              box(width=12,
                                                  title = span(strong("1.2."), icon("tasks"), "Manage Datasets"), status = "primary", solidHeader = F,
                                                  id = "tab2",
                                                  tabPanel("Loaded files", 
                                                           uiOutput("delete_deg"),
                                                           uiOutput("delete_deg_confirm"),
                                                  )
                                              ),
                                              
                              ))
                          )
                          
                   )
                   
                 )
                 
)