
library(rintrojs)
library(shinyjs)



sidebar1 <- dashboardSidebar(width = 350,
    sidebarMenu(id="main_sb",
                
        menuItem("Organize Files", tabName = "sb_organize", icon = icon("table"))

    )
)

body1 <- dashboardBody(
    tabItems(
        
        #### Organize files body----------------------------------------
        
        tabItem(tabName = "sb_organize",
                h2("Organize Files"),
                tags$hr(style="border-color: grey;"),
                # verbatimTextOutput("debug0"),
                
                div(style = "position:absolute;right:1em;top:1em", 
                    actionButton("help", "Help"),
                ),
                
                
                fluidRow(
                    column(width = 6,
                        box(width=12,
                            title = span( icon("upload"), "Single File Upload"),  status = "primary", solidHeader = TRUE,
                            id = "tab1",
                            div(id="step1",
                                    fileInput("file", "Upload a single CSV file:",
                                              accept = c(
                                                  "text/csv",
                                                  "text/comma-separated-values,text/plain",
                                                  ".csv")
                                    )
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
                        box(width=12,
                            title = span( icon("folder-open"), "Batch upload"),  status = "primary", solidHeader = TRUE,
                            div(id="step1b",
                            tags$div(class="form-group shiny-input-container", 
                                     tags$div(tags$label("Upload a folder containing CSV files:")),
                                     tags$div(tags$label("Select folder", class="btn btn-primary",
                                                         tags$input(id = "fileIn", webkitdirectory = TRUE, type = "file", style="display: none;", onchange="pressed()"))),
                                     tags$label("", id = "noFile"), # need to fix
                                     tags$div(id="fileIn_progress", class="progress progress-striped active shiny-file-input-progress",
                                              tags$div(class="progress-bar") # somehow this breaks the upload = =..
                                     )     
                            )),
                            uiOutput("batch_feedback_1"),
                            uiOutput("batch_opt_1"),
                            uiOutput("batch_opt_2"),
                            uiOutput("batch_additional_cols"),
                            uiOutput("batch_feedback_2"),
                            uiOutput("batch_feedback_3"),
                            
                            uiOutput("ug_3")
                            
                            
                        ),
                        box(width=12,
                            title = span( icon("table"), "Filter or duplicate existing dataset"), status = "primary", solidHeader = TRUE,
                            collapsible = T,
                            tabPanel("Loaded files", 
                                     uiOutput("add_subset_select"),
                                     uiOutput("add_subset_opt"),
                                     uiOutput("add_subset_preview"),
                                     uiOutput("add_subset_name"),
                                     
                                     uiOutput("add_subset_confirm"),
                            )
                        )
                    ),

                    column(width = 6,
                           
                    box(width=12,
                        title = span( icon("tasks"), "Manage Datasets"), status = "primary", solidHeader = TRUE,
                        id = "tab2",
                        tabPanel("Loaded files", 
                                 div(id="step2", uiOutput("delete_deg")),
                                 uiOutput("delete_deg_confirm"),
                                 )
                    )
                    )
                    
                )
                
        )
        # ,
        # #### single body----------------------------------------
        # 
        # tabItem(tabName = "sb_single",
        #         h2("Plot"),
        #         
        #         fluidRow(
        #             tabBox(
        #                 title = NULL, width = 8,
        #                 # The id lets us use input$tabset1 on the server to find the current tab
        #                 id = "tabset1", height = "250px",
        #                 tabPanel("GSEA table", 
        #                          # h5(strong("2- Subset data"), style = "font-size:18px;"),
        #                          
        #                          # uiOutput("deg_subset_mode"),
        #                          # 
        #                          # uiOutput("gl_1"),
        #                          # uiOutput("gl_2"), 
        #                          
        #                          ),
        #                 tabPanel("Table", "Parameters Used: rnk=(), gmt=()")
        #             ),
        #             infoBoxOutput("tabset1Selected"), infoBoxOutput("tabset2Selected")
        #         ),
        #         
        #         fluidRow(
        #             tabBox(
        #                 title = NULL, width = 4,
        #                 id = "tab1", height = "250px",
        #                 tabPanel("Plot", 
        #                          "Plot",
        #                          div(
        #                              style = "position: absolute; left: 2em; bottom:2em;",
        #                              dropdown(
        #                                  radioGroupButtons(
        #                                      inputId = "box5.1",
        #                                      label = "Change time", 
        #                                      choiceNames = c("Year", "Quarter", "Month"), 
        #                                      choiceValues = c("year", "yearquarter_adm", "yearmonth_adm"), 
        #                                      selected = "year", 
        #                                      direction = "vertical"
        #                                  ),
        #                                  radioGroupButtons(
        #                                      inputId = "box5.2",
        #                                      label = "Change plot", 
        #                                      choiceNames = c("Count", "Proportion"), 
        #                                      choiceValues = c("dodge", "fill"), 
        #                                      selected = "dodge", 
        #                                      direction = "vertical"
        #                                  ),
        #                                  size = "xs",
        #                                  icon = icon("gear", class = "opt"), 
        #                                  up = TRUE
        #                              )
        #                          ),
        #                          div(
        #                              style = "position: absolute; left: 5.5em;bottom: 2em;",
        #                              dropdown(
        #                                  downloadButton(outputId = "down_box_5", label = "Download plot"),
        #                                  size = "xs",
        #                                  icon = icon("download", class = "opt"), 
        #                                  up = TRUE
        #                              )
        #                          )
        #                          
        #                 )
        #                 
        #             ),
                    # tabBox(
                    #     title = NULL, width = 4,
                    #     id = "tab1", height = "250px",
                    #     tabPanel("Plot", "Plot")
                    # ),
        #             tabBox(
        #                 title = NULL, width = 4,
        #                 id = "tab1", height = "250px",
        #                 tabPanel("Plot", "Plot")
        #             )
        #         )
        #         
        #         
        # )
    ),
)

# ====================== Single ======================
sidebar2 <- dashboardSidebar(
    width = 300,
    sidebarMenu(id="single_sb", 
                
                

        div(id="stepb1", uiOutput("select_df"))
        ,
        div(id="stepb2", uiOutput("deg_subset_mode")),
        div(id="stepb3", uiOutput("x_confirm"))

    )
)
body2 <- dashboardBody(
    h2("Single dataset"),
    tags$hr(style="border-color: grey;"),
    verbatimTextOutput("odataset"),
    
    div(style = "position:absolute;right:1em;top:1em", 
        actionButton("help2", "Help"),
    ),
    
    fluidRow(
        div(
            id = "single_main_panel",
            column(
                width = 12,
                
                div(id="stepb4",style="height:400px",
                    uiOutput("single_panels")
                )

            )
        )
    )
)

# ====================== Two way ======================
sidebar3 <- dashboardSidebar(
    width = 300,
    sidebarMenu(id="xy_sb",
                
                div(id="stepc1",
                    uiOutput("select_x"),
                    uiOutput("select_y")
                ),
                div(id="stepc2", style="height:60px",
                    uiOutput("xy_shared")
                ),
                div(id="stepc3", 
                uiOutput("xy_confirm")
                )
                
    )
)
body3 <- dashboardBody(
    h2("Two datasets"),
    tags$hr(style="border-color: grey;"),
    
    div(style = "position:absolute;right:1em;top:1em", 
        actionButton("help3", "Help"),
    ),
    
    # verbatimTextOutput("debugxy"),
    
    fluidRow(
        div(
            id = "xy_main_panel",
            column(
                width = 12,
                
                div(id="stepc4", style="height:400px",
                    uiOutput("xy_panels")
                    )
                
            )
        )
    )
)

# ====================== Multiple way ======================
sidebar4 <- dashboardSidebar(
    width = 300,
    sidebarMenu(id="n_sb",
                div(id="stepd1", uiOutput("select_df_p2")),
                div(id="stepd2", style="height:60px",
                    # textOutput("n_shared_cols"),
                    # textOutput("n_shared_rows")
                    uiOutput("n_shared")
                    ),
                
                div(id="stepd3",
                    uiOutput("n_use_data")
                    )
                

                
    )
)
body4 <- dashboardBody(
    h2("Multiple datasets"),
    tags$hr(style="border-color: grey;"),
    
    div(style = "position:absolute;right:1em;top:1em", 
        actionButton("help4", "Help"),
    ),
    
    # verbatimTextOutput("debug2"),
    
    fluidRow(
        div(
            id = "n_main_panel",
            column(
                width = 12,
                div(id="stepd4", style="height:400px",
                    uiOutput("n_panels")
                    )
                
            )
        )
    )
)


# Put them together into a dashboardPage
ui <- fluidPage(rintrojs::introjsUI(), useShinyjs(),
                #includeScript("./www/text.js"),
                #HTML("<script type='text/javascript' src='getFolders.js'></script>"),
                
    # Application title
    navbarPage("easyVizR", id="tabs",
               theme = shinytheme("flatly"),
               
               tabPanel("Upload files", 
                        tags$style(type = "text/css", "#map {height: calc(100vh - 53px) !important;}"),
                        dashboardPage(
                            
                            # dashboardHeader(title = "Run GSEA",titleWidth = 300),
                            dashboardHeader(disable = T),
                            skin = "black",
                            
                            sidebar1,
                            body1
                        ),
                        tags$style(type = "text/css", ".container-fluid {padding-left:0px;
                    padding-right:0px;}"),
                        tags$style(type = "text/css", ".navbar {margin-bottom: 0px;}"),
                        tags$style(type = "text/css", ".container-fluid .navbar-header .navbar-brand {margin-left: 0px;}")
               )
               ,
               tabPanel("Single Dataset",
                        dashboardPage(
                   # dashboardHeader(title = "Enrichment Plots",titleWidth = 300),
                   dashboardHeader(disable = TRUE),
                   skin = "black",
                   sidebar2,
                   body2
               )),
               tabPanel("Two Datasets",dashboardPage(
                   # dashboardHeader(title = "Enrichment Plots",titleWidth = 300),
                   dashboardHeader(disable = TRUE),
                   skin = "black",
                   sidebar3,
                   body3
               )),
               tabPanel("Multiple Datasets",dashboardPage(
                   # dashboardHeader(title = "Enrichment Plots",titleWidth = 300),
                   dashboardHeader(disable = TRUE),
                   skin = "black",
                   sidebar4,
                   body4
               ))
    )
    )