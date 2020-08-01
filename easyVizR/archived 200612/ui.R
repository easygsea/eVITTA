library(shinythemes)

# ==== ui.R START ===============================================================    
# Define UI for application 
ui <- 
    
    # Application title
    navbarPage("EasyVizR",theme = shinytheme("flatly"),
    
   ####----------------------upload data---------------------------####
   tabPanel("Upload and Organize data",fluidPage(
       
       sidebarLayout(
           sidebarPanel(
               
               h5(strong("Upload data"), style = "font-size:18px;"),
               em("NOTE: matrix must contain columns for 1) gene name, 2) logFC, 3) p-value and 4) FDR/ q-value."),
               br(),
               br(),
               
               
               
               fileInput("file", "Upload your own CSV file:",
                         accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
               
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
           mainPanel(
               br(),
               #tags$hr(style="border-color: grey;"),
               h5(strong("Manage data"), style = "font-size:18px;"),
               
               uiOutput("delete_deg"),
               uiOutput("delete_deg_confirm"),

               
               
               
           )
       )
   )),
               
               
               
    ####----------------------single DEG list---------------------------####
    tabPanel("Single Dataset",fluidPage(

    # Sidebar 
    sidebarLayout(
        # Sidebar Panel
        sidebarPanel(
            h5(strong("Select data"), style = "font-size:18px;"),
            uiOutput("select_df"),
            uiOutput("deg_subset_mode"),
            uiOutput("x_confirm"),
        ),
        
        # Main Panel 
        mainPanel(
            
            #-----------------table----------------------#


            verbatimTextOutput("odataset"),
            
            
            
            uiOutput("single_main"),
            br(),
            
            #----------------plots--------------------#
            
            
            
            br(),
            
            uiOutput("text1"),
            
            
        )
    )
)),
####---------------------- two DEG lists---------------------------####
tabPanel("Two Datasets",fluidPage(
    
    sidebarLayout(
        sidebarPanel(
            
            h5(strong("Select data"), style = "font-size:18px;"),
            uiOutput("select_x"),
            uiOutput("select_y"),
            
            uiOutput("xy_shared_cols"),
            uiOutput("xy_shared_rows"),
            
            uiOutput("xy_confirm"),
            
            
            

            
        ),
        mainPanel(
            
            uiOutput("xy_main"),
            
            
            
        )
    )
)),


    ####---------------------- multiple DEG lists---------------------------####
    tabPanel("Multiple Datasets",fluidPage(
    
    sidebarLayout(
        sidebarPanel(
            
            h5(strong("Select data"), style = "font-size:18px;"),
            
            uiOutput("select_df_p2"),
            textOutput("n_shared_cols"),
            textOutput("n_shared_rows"),
            
            uiOutput("n_use_data"),
            
            
            
            
            
        ),
        mainPanel(
            verbatimTextOutput("debug2"),br(),
            
            uiOutput("n_main"),
            
            
            
            
            
            
        )
    )
))




)

# ===================================================== ui.R END ================
