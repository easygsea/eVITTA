# ==== ui.R START ===========================================================

# 
# This is the static framework of the UI.
# Dynamic UI elements are defined in server.R, rendered using renderUI, and called using uiOutput.
# 


source("ui/body1.R")
source("ui/body2.R")
source("ui/body3.R")
source("ui/sideopt2.R")
source("ui/sideopt3.R")


#======================================================================#
####                           SIDEBAR                              ####
#======================================================================#

sidebar <- dashboardSidebar(
    sidebarMenu(id="tabs",
                
                # ---------------- tabs -------------------
                
                menuItem("Organize Files", tabName="tab1", icon=icon("th-list")),
                
                # menuItem("Single Dataset", tabName="tab2", icon=icon("vial")),
                
                menuItem("Multiple Datasets", tabName="tab3", icon=icon("vials")),
                
                tags$hr(style="border-color: #48617b;margin: 8px;"),
                
                
                # ---------------- options panels -------------------
                
                # these are options that will only show up upon selecting certain tabs.
                # these are hidden initially with shinyjs, 
                # and enabled in server side when tabs are selected.
                # (this is to prevent empty white boxes from flashing upon startup)
                
                shinyjs::hidden(
                    div(id="sidebar_opt", 
                        
                        # sideopt2,
                        
                        sideopt3
                    )
                )
                
    )

)


#======================================================================#
####                            BODY                                ####
#======================================================================#


body <- dashboardBody(
  
    # -------------- Load Dependencies -------------------
  
    # link to stylesheet
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    # apply specific css adjustments additionally
    tags$head(
      tags$style(HTML(paste0(
      
      # fixes large datatables flashing and adds margin on bottom
      "#n_ins_tbl{min-height: 480px;margin-bottom: 30px;}
      #single_tbl{min-height: 480px;margin-bottom: 30px;}
      #single_gl_tbl{min-height: 480px;margin-bottom: 30px;}"
      ,
      # fixes textareainput box in multiple dropdown
      "#n_igl{width: 200px;height: 100px;overflow-y: scroll;resize: none;}"
      ,
      # fixes visnetwork footer
      "#vis_network{margin-bottom:35px;}"
      
    )))
    ),
  
  
    rintrojs::introjsUI(), # introjs
    useShinyjs(), # shinyJS
    
    use_waiter(), # waiter
    waiter_show_on_load(tagList(spin_three_bounce(),h4(loadMsg)), color = "#1976D2"), # shows before anything else 

    
    tabItems(
      
      body1,
      
      # body2, 
      
      body3

    )
)

#======================================================================#
####                   ASSEMBLE DASHBOARD PAGE                      ####
#======================================================================#

shinyUI(
  dashboardPage(
    title="easyVizR",
    dashboardHeader(title = "easyVizR",
                    tags$li(class = "dropdown", actionButton("home", "eVITTA Home",icon("home"), 
                                                             style="color: #fff; background-color: transparent; border-color: #c0d3e7; margin-top:8px; margin-right:8px; border-radius:2rem; border:0.125rem solid #fff",
                                                             onclick ="location.href='http://tau.cmmt.ubc.ca/eVITTA/';"))
                    ),
    sidebar,
    body
  )
)



# ===================================================== ui.R END ============