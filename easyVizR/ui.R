# ==== ui.R START ===========================================================

# 
# This is the static framework of the UI.
# Dynamic UI elements are defined in server.R, rendered using renderUI, and called using uiOutput.
# 


source("ui/body1.R")
source("ui/body_filters.R")

source("ui/body_ins.R")
source("ui/body2.R")
source("ui/body3.R")
source("ui/sideopt2.R")
source("ui/sideopt3.R")
source("ui/css_addons.R")



#======================================================================#
####                           SIDEBAR                              ####
#======================================================================#

sidebar <- dashboardSidebar(
    sidebarMenu(id="tabs",
                
                # ---------------- tabs -------------------
                
                menuItem("1. Organize Data", tabName="tab1", icon=icon("th-list")),
                
                # menuItem("Single Dataset", tabName="tab2", icon=icon("vial")),
                menuItem(text = span(id = "tab2_text", "2. Apply Filters"), tabName="tab_filters", icon=icon("cut")),
                menuItem(text = span(id = "tab3_text", "3. Select Intersection"), tabName="tab_ins", icon=icon("map-marker")),
                
                menuItem(text = span(id = "tab4_text", "4. Visualize Intersection"), tabName="tab3", icon=icon("vials")),
                
                tags$hr(style="border-color: #48617b;margin: 8px;")
                
                ,uiOutput("btn_demo")
                ,uiOutput("download_sample_data")
                
                
                # # ---------------- options panels -------------------
                # 
                # # these are options that will only show up upon selecting certain tabs.
                # # these are hidden initially with shinyjs, 
                # # and enabled in server side when tabs are selected.
                # # (this is to prevent empty white boxes from flashing upon startup)
                # 
                # shinyjs::hidden(
                #     div(id="sidebar_opt", 
                #         
                #         # sideopt2,
                #         
                #         # sideopt3
                #     )
                # )
                
    )
    ,disconnectMessage(text = "Your session has timed out. Please refresh page and start again. For bug report, email us at evitta@cmmt.ubc.ca. Thank you for your support.")
    

)


#======================================================================#
####                            BODY                                ####
#======================================================================#


body <- dashboardBody(
    # -------------- Load Dependencies -------------------
  
    # link to stylesheet
    tags$head(
      includeHTML(("google-analytics.html")),
      #Added get IP here!
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      ######Added get IP here!
      tags$script(src="getIP.js")
    ),
    
    # apply specific css adjustments additionally
    css_addons,
  
  
    rintrojs::introjsUI(), # introjs
    useShinyjs(), # shinyJS
    extendShinyjs(text = jscode, functions = c("winprint", "collapse")),
    
    use_waiter(), # waiter
    waiter_show_on_load(tagList(spin_three_bounce(),h4(loadMsg)), color = "#1976D2"), # shows before anything else 

    
    tabItems(
      
      body1,
      
      body_filters,
      
      body_ins,
      
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