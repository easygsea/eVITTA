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
                
                menuItem("Single Dataset", tabName="tab2", icon=icon("vial")),
                
                menuItem("Multiple Datasets", tabName="tab3", icon=icon("vials")),
                
                tags$hr(style="border-color: #808080; margin:10px;"),
                
                
                # ---------------- options panels -------------------
                
                # these are options that will only show up upon selecting certain tabs.
                # these are hidden initially with shinyjs, 
                # and enabled in server side when tabs are selected.
                # (this is to prevent empty white boxes from flashing upon startup)
                
                shinyjs::hidden(
                    div(id="sidebar_opt", 
                        
                        sideopt2,
                        
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
  
    rintrojs::introjsUI(), # introjs
    useShinyjs(), # shinyJS
    
    # use_waiter(), # waiter
    # waiter_show_on_load(tagList(spin_fading_circles(),h4(loadMsg))), # shows before anything else

    
    tabItems(
      
      body1,
      
      body2, 
      
      body3

    )
)

#======================================================================#
####                   ASSEMBLE DASHBOARD PAGE                      ####
#======================================================================#

shinyUI(
  dashboardPage(
    title="easyVizR",
    dashboardHeader(title = "easyVizR", dropdownMenuOutput("dropdown_menu")),
    sidebar,
    body
  )
)



# ===================================================== ui.R END ============