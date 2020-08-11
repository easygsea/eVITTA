source("ui/ui-menu1.R")
source("ui/ui-menu2.R")
source("ui/ui-menu3.R")
source("ui/ui-menu4.R")
source("ui/ui-menu5.R")


sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Run Analysis", tabName = "gsea", icon = icon("play")), #code-branch align-left
        menuItem("Enrichment Results", tabName = "kegg", icon = icon("table")), #fingerprint
        menuItem("Enrichment Network", tabName = "network", icon = icon("project-diagram")),
        menuItem("Download Results", tabName = "download", icon = icon("download")),
        menuItem("Help", tabName = "help", icon = icon("info-circle"))
        
    )
)

loadMsg = "easyGSEA - gene set enrichment analysis, interpretation & visualization"

shinyUI(
    dashboardPage(
        title="easyGSEA - gene set enrichment analysis, interpretation & visualization",       
        
        dashboardHeader(title = "easyGSEA"), #,titleWidth = 200
        # skin = "black",
        sidebar,
        dashboardBody(
            use_waiter(), # dependencies
            waiter_show_on_load(tagList(spin_fading_circles(),h4(loadMsg))), # shows before anything else 
            
            tags$head(
                HTML("<script type='text/javascript' language='javascript' src='https://reactome.org/DiagramJs/diagram/diagram.nocache.js'></script>"),
                tags$script(src = "reactome.js")
            ),
            useShinyjs(),
            # tags$head(
            #     tags$link(rel = "stylesheet", type = "text/css", href = "./www/custom.css")),
            
            tabItems(
                bodyGSEA,
                bodyResults,
                bodyNetwork,
                bodyDownload,
                bodyHelp
            ),
            tags$footer(HTML("<b>Taubert Lab</b> | BC Children's Hospital Research Institute | Centre for Molecular Medicine and Therapeutics | University of British Columbia. 2019-2020. All Rights Reserved."),
                        align = "left", style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:30px;
              color: white;
              padding: 5px;
              background-color: #3179ae;
              z-index: 1000;")
            
        )
        
    )
    
    
)
