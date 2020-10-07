source("ui/1.ui-run.R")
source("ui/2.ui-results.R")
source("ui/3.ui-network.R")
# source("ui/ui-summary.R")
source("ui/4.ui-download.R")
source("ui/5.ui-help.R")
source("ui/css_addons.R")

sidebar <- dashboardSidebar(
    sidebarMenu(id="tabs",
        menuItem("1. Run Analysis", tabName = "gsea", icon = icon("play")), #code-branch align-left
        a_mode,
        menuItem("2. Enrichment Results", tabName = "kegg", icon = icon("bar-chart")), #fingerprint
        menuItem("3. Enrichment Network", tabName = "network", icon = icon("project-diagram")),
        # menuItem("Enrichment Summary", tabName = "summary", icon = icon("table")),
        menuItem("4. Download", tabName = "download", icon = icon("download")),
        a_download
        # ,menuItem("Help", tabName = "help", icon = icon("info-circle"))
        # ,a_example

    ),
    disconnectMessage(text = "Your session has timed out. Please refresh page and start again. For bug report, email us at evitta@cmmt.ubc.ca. Thank you for your support.")
)

loadMsg = "easyGSEA - gene set enrichment analysis, interpretation & visualization"

shinyUI(
    dashboardPage(
        #Added a Disconnect Message here: 2020-09-18
        
        title="easyGSEA - gene set enrichment analysis, interpretation & visualization",

        dashboardHeader(title = "easyGSEA",
                        tags$li(class = "dropdown", actionButton("home", "eVITTA Home",icon("home"),
                                                                 style="color: #fff; background-color: transparent; border-color: #c0d3e7; margin-top:8px; margin-right:8px; border-radius:2rem; border:0.125rem solid #fff",
                                                                 onclick ="location.href='http://tau.cmmt.ubc.ca/eVITTA/';"))
                        ), #,titleWidth = 200
        # skin = "black",
        sidebar,
        dashboardBody(
            # theme = shinytheme("flatly"),

            use_waiter(), # dependencies
            waiter_show_on_load(tagList(spin_three_bounce(),h4(loadMsg)), color = "#1976D2"), # shows before anything else

            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                HTML("<script type='text/javascript' language='javascript' src='https://reactome.org/DiagramJs/diagram/diagram.nocache.js'></script>"),
                tags$script(src = "reactome.js")
            ),
            
            # # this is used to reset input values
            # example usage: session$sendCustomMessage(type = "resetValue", message = "gene_column")
            tags$script("Shiny.addCustomMessageHandler('resetValue', function(variableName) {
            Shiny.onInputChange(variableName, null);});"),
            
            useShinyalert(),  # Set up shinyalert
            useShinyjs(), # Set up shinyjs

            tabItems(
                bodyGSEA,
                bodyResults,
                bodyNetwork,
                # bodySummary,
                bodyDownload,
                bodyHelp
            )
            # tags$footer(HTML("<b>Taubert Lab</b> | BC Children's Hospital Research Institute | Centre for Molecular Medicine and Therapeutics | University of British Columbia. 2019-2020. All Rights Reserved."),
            #             align = "left", style = "
            #   position:absolute;
            #   bottom:0;
            #   width:100%;
            #   height:30px;
            #   color: white;
            #   padding: 5px;
            #   background-color: #3179ae;
            #   z-index: 1000;")


            # apply specific css adjustments additionally
            ,css_addons
        )

    )
    


)
