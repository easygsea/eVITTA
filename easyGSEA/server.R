# sets max upload size to 50MB, modal appears when it exceeds 10MB,
# see 1.1.server-run.R line 325 for more details
options(shiny.maxRequestSize=50*1024^2) 

server <- function(input, output, session) {
    waiter_hide() # will hide *on_load waiter

    # addClass(selector = "body", class = "sidebar-collapse")
    runjs("$('#rnkfile').parent().removeClass('btn-default').addClass('btn-danger');")
    
    # js$hidehead('none')
    
    # toggle button for a demo run
    output$btn_demo <- renderUI({
        btn_demo("ee")
    })
    
    observeEvent(input$ee,{
        btn_demo_e()
    })
    
    source("server/server-variables.R", local = TRUE)
    source("server/server-reactives.R", local = TRUE)
    source("server/server-rv.R", local = TRUE)
    source("server/server-ramCheck.R", local = TRUE)
    source("server/server-functions.R", local = TRUE)
    source("server/help.R", local = TRUE) #intro tour#
    source("server/1.1.server-run.R", local = TRUE)
    source("server/1.2.server-body1.R", local = TRUE)
    
    source("server/2.server-results.R", local = TRUE)
    # source("server/server-summary.R", local = TRUE)
    source("server/3.server-network.R", local = TRUE)
    source("server/4.server-download.R", local = TRUE)
    
    
}
