# options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output, session) {
    waiter_hide() # will hide *on_load waiter
    
    # addClass(selector = "body", class = "sidebar-collapse")
    runjs("$('#rnkfile').parent().removeClass('btn-default').addClass('btn-danger');")
    
    # js$hidehead('none')
    
    source("server/server-rv.R", local = TRUE)
    source("server/server-functions.R", local = TRUE)
    source("server/server-run.R", local = TRUE)
    source("server/server-body1.R", local = TRUE)
    
    source("server/server-vis.R", local = TRUE)
    # source("server/server-summary.R", local = TRUE)
    source("server/server-network.R", local = TRUE)
    source("server/server-download.R", local = TRUE)
}
