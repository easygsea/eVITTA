# sets max upload size to 500MB, modal appears when it exceeds 50MB,
# see 1.1.server-run.R line 325 for more details
options(shiny.maxRequestSize=500*1024^2) 

server <- function(input, output, session) {
    waiter_hide() # will hide *on_load waiter

    # addClass(selector = "body", class = "sidebar-collapse")
    runjs("$('#rnkfile').parent().removeClass('btn-default').addClass('btn-danger');")
    
    # js$hidehead('none')
    
    source("server/server-variables.R", local = TRUE)
    source("server/server-reactives.R", local = TRUE)
    source("server/server-rv.R", local = TRUE)
    source("server/server-ramCheck.R", local = TRUE)
    source("server/server-functions.R", local = TRUE)
    source("server/1.1.server-run.R", local = TRUE)
    source("server/1.2.server-body1.R", local = TRUE)
    
    source("server/2.server-results.R", local = TRUE)
    # source("server/server-summary.R", local = TRUE)
    source("server/3.server-network.R", local = TRUE)
    source("server/4.server-download.R", local = TRUE)
    
    
}
