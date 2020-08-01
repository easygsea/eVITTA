# options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output, session) {
    # addClass(selector = "body", class = "sidebar-collapse")
    runjs("$('#rnkfile').parent().removeClass('btn-default').addClass('btn-danger');")
    
    # js$hidehead('none')
    
    source("server/server-rv.R", local = TRUE)
    source("server/server-functions.R", local = TRUE)
    source("server/server-menu1.R", local = TRUE)
    source("server/server-body1.R", local = TRUE)

    source("server/server-menu2.R", local = TRUE)
    source("server/server-menu3.R", local = TRUE)
    source("server/server-menu4.R", local = TRUE)
}
