# sets max upload size to 50MB, modal appears when it exceeds 10MB,
# see 1.1.server-run.R line 325 for more details
options(shiny.maxRequestSize=50*1024^2) 

server <- function(input, output, session) {
    # a lovely reminder to remind the user about gprofiler
    showModal(modalDialog(title = tags$h3("Please be aware:"),
                                                  tags$h4("Our ID conversion function is not working properly. ")
                                                  ,br(),
                                                  size = "m",
                                                  easyClose = TRUE
                                                  ,footer = modalButton("OK")))
    waiter_hide() # will hide *on_load waiter

    # addClass(selector = "body", class = "sidebar-collapse")
    runjs("$('#rnkfile').parent().removeClass('btn-default').addClass('btn-danger');")
    
    # js$hidehead('none')
    
    # toggle button for a demo run
    output$btn_demo <- renderUI({
        req(is.null(rv$demo_yes))
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
    
    # the ui to download the files
    output$sample_data_download <- renderUI({
        req(is.null(rv$demo_yes))
        fixedPanel(
            bottom = 22,
            left = 14,
            if(is.null(input$selected_mode) || input$selected_mode == "gsea"){
                downloadLink("dataset_download","Download Sample Data", style = "color: #FFFF99;font-size:90%;" )
            } else {
                downloadLink("dataset_download_ora","Download Sample Data", style = "color: #FFFF99;font-size:90%;" )
            }
        )
    })
    output$dataset_download <- downloadHandler(
        filename = "hsa.zip",
        content = function(file){file.copy("www/demo/hsa.zip", file)}
        
    )
    output$dataset_download_ora <- downloadHandler(
        filename = "hsa_list.txt",
        content = function(file){file.copy("www/demo/hsa_list.txt", file)}
        
    )
    
}
