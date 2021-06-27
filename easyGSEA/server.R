# sets max upload size to 50MB, modal appears when it exceeds 10MB,
# see 1.1.server-run.R line 325 for more details
options(shiny.maxRequestSize=50*1024^2) 

server <- function(input, output, session) {
    # # a lovely reminder to remind the user about gprofiler
    # showModal(modalDialog(title = tags$h3("Please be aware:"),
    #                                               tags$h4("Our ID conversion function is not working properly. ")
    #                                               ,br(),
    #                                               size = "m",
    #                                               easyClose = TRUE
    #                                               ,footer = modalButton("OK")))
    waiter_hide() # will hide *on_load waiter

    runjs("$('#rnkfile').parent().removeClass('btn-default').addClass('btn-danger');")

    #=====================================================#
    ####        sourced data processing R scripts      ####
    #=====================================================#
    # essential reactive values
    source("server/server-rv.R", local = TRUE)
    
    # codes for the demo session. uncomment for a demo run
    source("server/demo.R", local = TRUE)
    
    # essential static variables
    source("server/server-variables.R", local = TRUE)
    
    # essential reactive variables
    source("server/server-reactives.R", local = TRUE)

    # RAM check on session start
    source("server/server-ramCheck.R", local = TRUE)
    
    # functions: visualizations, UI, data processing
    source("server/server-functions.R", local = TRUE)
    
    # intro tour
    source("server/help.R", local = TRUE) #intro tour#
    
    # GSEA/ORA data processing
    source("server/1.1.server-run.R", local = TRUE)
    source("server/1.2.server-body1.R", local = TRUE)
    
    # Visualizations
    source("server/2.server-results.R", local = TRUE)
    source("server/3.server-network.R", local = TRUE)
    
    # Data download
    source("server/4.server-download.R", local = TRUE)
    
    #=====================================================#
    ####           toggle button for a demo runs       ####
    #=====================================================#
    output$btn_demo <- renderUI({
        req(is.null(rv$demo_yes))
        btn_demo("ee")
    })
    
    observeEvent(input$ee,{
        btn_demo_e()
    })
    
    #=====================================================#
    ####            UI to download example files       ####
    #=====================================================#
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
    
    # CLEAR MEMORY UPON SESSION CLOSE
    onStop(fun = function(){
        gc()
    })
}
