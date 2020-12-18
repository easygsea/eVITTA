# sets max upload size to 100MB, modal appears when it exceeds 50MB for batch upload and 10MB for single upload,
options(shiny.maxRequestSize=100*1024^2) 
##Add a RV value for IP Address
#users = reactiveValues(count = 0, ip = c())

# ==== server.R START ===========================================================
# Define server logic 
# To access any input use input$[inputId] 
#                     ex. input$G_groups (the first select input value)
# To assign any output use output$[outputId] output$
#                      ex. output$myplot (assign the plot output)


server <- function(input, output, session) {
    # toggle button for a demo run
    output$btn_demo <- renderUI({
        btn_demo("ee")
    })
    
    observeEvent(input$ee,{
        btn_demo_e()
    })
    
    #during the start of the session
    onSessionStart = {
        IP <- reactive({ input$getIP })
        #users$count <<- users$count + 1
        observeEvent(input$getIP,{
            result <- toJSON(IP())
            result <- fromJSON(result)    
            if(result$geoplugin_request %in% ips){
                showModal(modalDialog(
                    title = "You already have an open session",
                    "Please close this session and use your open session",
                    easyClose = F,
                    footer = uiOutput("I don't want a footer")
                ))
            }else{
                ips<<-c(ips,result$geoplugin_request)
                #users$ip<<-c(users$ip,result$geoplugin_request)
            }
            result <- NULL
        })
        #users$ip<-append(users$ip,singleIP())
    }
    
    onSessionEnded(function() {
        {
            #print(ips)
            IPtoDelete <- ""
            isolate({IP <- reactive({ input$getIP })
            IPtoDelete <- toJSON(IP())
            IPtoDelete <- fromJSON(IPtoDelete)
            IPtoDelete <- as.data.frame(IPtoDelete)
            })
            #From google IP Address is unique, so we should be good to go
            #users$ip <<- users$ip[users$ip != result$geoplugin_request] 
            #users$count <<- users$count - 1
            #print(typeof(IPtoDelete))
            ips<<-ips[ips != IPtoDelete$geoplugin_request]
            IPtoDelete <- NULL
        }
    })
    
    
    
    
    
    
    
    waiter_hide() # will hide *on_load waiter
    
    # ram check on initialization
    source("server/server-ramCheck.R", local = TRUE)
    
    # initialize reactive values
    source("server/rv.R", local = TRUE)
    
    # server side functions
    source("server/functions.R", local = TRUE)
    
    
    #======================================================================#
    ####                        INTRO TOUR                              ####
    #======================================================================#
    
    
    source("server/help.R", local = TRUE)
    
    

    
    #======================================================================#
    ####                      ORGANIZE FILES                            ####
    #======================================================================#
    
    source("server/organize.R", local = TRUE)
    

    
    
    # single way and two way are obsolete now
    
    # source("server/x_events.R", local = TRUE)
    # source("server/x_ui.R", local = TRUE)
    # source("server/x_vis.R", local = TRUE)
    # source("server/xy.R", local = TRUE)
    
    #======================================================================#
    ####                        MULTIPLE ANALYSIS                       ####
    #======================================================================#
    
    source("server/multi_events.R", local = TRUE) # data processing
    source("server/multi_shared_widgets.R", local = TRUE) # filter button and ins table
    
    #========================= SELECT FILTERS ==============================#
    
    source("server/f_gls.R", local = TRUE)
    
    
    #========================= INTERSECTION ==============================#
    
    source("server/i_ins.R", local = TRUE)
    

    #========================= VISUALIZATIONS ==============================#

    source("server/n_ui.R", local = TRUE)
    
    
    # vis tabs:
    source("server/n_vis_heatmap.R", local = TRUE)
    source("server/n_vis_scatter.R", local = TRUE)
    source("server/n_vis_single.R", local = TRUE)
    source("server/n_vis_network.R", local = TRUE)
    
    #download sample data function and ui
    output$download_sample_data <- renderUI({
        req(is.null(rv$demo_yes))
        fixedPanel(
            bottom = 22,
            left = 12,
            downloadLink("dataset_download","Download Sample Data", style = "color: #FFFF99;font-size:90%;" ) #color: #FFFF99;
        )
    })
    output$dataset_download <- downloadHandler(
        filename = "demo.zip",
        content = function(file){
            file.copy("www/demo.zip", file)
        }
    )
    

    
    
    ####------------------- DEBUG -----------------------####
    
    # DEBUG (single)
    output$odataset <- renderPrint({
        paste("rv show_df"," = ", rv$show_df, ", ",
              "rv_genelist"," = ", rv$genelist, ", ",
              "input$genelist_p1"," = ", input$genelist_p1, ", ",
              "Table index"," = ", current(), ",",
              "rv$x_i = ", rv$x_i, ",",
              "ll size = ", length(rv$ll),
              "gg size = ", length(rv$gg))
    })
    output$debugxy <- renderPrint({
        paste("rv$selected_x = ", rv$selected_x, ", ",
              "rv$ll = ", rv$ll, ",",
              "ll size = ", length(rv$ll), ",",
              "gg size = ", length(rv$gg))
    })
    output$debug2 <- renderPrint({
        paste("length(rv$nx_i)= ",length(rv$nx_i), ", ",
              "input$n_hm_showna = ", input$n_hm_showna, ",",
              "sorthmby = ", paste0(rv$n_to_plot,"_", rv$heatmap_sortby), ",",
              "p_cutoff = ", rv$p_cutoff_2, ",",
              "ll size = ", length(rv$ll), ",",
              "gg size = ", length(rv$gg))
    })
    output$debug0 <- renderPrint({
        paste("rv$folder_upload_state = ", rv$folder_upload_state, ", ",
              "rv$batch_files = ", rv$batch_files$name, ", ",
              "rv$batch_failed = ", rv$batch_failed, ", ",
              "ll size = ", length(rv$ll), ",",
              "gg size = ", length(rv$gg))
    })

    
    # # # pass the rv to client (required for conditional panels to use rv values)
    # output$n_len <- reactive({
    #     return(length(rv$nx_i))
    # })
    # outputOptions(output, "n_len", suspendWhenHidden = F)

    
}
# ===================================================== server.R END ============
