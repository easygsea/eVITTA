# ==== server.R START ===========================================================
# Define server logic 
# To access any input use input$[inputId] 
#                     ex. input$G_groups (the first select input value)
# To assign any output use output$[outputId] output$
#                      ex. output$myplot (assign the plot output)


server <- function(input, output, session) {
    
    waiter_hide() # will hide *on_load waiter
    
    
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
    

    
    
    
    #======================================================================#
    ####                         SINGLE WAY                             ####
    #======================================================================#
    
    # source("server/x_events.R", local = TRUE)
    # source("server/x_ui.R", local = TRUE)
    # source("server/x_vis.R", local = TRUE)
    
    
    
    
    
    
    
    # # two-way is obsolete for now
    # source("server/xy.R", local = TRUE)
    
    

    
    #======================================================================#
    ####                        MULTIPLE WAY                            ####
    #======================================================================#
    
    source("server/n_events.R", local = TRUE)
    source("server/n_ui.R", local = TRUE)
    
    
    # 3 tabs:
    source("server/n_vis_heatmap.R", local = TRUE)
    source("server/n_vis_intersect.R", local = TRUE)
    source("server/n_vis_scatter.R", local = TRUE)
    source("server/n_vis_single.R", local = TRUE)
    source("server/n_vis_network.R", local = TRUE)
    

    
    
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
