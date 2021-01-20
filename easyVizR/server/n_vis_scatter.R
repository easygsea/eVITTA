
#======================================================================#
####                       4.2.1 XY SCATTER                          ####
#======================================================================#

####--------------- OPTIONS -------------------####


# colormode options (shows different panels conditionally)
output$nxy_colormode_options <- renderUI({
  req(is.null(n_ins_full())==F)
  req(rv$nxy_colormode !="None")
  if(rv$nxy_colormode =="Two colors"){
    div(
      "Color threshold options:",
      fluidRow(
        column(6,
               numericInput("nxy_p", 
                            "P <:", value = rv$nxy_p, min = 0, max = 1, step=0.001, width="100px"),
        ),
        column(6,
               numericInput("nxy_q", 
                            "FDR <:", value = rv$nxy_q, min = 0, max = 1, step=0.001, width="100px"),
        ),
      ),
      fluidRow(
        column(6,
               numericInput("nxy_stat", 
                            stat_replace1("|Stat| >:",c(rv$nxy_selected_x, rv$nxy_selected_y)),
                            value = rv$nxy_stat, min = 0, max = 10, step=0.1, width="100px"),
        ),
        column(6,
               radioGroupButtons("n_sc_logic",
                                 label = HTML(paste0(
                                   "Color logic:",
                                   add_help("n_sc_logic_help", style="margin-left: 5px;"))
                                 ),
                                 choices=c("OR" ="Either", "AND" = "Both"),
                                 selected=rv$n_sc_logic,size="s"), 
               bsTooltip("n_sc_logic_help", 
                         "<b>AND</b>: highlights if conditions are met for <b>ALL</b> datasets.<br><b>OR</b>: highlights if conditions are met for <b>ANY</b> dataset.", 
                         placement = "right"),
        )
      ),
      
      # uiOutput("nxyz_logic_caption"),
    )
  } else if (rv$nxy_colormode =="Color and size"){
    div(
      radioButtons(
        inputId = "nxy_sig",
        label = "Significance:",
        choices = c("PValue", "FDR"),
        selected=rv$nxy_sig, inline=T)
    )
    
  }
})

output$nxy_logic_caption <- renderUI({
  if (rv$n_sc_logic == "Both"){
    HTML("<strong>AND</strong>: highlights if conditions are met for <strong>ALL</strong> datasets.")
  } else if (rv$n_sc_logic == "Either"){
    HTML("<strong>OR</strong>: highlights if conditions are met for <strong>ANY</strong> dataset.")
  }
})
output$nxyz_logic_caption <- renderUI({
  if (rv$n_sc_logic == "Both"){
    HTML("<strong>AND</strong>: highlights if conditions are met for <strong>ALL</strong> datasets.")
  } else if (rv$n_sc_logic == "Either"){
    HTML("<strong>OR</strong>: highlights if conditions are met for <strong>ANY</strong> dataset.")
  }
})

####--------------- XY SCATTER -------------------####


nxy_sc_plt <- reactive({
  req_vars(c(
    n_ins_full(), n_ins_gls(), rv$ins_criteria, df_n_basic(), rv$df_n,
    rv$nxy_selected_x, rv$nxy_selected_y,
    rv$nxy_sc_dflogic
    ), check_len=T)
  
  
  selected <- c(rv$nxy_selected_x, rv$nxy_selected_y)
  to_plot_df <- get_df_by_dflogic(selected, dflogic = rv$nxy_sc_dflogic, 
                                  gls = n_ins_gls(),
                                  user_criteria = rv$ins_criteria,
                                  starting_df =df_n_basic()
                                  )
    
  fig <- draw_xy_scatter(to_plot_df, selected)
  fig

})


output$df_nxy_scatter <- renderPlotly({
  
  req(is.null(n_ins_full())==F)
  req(is.null(rv$nxy_colormode)==F)
  validate(need(nrow(n_ins_full()) > 0, "Selected intersection is empty; please double check your selection in Intersection of Interest"))
  req(nrow(n_ins_full()) > 0)
  nxy_sc_plt()
})



# download plotly html graph
output$scatter_nxy_dl <- downloadHandler(
  filename = function() {paste("scatter-multiple-xy-", Sys.Date(), ".html", sep = "")},
  content = function(file) {saveWidget(as_widget(nxy_sc_plt()), file, selfcontained = TRUE)})



# ---------------- color summary table
# ui
output$nxy_color_summary_panel <- renderUI({
  if (rv$nxy_colormode!="Color and size"){
    div(
      HTML(paste0(
        "<b>Color summary</b>:",
        add_help("nxy_cdf_help", style="margin-left: 5px;"))
      ),
      bsTooltip("nxy_cdf_help", 
                "Summarizes terms by their displayed color", 
                placement = "top"),
      dataTableOutput("nxy_color_tbl"),
      br(),
      downloadButton("download_nxy_color_df", "Download color summary"),
    )
  } else {
    div(
      "Color summary is only available for discrete color schemes."
    )
  }
})


nxy_color_df <- reactive({ rv$nxy_color })
# summary table
output$nxy_color_tbl <- DT::renderDataTable({
  nxy_color_df()
}, plugins="ellipsis",
options=list(scrollX=T, scrollY=T, paging = FALSE, searching = FALSE, info=FALSE,
             columnDefs = list(
               list(
                 targets = "_all",
                 render = JS("$.fn.dataTable.render.ellipsis( 36, false )")
               ))
))

# download summary table
output$download_nxy_color_df <- downloadHandler(
  filename = function() {
    paste("colors-scatter-multiple-", Sys.Date(), ".csv", sep="")},
  content = function(file) {
    write.csv(nxy_color_df(), file, 
              row.names = T, quote=TRUE)})



####--------------- correlation -------------------####


# correlation line
output$nxy_corline <- renderUI({
  req(is.null(rv$fit_nxy)==F)
  req(is.null(nxy_sc_plt())==F)
  
  if (length(coef(rv$fit_nxy))>1){
    intercept = format(round(coef(rv$fit_nxy)[[1]], 2), nsmall = 2)
    slope = format(round(coef(rv$fit_nxy)[[2]], 2), nsmall = 2)
    r2= format(round(summary(rv$fit_nxy)$r.squared, 2), nsmall = 2)
    box(
      title = NULL, background = "aqua", solidHeader = TRUE, width=12,
      strong("Correlation line:"),br(),
      column( 12,align="center" ,
              paste0("y = ", slope,"x + ",intercept), br(),
              paste0("(R^2 = ", r2,")")
      )
      
    )
  } else {
    box(
      title = NULL, background = "yellow", solidHeader = TRUE, width=12,
      strong("Your selection is invalid.")
      
    )
  }
  
})
output$nxy_cor_summary <- renderPrint({
  req(is.null(n_ins_full())==F)
  summary(rv$fit_nxy)
})



#======================================================================#
####                       4.2.2 3D SCATTER                          ####
#======================================================================#

# colormode options (shows different panels conditionally)
output$nxyz_colormode_options <- renderUI({
  req(is.null(n_ins_full())==F)
  req(rv$nxyz_colormode !="None")
  if(rv$nxyz_colormode =="Two colors"){
    div(
      "Color threshold options:",
      fluidRow(
        column(6,
               numericInput("n_3ds_p", 
                            "P <:", value = rv$n_3ds_p, min = 0, max = 1, step=0.001, width="100px"),
        ),
        column(6,
               numericInput("n_3ds_q", 
                            "FDR <:", value = rv$n_3ds_q, min = 0, max = 1, step=0.001, width="100px"),
        ),
      ),
      fluidRow(
        column(6,
               numericInput("n_3ds_Stat", 
                            stat_replace1("|Stat| >:",c(rv$nxy_selected_x, rv$nxy_selected_y, rv$nxy_selected_z)),
                            value = rv$n_3ds_Stat, min = 0, max = 10, step=0.1, width="100px"),
               ),
        column(6,
               radioGroupButtons("nxyz_sc_logic",
                                 label = HTML(paste0(
                                   "Color logic:",
                                   add_help("nxyz_sc_logic_help", style="margin-left: 5px;"))
                                 ),
                                 choices=c("OR" ="Either", "AND" = "Both"),
                                 selected=rv$nxyz_sc_logic,size="s"), 
               bsTooltip("nxyz_sc_logic_help", 
                         "<b>AND</b>: highlights if conditions are met for <b>ALL</b> datasets.<br><b>OR</b>: highlights if conditions are met for <b>ANY</b> dataset.", 
                         placement = "right"),
        )
      ),

      # uiOutput("nxyz_logic_caption"),
    )
  } 
})




####--------------- 3d scatter -------------------####


# main graph
n_3ds_plt <- reactive({
  validate(need(nrow(n_ins_full()) > 0, "Selected intersection is empty; please double check your selection in Intersection of Interest"))
  req(nrow(n_ins_full())>0)
  req_vars(c(rv$nxy_selected_x, rv$nxy_selected_y, rv$nxy_selected_z))
  req(rv$nxy_selected_z!="None")
  
  selected <- c(rv$nxy_selected_x, rv$nxy_selected_y, rv$nxy_selected_z)
  to_plot_df <- get_df_by_dflogic(selected, dflogic = rv$nxyz_sc_dflogic, 
                                  gls = n_ins_gls(),
                                  user_criteria = rv$ins_criteria,
                                  starting_df =df_n_basic()
  )
  fig <- draw_3d_scatter(to_plot_df, selected)
})

output$df_n_3ds <- renderPlotly({
  req(rv$df_n)
  req(is.null(n_ins_full())==F)
  # req(rv$n_3ds_status=="ok")
  
  n_3ds_plt()
})

# download plotly html graph
output$n_3ds_dl <- downloadHandler(
  filename = function() {paste("scatter-multiple-", Sys.Date(), ".html", sep = "")},
  content = function(file) {saveWidget(as_widget(n_3ds_plt()), file, selfcontained = TRUE)})

# ---------------- color summary table
n_3ds_prop_df <- reactive({ rv$n_3ds_prop })
# summary table
output$n_3ds_prop_tbl <- DT::renderDataTable({
  n_3ds_prop_df()
}, plugins="ellipsis",
options=list(scrollX=T, scrollY=T, paging = FALSE, searching = FALSE, info=FALSE,
             columnDefs = list(
               list(
                 targets = "_all",
                 render = JS("$.fn.dataTable.render.ellipsis( 36, false )")
               ))
))

# download summary table
output$download_3ds_df <- downloadHandler(
  filename = function() {
    paste("summary-scatter-multiple-", Sys.Date(), ".csv", sep="")},
  content = function(file) {
    write.csv(n_3ds_prop_df(), file, 
              row.names = T, quote=TRUE)})

