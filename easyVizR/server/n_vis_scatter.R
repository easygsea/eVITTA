
# ####-------------------- table ------------------------####
# 
# 
# # display table
# output$df_nxy_tbl <- DT::renderDataTable({
#   
#   df <- n_nxy_df()
#   
#   # tidy row names
#   if (nrow(df)>0){rownames(df) <- seq(1,nrow(df),1)}
#   
#   # to replace the stat col names
#   colnames(df) <- gsub("Stat", rv$tt[[rv$nx_i[[1]]]], colnames(df))
#   
#   
#   rv$df_nxy_fullcols <- colnames(df)
#   
#   # to abbreviate the long column names...take first 5 letters
#   char_limit <- 56 / length(colnames(df))
#   # print(char_limit)
#   colnames(df) <- sapply(names(df), function(x){
#     if (nchar(x)>char_limit)
#     {return (paste0(substr(x, start = 1, stop = char_limit),"..."))}
#     else{return (x)}
#   })
#   
#   # to round everything down to 3 decimals
#   df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
#   
#   
#   df
#   
#   
# }, plugins = "ellipsis",
# options = list(scrollX=TRUE, 
#                columnDefs = list(
#                  list(
#                    targets = 1,
#                    render = JS("$.fn.dataTable.render.ellipsis( 17, true )")
#                  ),
#                  list(
#                    targets = "_all",
#                    render = JS("$.fn.dataTable.render.ellipsis( 6, true )")
#                  )
#                ),
#                headerCallback= JS("function(thead, data, start, end, display){",
#                                   sprintf("  var tooltips = [%s];", toString(paste0("'", rv$df_nxy_fullcols, "'"))),
#                                   "  for(var i = 1; i <= tooltips.length; i++){",
#                                   "    $('th:eq('+i+')',thead).attr('title', tooltips[i-1]);",
#                                   "  }",
#                                   "}")))
# 
# 
# 
# # download current df
# output$download_nxy_df <- downloadHandler(
#   filename = function() {
#     paste("data", "-", "multiple", "-", Sys.Date(), ".csv", sep="")},
#   content = function(file) {
#     write.csv(n_nxy_df(), file, 
#               row.names = F, quote=TRUE)})
# 


####--------------- xy scatter -------------------####


# colormode options (shows different panels conditionally)
output$nxy_colormode_options <- renderUI({
  req(is.null(n_ins_full())==F)
  req(rv$nxy_colormode !="None")
  if(rv$nxy_colormode =="Two colors"){
    div(
      radioButtons(
        inputId = "nxy_sig",
        label = "Significance:",
        choices = c("PValue", "FDR"),
        selected="PValue", inline=T),
      numericInput("nxy_thresh", 
                   "Threshold:", value = 0.01, min = 0, max = 1, step=0.001, width="100px"),
      radioGroupButtons("n_sc_logic",
                        label = "Cutoff mode:",
                        choices=c("OR" ="Either", "AND" = "Both"),
                        selected="Both",size="s")
      , 
      uiOutput("nxy_logic_caption")
    )
  } else if (rv$nxy_colormode =="Color and size"){
    radioButtons(
      inputId = "nxy_sig",
      label = "Significance:",
      choices = c("PValue", "FDR"),
      selected="PValue", inline=T)
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



# plotly scatter
nxy_sc_plt <- reactive({
  req(is.null(n_ins_full())==F)
  req(is.null(rv$nxy_selected_x)==F)
  req(is.null(rv$nxy_selected_y)==F)
  
  withProgress(message = 'Making graph...', value = 0, {
    
    # df <- n_nxy_df()
    df <- n_ins_full()
    print(head(df))
    
    selected <- c(rv$nxy_selected_x, rv$nxy_selected_y)
    xsig <- paste(rv$nxy_sig, selected[[1]], sep="_")
    ysig <- paste(rv$nxy_sig, selected[[2]], sep="_")
    xstat <- paste("Stat", selected[[1]], sep="_")
    ystat <- paste("Stat", selected[[2]], sep="_")
    xp <- paste("PValue", selected[[1]], sep="_")
    yp <- paste("PValue", selected[[2]], sep="_")
    xq <- paste("FDR", selected[[1]], sep="_")
    yq <- paste("FDR", selected[[2]], sep="_")
    
    
    
    df[df==0]<-0.00001 # replace 0 with 0.001
    df <- remove_nas(df)
    
    req(nrow(df)>0)
    
    incProgress(0.2)
    
    # initialize marker settings as none
    df$color <- "black"
    df$color <- as.factor(df$color)
    df$size <- rv$nxy_sc_size+2 # initialized to 5
    marker_settings <- list(
      color= df$color, size= df$size, 
      line = list(color = 'white', width = 0))
    
    incProgress(0.2)
    
    if (rv$nxy_colormode== "Two colors"){ 
      #print(head(df))
      
      # color by AND or OR logic
      df$color <- as.character(df$color)
      if (rv$n_sc_logic == "Both"){
        df$color[which(df[[xsig]] < rv$nxy_thresh & df[[ysig]] < rv$nxy_thresh)] <- "red"
      } else if (rv$n_sc_logic == "Either"){
        df$color[which(df[[xsig]] < rv$nxy_thresh | df[[ysig]] < rv$nxy_thresh)] <- "red"
      }
      
      df$color <- as.factor(df$color)
      
      df$size <- rv$nxy_sc_size+2 # initialized to 5
      marker_settings <- list(
        color= df$color, size= df$size, 
        line = list(color = 'white', width = 1))
    }
    else if (rv$nxy_colormode== "Color and size"){
      df[df==0]<-0.00001 # replace 0 with 0.001
      df$color <- -log10(as.numeric(df[[xsig]]))
      df$color <- as.numeric(df$color)
      df$size <- -log10(as.numeric(df[[ysig]]))* (rv$nxy_sc_size-1) + 3 # initialized to 2+3
      #print(head(df))
      marker_settings <- list(
        color= df$color, size= df$size,
        opacity=.7, line = list(color = 'white', width = 1),
        colorscale=cscale, cauto=F, cmin=0, cmax=3,
        colorbar=list(title=paste0('-log10(',rv$nxy_sig,'(x))')))
    }
    
    # generate properties table only when two color mode is selected
    if (rv$nxy_colormode!="Color and size"){
      summary_df <- summarize_factor_column(df, "color")
      rv$nxy_color <- summary_df
    }
    
    
    incProgress(0.2)
    lm_fun <- paste0("`", xstat, "` ~ `", ystat, "`")
    rv$fit_nxy <- lm(lm_fun, data = df)
    
    print(head(df))
    
    
    
    fig <- plot_ly(
      data = df, 
      x = df[[xstat]],
      y = df[[ystat]],
      type = 'scatter',
      mode = 'markers', 
      marker = marker_settings,
      hoverinfo="text",
      text=c(paste(df$Name, 
                   "<br>Stat(x):", round(df[[xstat]], 3),
                   "<br>p(x):", round(df[[xp]], 3),
                   ", q(x):", round(df[[xq]], 3),
                   "<br>Stat(y):", round(df[[ystat]], 3),
                   "<br>p(y):", round(df[[yp]], 3),
                   ", q(y):", round(df[[yq]], 3)
      ))
    )
    fig <- fig %>% layout(title = paste0(rv$nxy_selected_x, " vs ", rv$nxy_selected_x, " (n=",nrow(df),")"),
                          yaxis = list(zeroline = T, title=paste0("Stat_",rv$nxy_selected_y)),
                          xaxis = list(zeroline = T, title=paste0("Stat_",rv$nxy_selected_x))
    )
    
  })
  return(fig)
})


output$df_nxy_scatter <- renderPlotly({
  req(is.null(n_ins_full())==F)
  req(is.null(rv$nxy_colormode)==F)
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
      strong("Color Summary:"),
      dataTableOutput("nxy_color_tbl"),br(),
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




####-------------------- 3D scatter ------------------------####

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
                            "P <:", value = 0.05, min = 0, max = 1, step=0.001, width="100px"),
        ),
        column(6,
               numericInput("n_3ds_q", 
                            "FDR <:", value = 1, min = 0, max = 1, step=0.001, width="100px"),
        ),
      ),
      fluidRow(
        column(6,
               numericInput("n_3ds_Stat", 
                            "|Stat| >:", value = 0, min = 0, max = 10, step=0.1, width="100px"),
               ),
        column(6,
               radioGroupButtons("nxyz_sc_logic",
                                 label = "Cutoff mode:",
                                 choices=c("OR" ="Either", "AND" = "Both"),
                                 selected="Both",size="s"), 
               )
      ),

      uiOutput("nxyz_logic_caption"),
    )
  } 
})


# main graph
n_3ds_plt <- reactive({
  req(nrow(n_ins_full())>0)
  req(is.null(rv$nxy_selected_x)==F)
  req(is.null(rv$nxy_selected_y)==F)
  req(is.null(rv$nxy_selected_z)==F)
  req(rv$nxy_selected_z!="None")
  
  withProgress(message = 'Making 3D Scatter...', value = 0, {
    
    df <- n_ins_full()
    df <- remove_nas(df)
    
    selected <- c(rv$nxy_selected_x, rv$nxy_selected_y, rv$nxy_selected_z)
    # get col names
    statcols <- paste("Stat_", selected, sep="")
    pcols <- paste("PValue_", selected, sep="")
    qcols <- paste("FDR_", selected, sep="")
    pp <- rv$n_3ds_p
    qq <- rv$n_3ds_q
    ss <- rv$n_3ds_Stat
    
    
    incProgress(0.2)
    
    # default color is black. conditionally color by threshold.
    df$color <- "black"
    
    # assign color by certain criteria
    if (rv$nxyz_colormode == "Two colors"){
      if (rv$nxyz_sc_logic == "Both"){
        req(is.null(rv$nxyz_sc_logic)==F)
        df$color <- ifelse(
          (df[[pcols[[1]]]] < pp & df[[qcols[[1]]]] < qq & df[[statcols[[1]]]] > ss) &
            (df[[pcols[[2]]]] < pp & df[[qcols[[2]]]] < qq & df[[statcols[[2]]]] > ss) &
            (df[[pcols[[3]]]] < pp & df[[qcols[[3]]]] < qq & df[[statcols[[3]]]] > ss), 
          "red", "gray")
        df$color <- ifelse(
          (df[[pcols[[1]]]] < pp & df[[qcols[[1]]]] < qq & df[[statcols[[1]]]] < -ss) &
            (df[[pcols[[2]]]] < pp & df[[qcols[[2]]]] < qq & df[[statcols[[2]]]] < -ss) &
            (df[[pcols[[3]]]] < pp & df[[qcols[[3]]]] < qq & df[[statcols[[3]]]] < -ss), 
          "blue", df$color)
      } else if (rv$nxyz_sc_logic == "Either"){
        df$color <- ifelse(
          (df[[pcols[[1]]]] < pp & df[[qcols[[1]]]] < qq & df[[statcols[[1]]]] > ss) |
            (df[[pcols[[2]]]] < pp & df[[qcols[[2]]]] < qq & df[[statcols[[2]]]] > ss) |
            (df[[pcols[[3]]]] < pp & df[[qcols[[3]]]] < qq & df[[statcols[[3]]]] > ss), 
          "red", "gray")
        df$color <- ifelse(
          (df[[pcols[[1]]]] < pp & df[[qcols[[1]]]] < qq & df[[statcols[[1]]]] < -ss) |
            (df[[pcols[[2]]]] < pp & df[[qcols[[2]]]] < qq & df[[statcols[[2]]]] < -ss) |
            (df[[pcols[[3]]]] < pp & df[[qcols[[3]]]] < qq & df[[statcols[[3]]]] < -ss), 
          "blue", df$color)
      }
      df$color <- as.factor(df$color)
    }
    
    incProgress(0.2)
    
    fig <- plot_ly(df, x = df[[statcols[[1]]]], y = df[[statcols[[2]]]], z = df[[statcols[[3]]]], marker = list(color = df$color, size=rv$nxyz_sc_size-1),
                   hoverinfo="text",
                   text=c(paste(df$Name, 
                                "<br>logFC(x):", round(df[[statcols[[1]]]], 3),
                                "<br>p=", round(df[[pcols[[1]]]], 3),", q=", round(df[[qcols[[1]]]], 3),
                                "<br>logFC(y):", round(df[[statcols[[2]]]], 3),
                                "<br>p=", round(df[[pcols[[2]]]], 3),", q=", round(df[[qcols[[2]]]], 3),
                                "<br>logFC(z):", round(df[[statcols[[3]]]], 3),
                                "<br>p=", round(df[[pcols[[3]]]], 3),", q=", round(df[[qcols[[3]]]], 3)
                   )))
    
    incProgress(0.2)
    
    # generate properties table
    summary_df <- summarize_factor_column(df, "color")
    rv$n_3ds_prop <- summary_df
    
    fig <- fig %>% add_markers()
    fig <- fig %>% layout(title = paste0('3D Scatter, n=',nrow(df)),
                          scene = list(xaxis = list(title = paste0(statcols[[1]])),
                                       yaxis = list(title = paste0(statcols[[2]])),
                                       zaxis = list(title = paste0(statcols[[3]]))))
    
  })
  fig
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

