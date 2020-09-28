# ####-------------------- table ------------------------####
# 
# df_n_tbl <- reactive({
#   
#   # df <- n_basic_df()
#   df <- n_ins_full()
#   
#   # tidy row names
#   if (nrow(df)>0){rownames(df) <- seq(1,nrow(df),1)}
#   
#   # to replace the stat col names
#   colnames(df) <- gsub("Stat", rv$tt[[rv$nx_i[[1]]]], colnames(df))
#   
#   df
# })
# 
# # display table
# output$df_n_tbl <- DT::renderDataTable({
#   #req(length(rv$ll) >= 1)
#   req(rv$df_n)
#   
#   df <- df_n_tbl()
#   
#   rv$df_n_fullcols <- colnames(df)
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
#                                   sprintf("  var tooltips = [%s];", toString(paste0("'", rv$df_n_fullcols, "'"))),
#                                   "  for(var i = 1; i <= tooltips.length; i++){",
#                                   "    $('th:eq('+i+')',thead).attr('title', tooltips[i-1]);",
#                                   "  }",
#                                   "}")))
# 
# 
# 
# # download current df
# output$download_n_df <- downloadHandler(
#   filename = function() {
#     paste("data", "-", "multiple", "-", Sys.Date(), ".csv", sep="")},
#   content = function(file) {
#     write.csv(df_n_tbl(), file, 
#               row.names = F, quote=TRUE)})
# 



####--------------------multiple deg heatmap------------------------####


# sort by which dataset
output$select_sortby_p2 <- renderUI({
  req(rv$df_n)
  radioButtons(
    inputId = "heatmap_sortby",
    label= "Sort heatmap by:",
    choices = rv$nx_n,
    selected = rv$select_sortby_p2
  )
})

# which data column to plot (i.e. Stat, etc)
output$n_to_plot <- renderUI({
  req(rv$df_n)
  # prepare choices
  choices <- rv$hm_numeric_stats
  names(choices) <- stat_replace1(rv$hm_numeric_stats, rv$nx_n)
  selectInput(
    inputId = "n_to_plot",
    label= shiny::HTML("Plot data: 
                               <span style='color: gray'>(Note: only numeric columns are selectable)</span>"),
    choices = choices, # this displays all the shared numeric columns, 
    selected = "Stat"
  )
})



output$n_heatmap <- renderUI({
  #req(length(rv$ll) >= 1)
  req(rv$df_n)
  # req(input$draw_heatmap)
  req(is.null(heatmap)==F)
  
  # plotOutput(
  #     outputId = "heatmap", width = "100%", height = "600px"
  # ) %>% withSpinner(type=4)
  plotlyOutput("heatmapp", height="600px")
})



# # pheatmap static heatmap
# output$heatmap <- renderPlot({
#     req(rv$df_n)
#     req(is.null(rv$n_hm_showna)==F)
#     
#     plotted <- rv$df_n
#     tol <- rv$n_hm_showna
#     if (rv$n_hm_cutmode == "All"){
#         plotted <- filter_by_sign(plotted, "Stat", rv$sign_2, tolerate=tol) # global filter
#         plotted <- apply_n_cutoffs(plotted, rv$p_cutoff_2, rv$q_cutoff_2, rv$Stat_cutoff_2, tolerate=tol)
#     }
#     else if (rv$n_hm_cutmode == "Single"){
#         plotted <- filter_by_sign(plotted, paste0("Stat_",rv$n_cutoff_by), rv$sign_2, tolerate = tol) # single filter
#         plotted <- apply_single_cutoff(plotted, rv$n_cutoff_by, rv$p_cutoff_2, rv$q_cutoff_2, rv$Stat_cutoff_2, tolerate=tol)
#     }
#     
#     print(head(plotted))
#     
#     rownames(plotted) <- plotted$Name # put genename as index
#     plotted <- dplyr::select(plotted,contains(rv$n_to_plot)) # only extract wanted columns to plot
#     
#     req(nrow(plotted) != 0)
#     
#     #colnames(plotted) <- input$heatmap_dfs # name columns after analysis # rename columns (optional)
#     plotted <- plotted[order(-plotted[paste(rv$n_to_plot,"_", rv$heatmap_sortby, sep="")]),] # sort in descending order based on selected column
#     
#     # to replace the stat col names 
#     colnames(plotted) <- gsub("Stat", rv$tt[[rv$nx_i[[1]]]], colnames(plotted))
#     
#     hmplot <- pheatmap(plotted, 
#                   #scale = "row", # don't use z score
#                   cluster_rows=F, cluster_cols=T,
#                   show_rownames=F, show_colnames=T) 
#     return (hmplot)
# })




# plotly dynamic heatmap
n_hm_plt <- reactive({
  
  # print(head(df))
  
  # withProgress(message = 'Drawing heatmap...', value = 0, {
    
    # df <- n_basic_df()
    df <- n_ins_full()
    
    rownames(df) <- df$Name # put genename as index
    
    # order by selected column
    sortby_coln <- paste0(rv$n_to_plot,"_", rv$heatmap_sortby)
    df <- df[order(-df[sortby_coln]),] 
    names <- df$Name # preserve formatting in vector
    # incProgress(0.1)
    
    # extract plotted values
    to_match <- paste0(rv$n_to_plot, "_")
    plotted <- data.frame(t(dplyr::select(df,contains(to_match))))
    req(nrow(plotted) > 0)
    # incProgress(0.1)
    
    # make matrix for plot
    dat <- expand.grid(x = rownames(plotted), y = addlinebreaks(names,30,"<br>"))
    dat$z <- unlist(plotted)
    req(length(dat$z)>0)
    # incProgress(0.2)
    
    # put all the shared columns in the hovertext (as many as you have).
    sharedcols <- rv$n_sharedcols
    addlabel <- ""
    for (coln in sharedcols){
      le <- unlist(data.frame(t(dplyr::select(df,matches(paste0("^",coln,"_"))))))
      if (is.numeric(le)){
        le <- round(le,3)
      }
      else if (is.character(le)){
        le <- addlinebreaks(le,30,"<br>")
      }
      if (coln == "Stat"){
        replace_stat <- stat_replace1(rep("Stat", length(rv$nx_n)), rv$nx_n, mode="each")
        replace_coln <- rep(replace_stat, nrow(df))
        addlabel <- paste(addlabel, paste0(replace_coln, ": ", le), sep="<br>")
      } else {
        addlabel <- paste(addlabel, paste0(coln, ": ", le), sep="<br>")
      }
      
    }
    # incProgress(0.2)
    # print(addlabel)
    # define the hovertext
    textt <- ~paste(dat$y, addlabel)
    
    fig <- plot_ly() %>%
      add_trace(data = dat, x = ~x, y = ~y, z = ~z, type = "heatmap",
                colorscale  = cscale_simple,zauto = T, zmid= 0, colorbar = list(title = rv$n_to_plot),
                hoverinfo = 'text',
                text = textt)
    # incProgress(0.2)
    fig <- fig %>% layout(
      xaxis = list(title = "", showticklabels = T),
      yaxis = list(title = "", showticklabels = rv$n_hm_ylabs)
      # ,margin = list(l=200)
    )
    
  # })
  
  fig
})
output$heatmapp <- renderPlotly({
  req(rv$df_n)
  
  n_hm_plt()
  
})



# download plotly html graph
output$n_hm_dl <- downloadHandler(
  filename = function() {paste("heatmap-multiple-", Sys.Date(), ".html", sep = "")},
  content = function(file) {saveWidget(as_widget(n_hm_plt()), file, selfcontained = TRUE)})

