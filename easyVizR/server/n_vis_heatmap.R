
####-------------------- options ------------------------####


# sort by which dataset
output$select_sortby_p2 <- renderUI({
  req(rv$df_n)
  #isolate({choices <- rv$nx_n})
  radioButtons(
    inputId = "heatmap_sortby",
    label= "Sort heatmap by:",
    #choices = choices,
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
    selected = rv$n_to_plot
  )
})

# max len for y labels
output$n_hm_ylabs_len <- renderUI({
  req(rv$n_hm_ylabs == T )
  
  sliderInput("n_hm_ylabs_len",
              "Max string length for y labels:",
              min = 10,
              max = 100,
              value = rv$n_hm_ylabs_len)
})


####-------------------- heatmap ------------------------####


# show heatmap
output$n_heatmap <- renderUI({
  # saveRDS(rv$n_to_plot, file = "rvs/n_to_plot.rds")
  # saveRDS(rv$heatmap_sortby, file = "rvs/heatmap_sortby.rds")
  #req(length(rv$ll) >= 1)

  req(rv$df_n)
  # req(input$draw_heatmap)
  req(is.null(heatmap)==F)
  
  # plotOutput(
  #     outputId = "heatmap", width = "100%", height = "600px"
  # ) %>% withSpinner(type=4)
  plotlyOutput("heatmapp", height="600px")
})



# draw plotly heatmap
n_hm_plt <- reactive({
  
  req_vars(c(
    rv$df_n, n_ins_gls(), rv$ins_criteria, rv$nx_n,
    n_ins_full(), rv$n_to_plot, rv$heatmap_sortby,
    rv$n_hm_ylabs
  ), check_len=T)
  
  if (rv$n_hm_ylabs==T){
    req(is.null(rv$n_hm_ylabs_len)==F)
  }
  
  # prevent duplicates
  validate(need(any(duplicated(n_ins_full()$Name))==F, 
                "Heatmap is disabled because there are duplicated names."))
  
  draw_heatmap( df = n_ins_full(),
                sortby_dataset = rv$n_to_plot,
                sortby_statistic = rv$heatmap_sortby,
                show_ylabs = rv$n_hm_ylabs,
                ylabs_len = rv$n_hm_ylabs_len
                )
})

# show plotly heatmap
output$heatmapp <- renderPlotly({
  req(rv$df_n)
  
  n_hm_plt()
  
})


# download plotly html heatmap
output$n_hm_dl <- downloadHandler(
  filename = function() {paste("heatmap-multiple-", Sys.Date(), ".html", sep = "")},
  content = function(file) {saveWidget(as_widget(n_hm_plt()), file, selfcontained = TRUE)})




# # pheatmap static heatmap (obsolete)
#-----------------------------------------
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