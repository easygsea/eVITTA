####================= SINGLE VISUALIZATIONS =====================####


# download rnk 
output$downloadrnk <- downloadHandler(
  filename = function() {
    paste("RNK-", rv$show_df, "-", Sys.Date(), ".rnk", sep="")},
  content = function(file) {
    output_file <- rv$df
    output_file <- append_rank(output_file)
    output_file <- output_file[,c("Name","Rank")]
    write.table(output_file, file, 
                row.names=FALSE, quote=F,sep="\t")})


####--------------------fs Table------------------------####

x_fs_df <- reactive({
  df <- rv$df
  df <- subset(df, PValue <= rv$df_x_p)
  df <- subset(df, FDR <= rv$df_x_q)
  df <- subset(df, abs(Stat) >= rv$df_x_Stat)
  df
})

output$downloaddf <- downloadHandler(
  filename = function() {
    paste("data", "-", rv$show_df, "-", Sys.Date(), ".csv", sep="")},
  content = function(file) {
    write.csv({
      
      df <- x_fs_df()
      names(df)[names(df) == 'Stat'] <- rv$tt[[rv$x_i]]
      df
      
    }, file, 
    row.names = FALSE, quote=T)})


# show table
output$single_tbl <- DT::renderDataTable({
  req(length(rv$ll) >= 1)
  req(rv$df)
  
  df <- x_fs_df()
  
  names(df)[names(df) == 'Stat'] <- rv$tt[[rv$x_i]]
  
  # to round everything down to 3 decimals
  df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
  
  df
}, 
plugins = "ellipsis",
options = list(scrollX=TRUE, 
               columnDefs = list(
                 list(
                   targets = 1,
                   render = JS("$.fn.dataTable.render.ellipsis( 17, true )")
                 ),
                 list(
                   targets = "_all",
                   render = JS("$.fn.dataTable.render.ellipsis( 6, true )")
                 )
               ))
)


####--------------------gl Table------------------------####


x_gl_df <- reactive({
  req(length(rv$ll) >= 1)
  req(rv$df)
  
  df <- rv$df
  df <- subset(df, PValue <= rv$df_gl_p)
  df <- subset(df, FDR <= rv$df_gl_q)
  df <- subset(df, abs(Stat) >= rv$df_gl_Stat)
  df
})

# download current gl df
output$downloadgldf <- downloadHandler(
  filename = function() {
    paste("subset", "-", rv$show_df, "-", Sys.Date(), ".csv", sep="")},
  content = function(file) {
    write.csv({
      
      df <- x_gl_df()
      names(df)[names(df) == 'Stat'] <- rv$tt[[rv$x_i]]
      df
      
    }, file, 
    row.names = FALSE, quote=T)})



# show gl table
output$single_gl_tbl <- DT::renderDataTable({
  req(length(rv$ll) >= 1)
  req(rv$df)
  
  df <- x_gl_df()
  
  colnames(df)[colnames(df) == 'Stat'] <- rv$tt[[rv$x_i]]
  
  # to round everything down to 3 decimals
  df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
  
  df
})



####-----------------single fs volcano ----------------####


# volcano graph 
x_vol_plt <- reactive({
  df_p <- rv$df
  df_p[df_p==0]<-0.00001 # replace 0 with 0.001
  df_p$color <- ifelse(df_p$PValue <rv$fs_volcano_p & abs(df_p$Stat)>rv$fs_volcano_Stat, "red", "gray")
  
  fs_volcano <- plot_ly(
    data = df_p, 
    x = df_p$Stat,
    y = -log10(df_p$PValue),
    mode = 'markers', marker = list(color = df_p$color),
    hoverinfo="text",
    text=c(paste(df_p$Name, 
                 "<br>",rv$tt[[rv$x_i]],":", as.character(round(df_p$Stat, 3)),
                 "<br>PValue:", as.character(round(df_p$PValue, 3)),
                 "<br>FDR:", as.character(round(df_p$FDR, 3))
    ))
  )
  
  fs_volcano <- fs_volcano %>% layout(title = paste0("Volcano plot of ", rv$show_df, " (n=",nrow(df_p),")"),
                                      yaxis = list(zeroline = T, title="-log10(PValue)",
                                                   range=c(0,rv$volcano_ymax)),
                                      xaxis = list(zeroline = T, title=rv$tt[[rv$x_i]], 
                                                   range=c(-rv$volcano_xmax,rv$volcano_xmax)))
  
  return(fs_volcano)
})
output$p1_fs_volcano <- renderPlotly({
  req(length(rv$ll) >= 1)
  req(rv$df)
  
  x_vol_plt()
})

# download plotly html graph
output$x_vol_dl <- downloadHandler(
  filename = function() {paste("volcano-single-", Sys.Date(), ".html", sep = "")},
  content = function(file) {saveWidget(as_widget(x_vol_plt()), file, selfcontained = TRUE)})


####-----------------single gl bar plot----------------####



output$gl_bar_fig <- renderUI({
  req(length(rv$ll) >= 1)
  req(rv$df)
  
  #req(input$gl_plottype=='Bar plot')
  plotlyOutput(
    outputId = "p1_bar",
    width = "100%",
    height = "400px"
  )
})

# bar graph
x_bar_plt <- reactive({
  df_p <- rv$df
  df_p[df_p==0]<-0.00001 # replace 0 with 0.001
  df_p$color <- -log10(as.numeric(df_p[[rv$p1_bar_sig]]))
  print(head(df_p))
  fig <- plot_ly(
    x = df_p$Name,
    y = df_p[[rv$p1_bar_data]],
    name = "Expression bar plot",
    type = "bar",
    hoverinfo="text",
    marker = list(
      colorscale=cscale,
      color = df_p$color,
      colorbar=list(title=paste0("-log10(",rv$p1_bar_sig, ")")),
      cauto = F,cmin = 0,cmax = 3
    ),
    text=c(paste(df_p$Name, 
                 "<br>",rv$tt[[rv$x_i]],":", as.character(round(df_p$Stat, 3)),
                 "<br>PValue:", as.character(round(df_p$PValue, 3)),
                 "<br>FDR:", as.character(round(df_p$FDR, 3))))
    
    
  )
  return(fig)
})

output$p1_bar <- renderPlotly({
  req(length(rv$ll) >= 1)
  req(rv$df)
  
  x_bar_plt()
})

# download plotly html graph
output$x_bar_dl <- downloadHandler(
  filename = function() {paste("bar-single-", Sys.Date(), ".html", sep = "")},
  content = function(file) {saveWidget(as_widget(x_bar_plt()), file, selfcontained = TRUE)})


####------------------single gl volcano---------------------####


# volcano graph 
output$p1_gl_volcano <- renderPlotly({
  req(length(rv$ll) >= 1)
  req(rv$df)
  
  df_p <- rv$df
  df_p[df_p==0]<-0.00001 # replace 0 with 0.001
  df_p$color <- ifelse(df_p$PValue <rv$gl_volcano_p & abs(df_p$Stat)>rv$gl_volcano_Stat, "red", "gray")
  
  gl_volcano <- plot_ly(
    data = df_p, 
    x = df_p$Stat,
    y = -log10(df_p$PValue),
    mode = 'markers', marker = list(color = df_p$color),
    hoverinfo="text",
    text=c(paste(df_p$Name, 
                 "<br>",rv$tt[[rv$x_i]],":", as.character(round(df_p$Stat, 3)),
                 "<br>PValue:", as.character(round(df_p$PValue, 3)),
                 "<br>FDR:", as.character(round(df_p$FDR, 3)))
    )
  )
  gl_volcano <- gl_volcano %>% layout(title = paste0("Volcano plot of subset of ", rv$show_df, " (n=",nrow(df_p),")"),
                                      yaxis = list(zeroline = T, title="-log10(PValue)",
                                                   range=c(0, rv$volcano_ymax)),
                                      xaxis = list(zeroline = T, title=rv$tt[[rv$x_i]],
                                                   range=c(-rv$volcano_xmax,rv$volcano_xmax)))
  return(gl_volcano)
})
