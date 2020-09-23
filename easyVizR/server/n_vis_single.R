####-----------------single gl bar plot----------------####


output$nx_bar_panel <- renderUI({
  req(is.null(n_ins_full())==F)
  
  if (nrow(n_ins_full())<= nmax_bar) {
    box(
      title = span( icon("chart-area"), "Bar"), status = "primary", solidHeader = F, width=12,
      div(id="nx_barp",
          plotlyOutput("nx_bar", width = "100%", height = "400px"),
          ),
      
      
      div(style = "position: absolute; left: 1em; bottom: 1em; width:300px;",
          dropdown(
            radioButtons("nx_bar_sig", "Color by significance:",
                         choices=c("PValue", "FDR"),
                         selected="PValue"),
            selectInput(
              inputId = "nx_bar_to_plot",
              label= "Plot data:",
              choices = rv$hm_numeric_stats, # this displays all the shared numeric columns, 
              selected = "Stat"
            ),
            
            size = "xs",
            icon = icon("gear", class = "opt"),
            up = TRUE
          )
      ),
      div(style = "position: absolute; left: 4em; bottom: 1em",
          dropdown(
            downloadButton("nx_bar_dl", "Download plot"),
            
            size = "xs",
            icon = icon("download", class = "opt"),
            up = TRUE
          )
      )
    )
  } else {
    box(
      title = span( icon("chart-area"), "Bar"), status = "warning", solidHeader = F, width=12,
      div(id="nx_barp",
      paste0("Bar plot is only available for ", nmax_bar," or less entries.")
      )
    )
    
  }
})



# bar graph
nx_bar_plt <- reactive({
  df <- n_ins_full()
  
  name <- rv$nx_selected
  pcol <- paste0("PValue_", name)
  qcol <- paste0("FDR_", name)
  statcol <- paste0("Stat_", name)
  sigcol <- paste0(rv$nx_bar_sig,"_",name)
  plotcol <- paste0(rv$nx_bar_to_plot, "_", name)
  
  df[df==0]<-0.00001 # replace 0 with 0.001
  
  df$color <- -log10(as.numeric(df[[sigcol]]))
  print(head(df))
  fig <- plot_ly(
    x = df$Name,
    y = df[[plotcol]],
    type = "bar",
    hoverinfo="text",
    marker = list(
      colorscale=cscale,
      color = df$color,
      colorbar=list(title=paste0("-log10(",rv$nx_bar_sig, ")")),
      cauto = F,cmin = 0,cmax = 3
    ),
    text=c(paste(df$Name, 
                 "<br>Stat:", as.character(round(df[[statcol]], 3)),
                 "<br>PValue:", as.character(round(df[[pcol]], 3)),
                 "<br>FDR:", as.character(round(df[[qcol]], 3))))
    
    
  )
  fig <- fig %>% layout(title = paste0(name, " (n=",nrow(df),")")
  )
  return(fig)
})

output$nx_bar <- renderPlotly({
  req(is.null(rv$nx_selected)==F)
  req(is.null(n_ins_full())==F)
  req(nrow(n_ins_full())<= nmax_bar) # only plot when few genes are selected
  
  nx_bar_plt()
})

# download plotly html graph
output$nx_bar_dl <- downloadHandler(
  filename = function() {paste("single-bar-", Sys.Date(), ".html", sep = "")},
  content = function(file) {saveWidget(as_widget(nx_bar_plt()), file, selfcontained = TRUE)})



####-----------------single volcano ----------------####


# volcano graph 
nx_vol_plt <- reactive({
  
  if (rv$nx_vol_plotmode=="Focus"){
    df <- n_ins_full()
  } else if (rv$nx_vol_plotmode=="Context"){
    df_ins <- n_ins_full()$Name
    df <- n_ins_full()
    df <- rbind(rv$df_n[-which(rv$df_n$Name %in% df_ins),], df) # make sure the ins is plotted first
  }
  
  
  name <- rv$nx_selected
  pcol <- paste0("PValue_", name)
  qcol <- paste0("FDR_", name)
  statcol <- paste0("Stat_", name)
  
  df <- df[, c("Name",pcol,qcol,statcol)]
  
  df[[pcol]][df[[pcol]]==0]<-0.00001 # replace 0 with 0.001
  df <- remove_nas(df)
  
  if (rv$nx_vol_plotmode=="Focus"){
    df$color <- ifelse(df[[pcol]] <= rv$nx_p & 
                         abs(df[[statcol]]) >= rv$nx_Stat, 
                       rv$nx_vol_c1, rv$nx_vol_c2)
  } else if (rv$nx_vol_plotmode=="Context"){
    # print(df_ins)
    df$color <- ifelse(df$Name %in% df_ins,
                       rv$nx_vol_c2, rv$nx_vol_c3)
    df$color <- ifelse(df[[pcol]] <= rv$nx_p & 
                         abs(df[[statcol]]) >= rv$nx_Stat & df$Name %in% df_ins, 
                       rv$nx_vol_c1, df$color)
    # print(df)
  }
  
  
  
  volcano_xmax <- max(abs(df[[statcol]]), na.rm = TRUE)+0.5 # find max of Stat
  volcano_ymax <- max(-log10(df[[pcol]]), na.rm = TRUE)+0.5 # find max of -log10p
  
  fs_volcano <- plot_ly(
    data = df, 
    x = df[[statcol]],
    y = -log10(df[[pcol]]),
    mode = 'markers', 
    marker = list(color = df$color),
    hoverinfo="text",
    text=c(paste(df$Name, 
                 "<br>Stat:", as.character(round(df[[statcol]], 3)),
                 "<br>PValue:", as.character(round(df[[pcol]], 3)),
                 "<br>FDR:", as.character(round(df[[qcol]], 3))
    ))
  )
  
  fs_volcano <- fs_volcano %>% layout(title = paste0(name, " (n=",nrow(df),")"),
                                      yaxis = list(zeroline = T, title="-log10(PValue)",
                                                   range=c(0,volcano_ymax)),
                                      xaxis = list(zeroline = T, title="Stat",
                                                   range=c(-volcano_xmax,volcano_xmax)))
  
  return(fs_volcano)
})
output$nx_vol <- renderPlotly({
  req(is.null(rv$nx_selected)==F)
  req(is.null(n_ins_full())==F)
  
  nx_vol_plt()
})

# download plotly html graph
output$nx_vol_dl <- downloadHandler(
  filename = function() {paste("single-volcano-", Sys.Date(), ".html", sep = "")},
  content = function(file) {saveWidget(as_widget(nx_vol_plt()), file, selfcontained = TRUE)})