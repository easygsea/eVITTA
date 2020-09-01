
####-------------------- table ------------------------####


# display table
output$df_nxy_tbl <- DT::renderDataTable({
  
  df <- n_nxy_df()
  
  # tidy row names
  if (nrow(df)>0){rownames(df) <- seq(1,nrow(df),1)}
  
  # to replace the stat col names
  colnames(df) <- gsub("Stat", rv$tt[[rv$nx_i[[1]]]], colnames(df))
  
  
  rv$df_nxy_fullcols <- colnames(df)
  
  # to abbreviate the long column names...take first 5 letters
  char_limit <- 56 / length(colnames(df))
  # print(char_limit)
  colnames(df) <- sapply(names(df), function(x){
    if (nchar(x)>char_limit)
    {return (paste0(substr(x, start = 1, stop = char_limit),"..."))}
    else{return (x)}
  })
  
  # to round everything down to 3 decimals
  df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
  
  
  df
  
  
}, plugins = "ellipsis",
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
               ),
               headerCallback= JS("function(thead, data, start, end, display){",
                                  sprintf("  var tooltips = [%s];", toString(paste0("'", rv$df_nxy_fullcols, "'"))),
                                  "  for(var i = 1; i <= tooltips.length; i++){",
                                  "    $('th:eq('+i+')',thead).attr('title', tooltips[i-1]);",
                                  "  }",
                                  "}")))



# download current df
output$download_nxy_df <- downloadHandler(
  filename = function() {
    paste("data", "-", "multiple", "-", Sys.Date(), ".csv", sep="")},
  content = function(file) {
    write.csv(n_nxy_df(), file, 
              row.names = F, quote=TRUE)})



####--------------- xy scatter -------------------####



# color mode
output$nxy_colormode <- renderUI({
  req(is.null(n_nxy_df())==F)
  radioButtons(
    inputId = "nxy_colormode",
    label = "Represent significance by:",
    choices = c("None", "Two colors", "Color and size"))
})
output$nxy_sig <- renderUI({
  req(is.null(n_nxy_df())==F)
  req(rv$nxy_colormode !="None")
  radioButtons(
    inputId = "nxy_sig",
    label = "Significance:",
    choices = c("PValue", "FDR"),
    selected="PValue")
})
output$nxy_thresh <- renderUI({
  req(rv$nxy_colormode =="Two colors")
  
  numericInput("nxy_thresh", 
               "Threshold:", value = 0.01, min = 0, max = 1, step=0.001, width="100px")
})


# plotly scatter
nxy_sc_plt <- reactive({
  req(is.null(n_nxy_df())==F)
  req(is.null(rv$nxy_selected_x)==F)
  req(is.null(rv$nxy_selected_y)==F)
  
  withProgress(message = 'Making graph...', value = 0, {
    
    df_p <- n_nxy_df()
    print(head(df_p))
    
    selected <- c(rv$nxy_selected_x, rv$nxy_selected_y)
    xsig <- paste(rv$nxy_sig, selected[[1]], sep="_")
    ysig <- paste(rv$nxy_sig, selected[[2]], sep="_")
    xstat <- paste("Stat", selected[[1]], sep="_")
    ystat <- paste("Stat", selected[[2]], sep="_")
    xp <- paste("PValue", selected[[1]], sep="_")
    yp <- paste("PValue", selected[[2]], sep="_")
    xq <- paste("FDR", selected[[1]], sep="_")
    yq <- paste("FDR", selected[[2]], sep="_")
    
    
    
    df_p[df_p==0]<-0.00001 # replace 0 with 0.001
    df_p <- remove_nas(df_p)
    
    req(nrow(df_p)>0)
    
    incProgress(0.2)
    
    # initialize marker settings as none
    df_p$color <- "black"
    df_p$color <- as.factor(df_p$color)
    df_p$size <- rv$nxy_sc_size+2 # initialized to 5
    marker_settings <- list(
      color= df_p$color, size= df_p$size, 
      line = list(color = 'white', width = 0))
    
    incProgress(0.2)
    
    if (rv$nxy_colormode== "Two colors"){ # is this a good idea????
      #print(head(df_p))
      
      df_p$color <- as.character(df_p$color)
      if (rv$n_sc_logic == "Both"){
        df_p$color[which(df_p[[xsig]] < rv$nxy_thresh & df_p[[ysig]] < rv$nxy_thresh)] <- "red"
      } else if (rv$n_sc_logic == "Either"){
        df_p$color[which(df_p[[xsig]] < rv$nxy_thresh | df_p[[ysig]] < rv$nxy_thresh)] <- "red"
      }
      
      df_p$color <- as.factor(df_p$color)
      
      df_p$size <- rv$nxy_sc_size+2 # initialized to 5
      marker_settings <- list(
        color= df_p$color, size= df_p$size, 
        line = list(color = 'white', width = 1))
    }
    else if (rv$nxy_colormode== "Color and size"){
      df_p[df_p==0]<-0.00001 # replace 0 with 0.001
      df_p$color <- -log10(as.numeric(df_p[[xsig]]))
      df_p$color <- as.numeric(df_p$color)
      df_p$size <- -log10(as.numeric(df_p[[ysig]]))* (rv$nxy_sc_size-1) + 3 # initialized to 2+3
      #print(head(df_p))
      marker_settings <- list(
        color= df_p$color, size= df_p$size,
        opacity=.7, line = list(color = 'white', width = 1),
        colorscale=cscale, cauto=F, cmin=0, cmax=3,
        colorbar=list(title=paste0('-log10(',rv$nxy_sig,'(x))')))
    }
    
    incProgress(0.2)
    lm_fun <- paste0("`", xstat, "` ~ `", ystat, "`")
    rv$fit_nxy <- lm(lm_fun, data = df_p)
    
    print(head(df_p))
    
    
    
    fig <- plot_ly(
      data = df_p, 
      x = df_p[[xstat]],
      y = df_p[[ystat]],
      type = 'scatter',
      mode = 'markers', 
      marker = marker_settings,
      hoverinfo="text",
      text=c(paste(df_p$Name, 
                   "<br>Stat(x):", round(df_p[[xstat]], 3),
                   "<br>p(x):", round(df_p[[xp]], 3),
                   ", q(x):", round(df_p[[xq]], 3),
                   "<br>Stat(y):", round(df_p[[ystat]], 3),
                   "<br>p(y):", round(df_p[[yp]], 3),
                   ", q(y):", round(df_p[[yq]], 3)
      ))
    )
    fig <- fig %>% layout(title = paste0(rv$nxy_selected_x, " vs ", rv$nxy_selected_x, " (n=",nrow(df_p),")"),
                          yaxis = list(zeroline = T, title=paste0("Stat_",rv$nxy_selected_y)),
                          xaxis = list(zeroline = T, title=paste0("Stat_",rv$nxy_selected_x))
    )
    
  })
  return(fig)
})


output$df_nxy_scatter <- renderPlotly({
  req(is.null(n_nxy_df())==F)
  req(is.null(rv$nxy_colormode)==F)
  req(nrow(n_nxy_df()) > 0)
  
  nxy_sc_plt()
})



# download plotly html graph
output$scatter_nxy_dl <- downloadHandler(
  filename = function() {paste("scatter-multiple-xy-", Sys.Date(), ".html", sep = "")},
  content = function(file) {saveWidget(as_widget(nxy_sc_plt()), file, selfcontained = TRUE)})



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
  req(is.null(n_nxy_df())==F)
  summary(rv$fit_nxy)
})




####-------------------- 3D scatter ------------------------####


# main graph
n_3ds_plt <- reactive({
  req(nrow(n_nxy_df())>0)
  req(is.null(rv$nxy_selected_x)==F)
  req(is.null(rv$nxy_selected_y)==F)
  req(is.null(rv$nxy_selected_z)==F)
  req(rv$nxy_selected_z!="None")
  
  withProgress(message = 'Making 3D Scatter...', value = 0, {
    
    df <- n_nxy_df()
    df <- remove_nas(df)
    
    selected <- c(rv$nxy_selected_x, rv$nxy_selected_y, rv$nxy_selected_z)
    
    # attain columns max/min values for cutoff and coloring (automatically filters out NA)
    df <- df %>% mutate(mp = do.call(pmax, dplyr::select(df, contains("PValue")))) 
    df <- df %>% mutate(mq = do.call(pmax, dplyr::select(df, contains("FDR")))) 
    df <- df %>% mutate(msmin = do.call(pmin, dplyr::select(df, contains("Stat")))) %>% 
      mutate(msmax = do.call(pmax, dplyr::select(df, contains("Stat")))) 
    
    incProgress(0.2)
    
    # assign color by certain criteria
    df$color <- ifelse(df$mp < rv$n_3ds_p & df$mq < rv$n_3ds_q & df$msmin > rv$n_3ds_Stat, "red", "gray")
    df$color <- ifelse(df$mp < rv$n_3ds_p & df$mq < rv$n_3ds_q & df$msmax < -rv$n_3ds_Stat, "blue",df$color)
    df$color <- as.factor(df$color)
    #print(head(df))
    
    incProgress(0.2)
    
    # get col names
    statcols <- paste("Stat_", selected, sep="")
    pcols <- paste("PValue_", selected, sep="")
    qcols <- paste("FDR_", selected, sep="")
    # print(statcols)
    
    incProgress(0.2)
    
    fig <- plot_ly(df, x = df[[statcols[[1]]]], y = df[[statcols[[2]]]], z = df[[statcols[[3]]]], marker = list(color = df$color, size=2),
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
    summary <- c("total"=nrow(df), table(df$color))
    summary_df <- t(data.frame(as.list(summary)))
    summary_df <- data.frame(summary_df)
    # add genes to the table
    unicl <- unique(df$color)
    gene_cats <- lapply(unicl, function(x){
      gl <- df[which(df$color==x),][["Name"]]
      paste(gl, collapse=" ")
    })
    gene_cats <- c(paste(df$Name, collapse=" "),gene_cats)
    summary_df$genes <- unlist(gene_cats)
    colnames(summary_df) <- c("n","Names")
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
  req(is.null(n_nxy_df())==F)
  # req(rv$n_3ds_status=="ok")
  
  n_3ds_plt()
})

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

# download plotly html graph
output$n_3ds_dl <- downloadHandler(
  filename = function() {paste("scatter-multiple-", Sys.Date(), ".html", sep = "")},
  content = function(file) {saveWidget(as_widget(n_3ds_plt()), file, selfcontained = TRUE)})

# download summary table
output$download_3ds_df <- downloadHandler(
  filename = function() {
    paste("summary-scatter-multiple-", Sys.Date(), ".csv", sep="")},
  content = function(file) {
    write.csv(n_3ds_prop_df(), file, 
              row.names = T, quote=TRUE)})

