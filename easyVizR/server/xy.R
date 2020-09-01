#======================================================================#
####                         TWO WAY                                ####
#======================================================================#


####---------------------- Events ---------------------------####

# update params
observe({
  if(is.null(input$xy_p)==F){rv$xy_p <- input$xy_p}
  if(is.null(input$xy_q)==F){rv$xy_q <- input$xy_q}
  if(is.null(input$xy_Stat)==F){rv$xy_Stat <- input$xy_Stat}
  
  if(is.null(input$xy_sc_p)==F){rv$xy_sc_p <- input$xy_sc_p}
  if(is.null(input$xy_sc_q)==F){rv$xy_sc_q <- input$xy_sc_q}
  if(is.null(input$xy_sc_Stat)==F){rv$xy_sc_Stat <- input$xy_sc_Stat}
  if(is.null(input$xy_colormode)==F){rv$xy_colormode <- input$xy_colormode}
  if(is.null(input$xy_sig)==F){rv$xy_sig <- input$xy_sig}
  if(is.null(input$xy_thresh)==F){rv$xy_thresh <- input$xy_thresh}
  if(is.null(input$xy_sc_size)==F){rv$xy_sc_size <- input$xy_sc_size}
  if(is.null(input$xy_sc_logic)==F){rv$xy_sc_logic <- input$xy_sc_logic}
  
  
})


# observe and pull out shared cols and rows among input dfs (as lists)
observe({
  req(is.null(input$selected_x)==F)
  req(is.null(input$selected_y)==F)
  
  selected <- c(input$selected_x, input$selected_y)
  observed <- detect_shared_dimensions(selected, rv$gg, rv$ll, input_mode="names")
  rv$xy_sharedcols <- observed$shared_cols
  rv$xy_sharedrows <- observed$shared_rows
})


# initialize
observeEvent(input$xy_confirm, {
  req(is.null(input$selected_x)==F)
  req(is.null(input$selected_y)==F)
  
  rv$df_xy <- NULL
  withProgress(message = 'Updating data...', value = 0, {
    
    # df_x <- isolate(rv$gg[[match(input$selected_x, rv$ll)]])
    # incProgress(0.2)
    # df_y <- isolate(rv$gg[[match(input$selected_y, rv$ll)]])
    # incProgress(0.2)
    # df_xy <- merge(df_x,df_y, by = "Name")
    # incProgress(0.2)
    selected <- c(input$selected_x, input$selected_y)
    df_xy <- build_df_n(selected, rv$gg, rv$ll, input_mode="names")
    
    # initialize cor line
    statx <- paste0("`Stat_", selected[[1]],"`") # back ticks to escape possible illegal punctuation
    staty <- paste0("`Stat_", selected[[2]],"`")
    lm_fun <- paste(statx, staty, sep = " ~ ")
    print(lm_fun)
    print(head(df_xy))
    rv$fit_xy <- lm(as.formula(lm_fun), data = df_xy)
    
    
    
    # initialize params
    rv$xyx_i <- isolate(match(input$selected_x, rv$ll))
    rv$xyy_i <- isolate(match(input$selected_y, rv$ll))
    
    rv$xy_p <- 0.25
    rv$xy_q <- 1
    rv$xy_Stat <- 0.25
    
    rv$selected_x <- isolate(input$selected_x)
    rv$selected_y <- isolate(input$selected_y)
    
    rv$xy_sc_p <- 0.25
    rv$xy_sc_q <- 1
    rv$xy_sc_Stat <- 0
    rv$xy_colormode <- "None"
    rv$xy_sig <- "PValue"
    rv$xy_thresh <- 0.05
    rv$xy_sc_size <- 3
    rv$xy_sc_logic <- "Both"
    
    incProgress(0.2)
  })
  rv$df_xy <- df_xy
  print(head(rv$df_xy))
  
})





####---------------------- Sidebar ---------------------------####

# select x and y
output$select_x <- renderUI({
  selectizeInput(
    inputId = "selected_x",
    label = "Select dataset 1:",
    choices = rv$ll)
})
output$select_y <- renderUI({
  selectizeInput(
    inputId = "selected_y",
    label = "Select dataset 2:",
    choices = rv$ll)
})

# feedback on whether the data has enough shared rows/cols
output$xy_shared <- renderUI({
  req(is.null(rv$xy_sharedcols)==F)
  req(is.null(rv$xy_sharedrows)==F)
  
  if (length(rv$xy_sharedcols)>=1){msgx=" (ok)"}
  else{ msgx=""}
  if (length(rv$xy_sharedrows)>=1){msgy=" (ok)"}
  else{ msgy=""}
  
  if (msgx==" (ok)" & msgy==" (ok)"){
    box(
      title = NULL, background = "green", solidHeader = TRUE, width=12,
      paste0("Shared columns: ",length(rv$xy_sharedcols), msgx),br(),
      paste0("Shared rows: ",length(rv$xy_sharedrows), msgy)
    )
  }
  else{
    box(
      title = NULL, background = "red", solidHeader = TRUE, width=12,
      paste0("Shared columns: ",length(rv$xy_sharedcols), msgx),br(),
      paste0("Shared rows: ",length(rv$xy_sharedrows), msgy)
    )
  }
})




# confirm and submit
output$xy_confirm <- renderUI({
  req(rv$xy_sharedcols>=1)
  req(rv$xy_sharedrows>=1)
  req(input$selected_x != input$selected_y)
  
  actionButton("xy_confirm", "Visualize!")
})


####----------------------Main Tabs---------------------------####


output$xy_floating_buttons <- renderUI({
  if (is.null(rv$df_xy)==T){
    div(style="margin-top:10px",
        
        actionBttn(
          inputId = "help_xy_pre", label=NULL,
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        ),
    )
  } else {
    div(style="margin-top:10px",
        
        actionBttn(
          inputId = "help_xy_post", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        ),
    )
  }
})


output$xy_panels <- renderUI({
  
  if(is.null(rv$df_xy)==T){
    box(
      title = span( icon("exclamation"), "Notification"), status = "warning", width=8,
      "No data selected."
    )
  }
  else{
    
    
    
    
    div(
      #     fluidRow(
      #         column(6,
      #                radioGroupButtons("dummy",
      #                                  choices=c("Main", "Intersection", "Correlation"),
      #                                  selected="Main", status="primary",
      #                                  checkIcon = list(
      #                                      yes = tags$i(class = "fa fa-check-square", 
      #                                                   style = "color: white"),
      #                                      no = tags$i(class = "fa fa-square-o", 
      #                                                  style = "color: white"))
      #                ),
      #         ),
      #         column(6, align= "right",
      #                
      #                
      #                dropdown(align="right",
      #                         
      #                         tags$h3("Customize Filters"),
      #                         
      #                         uiOutput("dummy2"),
      #                         
      #                         style = "material-circle", icon = icon("gear"),
      #                         status = "default", width = "800px",
      #                         right=T, 
      #                         animate = animateOptions(
      #                             enter = "slideInRight",
      #                             exit = "fadeOutRight", duration = 0.5
      #                         ),
      #                ),
      #                
      #                
      #                
      #         )
      #     ),
      
      box(
        title = span( icon("chart-area"), "Scatter"), status = "primary", solidHeader = TRUE, width=8,
        
        plotlyOutput("df_xy_scatter",
                     width = "100%",height = "600px")
        ,
        div(style = "position: absolute; left: 1em; bottom: 1em",
            dropdown(
              numericInput("xy_sc_p", 
                           "P filter:", value = 0.05, min = 0, max = 1, step=0.001, width="100px"),
              numericInput("xy_sc_q", 
                           "FDR filter:", value = 1, min = 0, max = 1, step=0.001, width="100px"),
              numericInput("xy_sc_Stat", 
                           "|Stat| filter:", value = 0, min = 0, max = 10, step=0.01, width="100px"),
              
              radioGroupButtons("xy_sc_logic",
                                label = "Cutoff mode:",
                                choices=c("Either", "Both"),
                                selected="Both",size="s")
              , 
              size = "xs",
              icon = icon("cut", class = "opt"),
              up = TRUE, width=200
            )
        ),
        div(style = "position: absolute; left: 4em; bottom: 1em",
            dropdown(
              uiOutput("xy_colormode"),
              uiOutput("xy_sig"),
              uiOutput("xy_thresh"),
              numericInput(
                inputId = "xy_sc_size",
                label = "Dot size:",
                value = 3, step=0.5, width="100px")
              ,
              size = "xs",
              icon = icon("gear", class = "opt"),
              up = TRUE, width=230
            )
        ),
        div(style = "position: absolute; left: 7em; bottom: 1em",
            dropdown(
              downloadButton("scatter_xy_dl", "Download plot")
              ,
              size = "xs",
              icon = icon("download", class = "opt"),
              up = TRUE
            )
        )
        
        
      ),
      box(
        title = span( icon("calculator"), "Correlation"), status = "primary", solidHeader = TRUE, width=4
        ,
        
        uiOutput("xy_corline"),br(),
        verbatimTextOutput("xy_cor_summary")
        
      ),
      box(
        title = span( icon("table"), "Table"), status = "primary", solidHeader = TRUE, width=12,
        
        dataTableOutput("xy_tbl", width = "100%",height="100%") 
        ,
        div(style = "position: absolute; left: 1em; bottom: 1em",
            dropdown(
              sliderTextInput("xy_p",
                              label = "Select P cutoff:",
                              choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                              selected=0.25, grid=T, force_edges=T),
              sliderTextInput("xy_q",
                              label = "Select FDR cutoff:",
                              choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                              selected=1, grid=T, force_edges=T),
              sliderInput("xy_Stat",
                          label = "Select |Stat| cutoff:",
                          min=0, max=5, step=0.1, value=0),
              "Note: This will be applied to all columns."
              ,
              size = "xs",
              icon = icon("cut", class = "opt"),
              up = TRUE, width=300
            )
        ),
        div(style = "position: absolute; left: 4em; bottom: 1em",
            dropdown(
              downloadButton("xy_tbl_dl",
                             label = "Download Table")
              ,
              size = "xs",
              icon = icon("download", class = "opt"),
              up = TRUE
            )
        )
        
        
      )
    )
  }
})




####================= TWO WAY VISUALIZATIONS =====================####


filtered_df_xy <- reactive({
  req(is.null(rv$df_xy)==F)
  req(is.null(rv$selected_x)==F)
  req(is.null(rv$selected_y)==F)
  
  df_xy <- rv$df_xy
  selected <- c(rv$selected_x, rv$selected_y)
  for (i in selected){
    df_xy <- apply_single_cutoff(df_xy, i, rv$xy_p, rv$xy_q, rv$xy_Stat, tolerate=F)
  }
  
  df_xy
})

####--------------- table -------------------####

# show df_xy table
output$xy_tbl <- DT::renderDataTable({
  req(rv$df_xy)
  
  
  df <- filtered_df_xy()
  
  
  rv$df_xy_fullcols <- colnames(df)
  
  # to abbreviate the long column names...take first 5 letters
  char_limit <- 56 / length(colnames(df))
  # print(char_limit)
  colnames(df) <- sapply(names(df), function(x){
    if (nchar(x)>char_limit)
    {return (paste0(substr(x, start = 1, stop = char_limit),"..."))}
    else{return (x)}
  })
  
  
  # to replace the stat col names 
  colnames(df) <- gsub("Stat", rv$tt[[rv$xyx_i]], colnames(df))
  
  # to round everything down to 3 decimals
  df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
  
  
  # print(head(df))
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
                                  sprintf("  var tooltips = [%s];", toString(paste0("'", rv$df_xy_fullcols, "'"))),
                                  "  for(var i = 1; i <= tooltips.length; i++){",
                                  "    $('th:eq('+i+')',thead).attr('title', tooltips[i-1]);",
                                  "  }",
                                  "}"))
)

# download table
output$xy_tbl_dl <- downloadHandler(
  filename = function() {paste0("data-",Sys.Date(),"-",input$selected_x,"_vs_",input$selected_y,".csv")},
  content = function(file) {
    
    output_file <- filtered_df_xy()
    
    write.csv(output_file, file, 
              row.names = FALSE, quote=T)})


####--------------- scatter plot -------------------####



# color mode
output$xy_colormode <- renderUI({
  req(is.null(rv$df_xy)==F)
  radioButtons(
    inputId = "xy_colormode",
    label = "Represent significance by:",
    choices = c("None", "Two colors", "Color and size"))
})
output$xy_sig <- renderUI({
  req(is.null(rv$df_xy)==F)
  req(rv$xy_colormode !="None")
  radioButtons(
    inputId = "xy_sig",
    label = "Significance:",
    choices = c("PValue", "FDR"),
    selected="PValue")
})
output$xy_thresh <- renderUI({
  req(rv$xy_colormode =="Two colors")
  
  numericInput("xy_thresh", 
               "Threshold:", value = 0.05, min = 0, max = 1, step=0.001, width="100px")
})


# plotly scatter
xy_sc_plt <- reactive({
  req(is.null(rv$df_xy)==F)
  req(is.null(rv$selected_x)==F)
  req(is.null(rv$selected_y)==F)
  
  withProgress(message = 'Making graph...', value = 0, {
    print(dim(rv$df_xy[[1]]))
    
    df_p <- rv$df_xy
    
    selected <- c(rv$selected_x, rv$selected_y)
    xsig <- paste(rv$xy_sig, selected[[1]], sep="_")
    ysig <- paste(rv$xy_sig, selected[[2]], sep="_")
    xstat <- paste("Stat", selected[[1]], sep="_")
    ystat <- paste("Stat", selected[[2]], sep="_")
    xp <- paste("PValue", selected[[1]], sep="_")
    yp <- paste("PValue", selected[[2]], sep="_")
    xq <- paste("FDR", selected[[1]], sep="_")
    yq <- paste("FDR", selected[[2]], sep="_")
    
    
    
    df_p[df_p==0]<-0.00001 # replace 0 with 0.001
    # cutoffs
    x_filtered <- apply_single_cutoff(df_p, selected[[1]], p=rv$xy_sc_p, q=rv$xy_sc_q, stat=rv$xy_sc_Stat, tolerate=F)
    y_filtered <- apply_single_cutoff(df_p, selected[[2]], p=rv$xy_sc_p, q=rv$xy_sc_q, stat=rv$xy_sc_Stat, tolerate=F)
    print(head(x_filtered))
    if (rv$xy_sc_logic == "Both"){
      df_p <- df_p[df_p$Name %in% intersect(x_filtered$Name, y_filtered$Name), ]
      # df_p <- df_p %>% filter(PValue.x < rv$xy_sc_p & PValue.y < rv$xy_sc_p)
      # df_p <- df_p %>% filter(FDR.x < rv$xy_sc_q & FDR.y < rv$xy_sc_q)
      # df_p <- df_p %>% filter(abs(Stat.x) > rv$xy_sc_Stat & abs(Stat.y) > rv$xy_sc_Stat)
    } else if (rv$xy_sc_logic == "Either"){
      # df_p <- df_p %>% filter(PValue.x < rv$xy_sc_p | PValue.y < rv$xy_sc_p)
      # df_p <- df_p %>% filter(FDR.x < rv$xy_sc_q | FDR.y < rv$xy_sc_q)
      # df_p <- df_p %>% filter(abs(Stat.x) > rv$xy_sc_Stat | abs(Stat.y) > rv$xy_sc_Stat)
      df_p <- df_p[df_p$Name %in% union(x_filtered$Name, y_filtered$Name), ]
    }
    
    df_p <- remove_nas(df_p) # when using Either mode, NA might slip by. 
    # need to delete NA rows before graphing, although those can show up in table.
    
    req(nrow(df_p)>0)
    
    incProgress(0.2)
    
    # initialize marker settings as none
    df_p$color <- "black"
    df_p$color <- as.factor(df_p$color)
    df_p$size <- rv$xy_sc_size+2 # initialized to 5
    marker_settings <- list(
      color= df_p$color, size= df_p$size, 
      line = list(color = 'white', width = 0))
    
    incProgress(0.2)
    
    if (rv$xy_colormode== "Two colors"){ # is this a good idea????
      #print(head(df_p))
      
      df_p$color <- as.character(df_p$color)
      df_p$color[which(df_p[[xsig]] < rv$xy_thresh | df_p[[ysig]] < rv$xy_thresh)] <- "red"
      df_p$color <- as.factor(df_p$color)
      
      df_p$size <- rv$xy_sc_size+2 # initialized to 5
      marker_settings <- list(
        color= df_p$color, size= df_p$size, 
        line = list(color = 'white', width = 1))
    }
    else if (rv$xy_colormode== "Color and size"){
      df_p[df_p==0]<-0.00001 # replace 0 with 0.001
      df_p$color <- -log10(as.numeric(df_p[[xsig]]))
      df_p$color <- as.numeric(df_p$color)
      df_p$size <- -log10(as.numeric(df_p[[ysig]]))* (rv$xy_sc_size-1) + 3 # initialized to 2+3
      #print(head(df_p))
      marker_settings <- list(
        color= df_p$color, size= df_p$size,
        opacity=.7, line = list(color = 'white', width = 1),
        colorscale=cscale, cauto=F, cmin=0, cmax=3,
        colorbar=list(title=paste0('-log10(',rv$xy_sig,'(Name))')))
    }
    
    incProgress(0.2)
    lm_fun <- paste0("`", xstat, "` ~ `", ystat, "`")
    rv$fit_xy <- lm(lm_fun, data = df_p)
    
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
                   "<br>",rv$tt[[rv$xyx_i]],"(x):", round(df_p[[xstat]], 3),
                   "<br>p(x):", round(df_p[[xp]], 3),
                   ", q(x):", round(df_p[[xq]], 3),
                   "<br>",rv$tt[[rv$xyy_i]],"(y):", round(df_p[[ystat]], 3),
                   "<br>p(y):", round(df_p[[yp]], 3),
                   ", q(y):", round(df_p[[yq]], 3)
      ))
    )
    fig <- fig %>% layout(title = paste0(rv$selected_x, " vs ", rv$selected_y, " (n=",nrow(df_p),")"),
                          yaxis = list(zeroline = T, title=paste0(rv$tt[[rv$xyx_i]],"_",rv$selected_y)),
                          xaxis = list(zeroline = T, title=paste0(rv$tt[[rv$xyy_i]],"_",rv$selected_x))
    )
    
  })
  return(fig)
})


output$df_xy_scatter <- renderPlotly({
  req(is.null(rv$df_xy)==F)
  req(is.null(rv$xy_colormode)==F)
  req(nrow(rv$df_xy) > 0)
  
  xy_sc_plt()
})



# download plotly html graph
output$scatter_xy_dl <- downloadHandler(
  filename = function() {paste("scatter-xy-", Sys.Date(), ".html", sep = "")},
  content = function(file) {saveWidget(as_widget(xy_sc_plt()), file, selfcontained = TRUE)})


####--------------- correlation -------------------####


# correlation line
output$xy_corline <- renderUI({
  req(is.null(rv$fit_xy)==F)
  req(is.null(xy_sc_plt())==F)
  
  intercept = format(round(coef(rv$fit_xy)[[1]], 2), nsmall = 2)
  slope = format(round(coef(rv$fit_xy)[[2]], 2), nsmall = 2)
  r2= format(round(summary(rv$fit_xy)$r.squared, 2), nsmall = 2)
  box(
    title = NULL, background = "aqua", solidHeader = TRUE, width=12,
    strong("Correlation line:"),br(),
    column( 12,align="center" ,
            paste0("y = ", slope,"x + ",intercept), br(),
            paste0("(R^2 = ", r2,")")
    )
    
  )
})
output$xy_cor_summary <- renderPrint({
  req(is.null(rv$df_xy)==F)
  summary(rv$fit_xy)
})









