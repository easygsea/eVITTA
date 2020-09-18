# UPON ENTERING THE FILTER OR VIS TAB, update the values of filters in that tab
observeEvent(input$tabs, {
  req(is.null(rv$nx_n)==F)
  # print(input$tabs)
  if (input$tabs == "tab_filters"){
    for (i in 1:length(rv$nx_n)){
      updateNumericInput(session, paste0("f_p_",i),
                         value = rv[[paste0("nic_p_",i)]]
      )
      updateNumericInput(session, paste0("f_q_",i),
                         value = rv[[paste0("nic_q_",i)]]
      )
      updateNumericInput(session, paste0("f_Stat_",i),
                         value = rv[[paste0("nic_Stat_",i)]]
      )
      updateNumericInput(session, paste0("f_sign_",i),
                         value = rv[[paste0("nic_sign_",i)]]
      )
    }
  } else if (input$tabs == "tab_ins"){
    
    removeUI(
      selector = "#ins_table_panel"
    )
    print("removed ui")
    insertUI(
      selector = "#ins_pg_bottom",
      where = "afterEnd",
      ui = uiOutput("ins_table_panel")
    )
    print("inserted ui in p3")
    
    
    # filter buttons
    removeUI(
      selector = "#n_filters"
    )
    print("removed ui")
    insertUI(
      selector = "#ins_filters_here",
      where = "afterEnd",
      ui = uiOutput("n_filters")
    )
    print("inserted ui in p4")
    
    
    
    
  } else if (input$tabs == "tab3"){
    # for (i in 1:length(rv$nx_n)){
    #   updateNumericInput(session, paste0("nic_p_",i),
    #                      value = rv[[paste0("nic_p_",i)]]
    #   )
    #   updateNumericInput(session, paste0("nic_q_",i),
    #                      value = rv[[paste0("nic_q_",i)]]
    #   )
    #   updateNumericInput(session, paste0("nic_Stat_",i),
    #                      value = rv[[paste0("nic_Stat_",i)]]
    #   )
    #   updateNumericInput(session, paste0("nic_sign_",i),
    #                      value = rv[[paste0("nic_sign_",i)]]
    #   )
    # }
    
    # ins panel
    removeUI(
      selector = "#ins_table_panel"
    )
    print("removed ui")
    insertUI(
      selector = "#vis_pg_bottom",
      where = "afterEnd",
      ui = uiOutput("ins_table_panel")
    )
    print("inserted ui in p4")
    
    # filter buttons
    removeUI(
      selector = "#n_filters"
    )
    print("removed ui")
    insertUI(
      selector = "#n_filters_here",
      where = "afterEnd",
      ui = uiOutput("n_filters")
    )
    print("inserted ui in p4")
    
    
    
    
  }

})




# UPON CLICKING APPLY BUTTON in the FILTER TAB, update filter values into rv
observeEvent(input$f_applytorv, {
  req(is.null(rv$nx_n)==F)

  for (i in 1:length(rv$nx_n)){
    req(is.null(input[[paste0("f_p_",i)]])==F)
    req(is.null(input[[paste0("f_q_",i)]])==F)
    req(is.null(input[[paste0("f_Stat_",i)]])==F)
    req(is.null(input[[paste0("f_sign_",i)]])==F)
    
    rv[[paste0("nic_p_",i)]] <- input[[paste0("f_p_",i)]]
    rv[[paste0("nic_q_",i)]] <- input[[paste0("f_q_",i)]]
    rv[[paste0("nic_Stat_",i)]] <- input[[paste0("f_Stat_",i)]]
    rv[[paste0("nic_sign_",i)]] <- input[[paste0("f_sign_",i)]]
    
    updateNumericInput(session, paste0("f_p_",i),
                       value = rv[[paste0("nic_p_",i)]]
    )
    updateNumericInput(session, paste0("f_q_",i),
                       value = rv[[paste0("nic_q_",i)]]
    )
    updateNumericInput(session, paste0("f_Stat_",i),
                       value = rv[[paste0("nic_Stat_",i)]]
    )
    updateNumericInput(session, paste0("f_sign_",i),
                       value = rv[[paste0("nic_sign_",i)]]
    )
  }
})



# UPON CLICKING APPLY BUTTON in the VIS TAB, update filter values into rv
observeEvent(input$nic_applytorv, {
  req(is.null(rv$nx_n)==F)
  
  for (i in 1:length(rv$nx_n)){
    req(is.null(input[[paste0("nic_p_",i)]])==F)
    req(is.null(input[[paste0("nic_q_",i)]])==F)
    req(is.null(input[[paste0("nic_Stat_",i)]])==F)
    req(is.null(input[[paste0("nic_sign_",i)]])==F)
    
    rv[[paste0("nic_p_",i)]] <- input[[paste0("nic_p_",i)]]
    rv[[paste0("nic_q_",i)]] <- input[[paste0("nic_q_",i)]]
    rv[[paste0("nic_Stat_",i)]] <- input[[paste0("nic_Stat_",i)]]
    rv[[paste0("nic_sign_",i)]] <- input[[paste0("nic_sign_",i)]]
    
    updateNumericInput(session, paste0("nic_p_",i),
                       value = rv[[paste0("nic_p_",i)]]
    )
    updateNumericInput(session, paste0("nic_q_",i),
                       value = rv[[paste0("nic_q_",i)]]
    )
    updateNumericInput(session, paste0("nic_Stat_",i),
                       value = rv[[paste0("nic_Stat_",i)]]
    )
    updateNumericInput(session, paste0("nic_sign_",i),
                       value = rv[[paste0("nic_sign_",i)]]
    )
  }
})



# =========================== apply filters panel on the side

output$f_apply_filters_panel <- renderUI({
  if(is.null(rv$nx_n)==F){
    div(
      HTML("<b>Filter presets:</b><br>Click to load one or multiple presets:<br>"),
      hr(),
      # dynamically render the list of presets as buttons
      tagList(lapply(1:length(filter_presets), function(i) {
        name <- names(filter_presets)[[i]]
        preset <- filter_presets[[i]]
        actionButton(inputId = paste0("fpreset_", preset[[1]]),
                     label = name
        )
      })),
      
      
      hr(),
      actionButton("f_applytorv", "Apply Filters", class = "btn-warning"),
      uiOutput("f_msg")
    )
  } else {
    HTML("Please select datasets to continue.")
  }
  
})

# observe these buttons and update filters when any is pressed
observeEvent(input[[paste0("fpreset_",filter_presets[[1]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[1]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("f_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("f_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("f_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("f_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("fpreset_",filter_presets[[2]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[2]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("f_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("f_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("f_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("f_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("fpreset_",filter_presets[[3]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[3]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("f_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("f_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("f_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("f_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("fpreset_",filter_presets[[4]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[4]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("f_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("f_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("f_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("f_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("fpreset_",filter_presets[[5]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[5]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("f_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("f_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("f_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("f_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("fpreset_",filter_presets[[6]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[6]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("f_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("f_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("f_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("f_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("fpreset_",filter_presets[[7]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[7]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("f_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("f_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("f_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("f_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("fpreset_",filter_presets[[8]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[8]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("f_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("f_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("f_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("f_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("fpreset_",filter_presets[[9]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[9]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("f_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("f_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("f_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("f_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("fpreset_",filter_presets[[10]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[10]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("f_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("f_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("f_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("f_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("fpreset_",filter_presets[[11]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[11]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("f_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("f_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("f_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("f_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("fpreset_",filter_presets[[12]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[12]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("f_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("f_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("f_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("f_sign_",x), selected=preset[[5]]) }
  }
})


# show a message if there are unapplied filters
output$f_msg <- renderUI({
  req(nrow(rv$df_n)>0)
  req(length(f_temp_gls())>0)
  req(length(n_ins_gls())>0)
  
  temp <- f_temp_gls()
  rvgl <- n_ins_gls()
  notequal <- vector()
  for (i in 1:length(rv$nx_n)){
    if (setequal(temp[[i]],rvgl[[i]])==F){
      notequal <- c(notequal, rv$nx_n[[i]])
    }
  }
  
  if(length(notequal)>0){
    HTML(paste0(
      "<br><b><i>You have unapplied changes in:</b></i><br>",
      paste(notequal, collapse=", "),
      "<br><b><i>These will be lost when you leave this page.</b></i>"
    ))
  }
})



# ============================ show the temporary filtered tables (BASED UPON THE FILTERS ON THE LEFT)
# apply cutoffs and generate temporary genelists
f_temp_gls <- reactive({
  req(nrow(rv$df_n)>0) # master df must not be empty
  # req(length(rv$s)==length(rv$nx_i)) # make sure selections are fully rendered
  
  df <- rv$df_n # start from master df, not the one cut by names
  
  gls <- vector(mode="list") # initialize gls as empty list
  
  # cutoff according to filters provided for each 
  for (i in 1:length(rv$nx_n)){
    req(input[[paste0("f_p_",i)]])
    req(input[[paste0("f_q_",i)]])
    req(input[[paste0("f_Stat_",i)]])
    req(input[[paste0("f_sign_",i)]])
    
    n <- rv$nx_n[[i]] # name
    ss <- df
    ss <- ss[ss[[paste0("PValue","_", n)]]<=input[[paste0("f_p_",i)]], ] # filter by p
    ss <- ss[ss[[paste0("FDR","_", n)]]<=input[[paste0("f_q_",i)]], ] # filter by q
    ss <- ss[abs(ss[[paste0("Stat","_", n)]])>=input[[paste0("f_Stat_",i)]], ] # filter by stat
    ss <- filter_by_sign(ss, paste0("Stat","_", n), input[[paste0("f_sign_",i)]], tolerate=T) # filter by stat sign
    
    gl <- as.character(na.omit(ss$Name)) # format into vector genelist
    gls[[n]] <- gl # write into list as named vector
  }
  
  return(gls)
})
# show the filtered tables based on the generated gene lists
observe({
  req(is.null(rv$nx_n)==F)
  lapply(1:length(rv$nx_n), function(x) {
    output[[paste0('T', x)]] <- DT::renderDataTable({
      
      df <- rv$df_n
      gl <- f_temp_gls()[[x]]
      show_cols <- c("Name", paste0(c("Stat_", "PValue_", "FDR_"), rv$nx_n[[x]]))
      df <- df[df$Name %in% gl, show_cols]
      colnames(df) <- c("Name", "Stat", "PValue", "FDR")
      rownames(df) <- NULL
      df <- df %>% mutate(across(is.numeric, ~ round(., 3))) # round
      rv[[paste0("f_temp_rown_",x)]] <- nrow(df)
      df
      
      
    }, options=list(scrollX=T, pageLength = 5, dom = 'tpr', pagingType = "simple"), rownames= FALSE)
  })
})
# show the rown of temp table
observe({
  lapply(1:length(rv$nx_n), function(x) {
    req(is.null(rv[[paste0("f_temp_rown_",x)]])==F)
    output[[paste0('T_info', x)]] <- renderUI({
      HTML(paste0("<i>", rv[[paste0("f_temp_rown_",x)]],
                  " out of ", nrow(rv$df_n), " total</i>"))
    })
  })
})

# show the filtered tables based on the stable rv
observe({
  req(is.null(rv$nx_n)==F)
  lapply(1:length(rv$nx_n), function(x) {
    output[[paste0('TT', x)]] <- DT::renderDataTable({
      
      df <- rv$df_n
      gl <- n_ins_gls()[[x]]
      show_cols <- c("Name", paste0(c("Stat_", "PValue_", "FDR_"), rv$nx_n[[x]]))
      df <- df[df$Name %in% gl, show_cols]
      colnames(df) <- c("Name", "Stat", "PValue", "FDR")
      rownames(df) <- NULL
      df <- df %>% mutate(across(is.numeric, ~ round(., 3))) # round
      rv[[paste0("f_rv_rown_",x)]] <- nrow(df)
      df
      
    }, options=list(scrollX=T, pageLength = 5, dom = 'tpr', pagingType = "simple"), rownames= FALSE)
  })
})

# =============================== show the gene list filters with the temp filtered table inside
output$f_filtering_ui <- renderUI({
  if(is.null(rv$nx_n)==T){
    box(
      title = span( icon("exclamation"), "Notification"), status = "warning", width=12,
      "No data selected."
    )
  } else {
    tagList(lapply(1:length(rv$nx_n), function(i) {
      box(
        title = NULL, status = "primary", solidHeader = F, width=12,
        column(4,
               HTML(paste0("<b>",rv$nx_n[[i]],"</b>")),br(),
               hr(),
               wellPanel( align = "left",
                          
                          fluidRow(
                            column(6, align = "left", 
                                   numericInput(inputId = paste0("f_p_",i), 
                                                "P filter:", value = 0.05, min = 0, max = 1, step=0.001, width="100px")),
                            column(6, align = "left",
                                   numericInput(paste0("f_Stat_",i), 
                                                "|Stat| filter:", value = 0, min = 0, max = 5, step=0.1, width="100px")),
                          ),
                          fluidRow(
                            column(6, align = "left",
                                   numericInput(inputId = paste0("f_q_",i), 
                                                "FDR filter:", value = 1, min = 0, max = 1, step=0.001, width="100px")),
                            column(6, align = "left",
                                   radioGroupButtons(inputId = paste0("f_sign_",i), 
                                                     label = "Filter by sign:",
                                                     choices=c("All"="All", "+"="Positive", "-"="Negative"),
                                                     selected="All",size="s",direction = "horizontal"),
                            ),
                          )
                          ,style = "padding: 15px;margin-top:10px;")
        ),
        column(4,
               HTML("<b>Preview:</b>"),
               uiOutput(paste0("T_info",i)),
               div(dataTableOutput(paste0('T', i)), style="font-size:90%;")
        ),
        column(4,
               HTML("<b>Applied filters:</b>"),br(),
               HTML(paste0("<i>", length(n_ins_gls()[[i]]),
                           " out of ", nrow(rv$df_n), " total</i>")),
               div(dataTableOutput(paste0('TT', i)), style="font-size:90%;")
        )
        
      )
    }))
  }

})




# lmUI <- function(id) {
#   ns <- shiny::NS(id)
#   uiOutput(ns("lmModel"))
# }
# 
# lmModelModule <- function(input, output, session, nx_n) {
#   data(mtcars)
#   cols <- sort(unique(names(mtcars)[names(mtcars) != 'mpg']))
#   # lmModel <- reactive({
#   #   lm(sprintf('mpg ~ %s',paste(input$vars, collapse = '+')), data = mtcars)
#   # })
#   lmTable <- reactive({
#     mtcars[,input$vars]
#   })
#   output[['lmModel']] <- renderUI({
#     ns <- session$ns
#     current_ns <- (environment(ns)[['namespace']])
#     current_n <- strsplit(current_ns, "_")[[1]][[2]]
#     tags$div(id = environment(ns)[['namespace']],
#              tagList(
#                box(
#                  title = span(icon("cut"),"Select datasets"), status = "primary", solidHeader = F, width=12,
#                  fluidRow(
#                    column(4,
#                           tags$h3(current_n),
#                           tags$h3(current_n),
#                           selectInput(ns('vars'),
#                                       'Select dependent variables',
#                                       choices = cols,
#                                       selected = cols[1:2],
#                                       multiple = TRUE)),
#                    column(8, 
#                           renderDataTable({lmTable()})
#                    ),
#                    # column(4, 
#                    #        renderPlot({par(mfrow = c(2,2))
#                    #          plot(lmModel())})
#                    # )
#                  )
#                )
#              )
#     
#     )
#   })
# }
# 
# 
# observeEvent(input$n_use_data, {
# 
#   nx_n <- rv$nx_n
#   for (x in 1:length(nx_n)){
#     id <- sprintf('lmModel_%s', x)
# 
#     insertUI(
#       selector = '#f_add_here', # define something as selector
#       where = "beforeBegin", # adds element before selector itself
#       ui = lmUI(id)
#     )
#     namew <- isolate(nx_n[[x]])
#     name <- reactive(namew)
#     # print(nx_n[[x]])
#     callModule(lmModelModule, id, nx_n = reactive(rv$nx_n))
#   }
# 
# })