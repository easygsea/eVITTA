#========================= APPLY FILTERS TAB ==============================#

####---------------------- SELECT DATASETS ---------------------------####

# select data
output$select_df_p2 <- renderUI({
  req(length(rv$ll) >= 1)
  checkboxGroupInput(
    inputId = "heatmap_dfs",
    label= shiny::HTML("Select from uploaded datasets: 
                               <span style='color: gray'>(2 or more required)</span>"),
    choices = rv$ll)
})

# feedback on whether the data has enough shared rows/cols
output$n_shared <- renderUI({
  # req(is.null(rv$n_sharedcols)==F)
  # req(is.null(rv$n_sharedrows)==F)
  
  if (length(rv$n_sharedcols)>=1){msgx=" (ok)"}
  else{ msgx=""}
  if (length(rv$n_sharedrows)>=1){msgy=" (ok)"}
  else{ msgy=""}
  
  
  if(length(input$heatmap_dfs) < 2){
    box(
      title = NULL, background = "black", solidHeader = TRUE, width=12,
      "Not enough datasets selected."
    )
  }
  else if (msgx==" (ok)" & msgy==" (ok)"){
    box(
      title = NULL, background = "green", solidHeader = TRUE, width=12,
      paste0("Shared columns: ",length(rv$n_sharedcols), msgx),br(),
      paste0("Shared rows: ",length(rv$n_sharedrows), msgy)
    )
  }
  else{
    box(
      title = NULL, background = "red", solidHeader = TRUE, width=12,
      paste0("Shared columns: ",length(rv$n_sharedcols), msgx),br(),
      paste0("Shared rows: ",length(rv$n_sharedrows), msgy)
    )
  }
})

output$n_shared_cols <- renderText({
  req(length(rv$ll) >= 1)
  if(length(input$heatmap_dfs) >= 2){
    if (length(rv$n_sharedcols)>=1){msg=" (ok)"}
    else{msg=" (x)"}
    paste0("Shared columns: ",length(rv$n_sharedcols), msg)
  }
  
  else{ "Shared columns: "}
  
  
})
output$n_shared_rows <- renderText({
  req(length(rv$ll) >= 1)
  if(length(input$heatmap_dfs) >= 2){
    if (length(rv$n_sharedrows)>=1){msg=" (ok)"}
    else{msg=" (x)"}
    paste0("Shared rows: ",length(rv$n_sharedrows), msg)
  }
  
  else{ "Shared rows: "}
})

# only enable the button if all requirements satisfied
observe({
  if (length(rv$ll) >= 2 & length(rv$heatmap_i) > 1 & length(rv$n_sharedrows)>=1 & length(rv$n_sharedcols)>=1){
    shinyjs::enable("n_use_data")
  } else {
    shinyjs::disable("n_use_data")
  }
})



####---------------------- APPLY FILTERS ---------------------------####

output$f_apply_filters_panel <- renderUI({
  if(is.null(rv$nx_n)==F){
    div(
      div(id="f_presets_panel",
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
          ),
      
      
      
      hr(),
      actionButton("f_applytorv", "Apply Filters", class = "btn-warning"),
      bsTooltip("f_applytorv","You can change these filters anytime in the Gene List Filters dropdown", "right"),
      
      uiOutput("f_msg"),br(),
      
      HTML("Note: You can change these filters in the Gene List Filters dropdown in later tabs.")
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
    updateRadioGroupButtons(session, paste0("f_sign_",i),
                            selected = rv[[paste0("nic_sign_",i)]]
    )
  }
})


####---------------------- DETAILED GENELIST FILTERING PANELS ---------------------------####


# --------------- show temporary filtered tables ----------------
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

# --------------- ASSEMBLE INTO A MULTI-PANEL UI -------------------
output$f_filtering_ui <- renderUI({
  if(is.null(rv$nx_n)==T){
    box(
      title = span( icon("exclamation"), "Notification"), status = "warning", width=12,
      "No data selected."
    )
  } else {
    div(id="f_filtering_panels",
        tagList(lapply(1:length(rv$nx_n), function(i) {
          box(
            title = NULL, status = "primary", solidHeader = F, width=12,
            
            column(4, 
                   div(style="word-break: break-all;",
                       HTML(paste0("<b>",rv$nx_n[[i]],"</b>")),br(),
                   ),
                   
                   hr(),
                   wellPanel(id=paste0("f_panel_",i),align = "left",
                              
                              fluidRow(
                                column(6, align = "left", 
                                       numericInput(inputId = paste0("f_p_",i), 
                                                    "P <=:", value = 0.05, min = 0, max = 1, step=0.001, width="100px")),
                                column(6, align = "left",
                                       numericInput(paste0("f_Stat_",i), 
                                                    "|Stat| >=:", value = 0, min = 0, max = 5, step=0.1, width="100px")),
                              ),
                              fluidRow(
                                column(6, align = "left",
                                       numericInput(inputId = paste0("f_q_",i), 
                                                    "FDR <=:", value = 1, min = 0, max = 1, step=0.001, width="100px")),
                                column(6, align = "left",
                                       radioGroupButtons(inputId = paste0("f_sign_",i), 
                                                         label = "Direction:",
                                                         choices=c("All"="All", "+"="Positive", "-"="Negative"),
                                                         selected="All",size="s",direction = "horizontal"),
                                ),
                              )
                              ,style = "padding: 15px;margin-top:10px;")
            ),
            column(4,style="padding-right:17px;",
                   HTML("<b>Filter preview:</b>"),
                   uiOutput(paste0("T_info",i)),
                   div(dataTableOutput(paste0('T', i)), style="font-size:90%;"),
                   div(style="position: absolute;top: 150px;right: -15px;color: cornflowerblue;font-size: 23px;",
                       icon("arrow-right")
                   )
            ),
            
            
            column(4,style="padding-left:17px;",
                   HTML("<b>Applied filters:</b>"),br(),
                   HTML(paste0("<i>", length(n_ins_gls()[[i]]),
                               " out of ", nrow(rv$df_n), " total</i>")),
                   div(dataTableOutput(paste0('TT', i)), style="font-size:90%;")
            )
            
            
          )
        }))
        )
    
  }

})

