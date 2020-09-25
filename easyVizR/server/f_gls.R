#========================= APPLY FILTERS TAB ==============================#

# min amount of shared cols and rows for dataset selection (default: 1 row/ 3 cols)
min_shared_rows <- 1
min_shared_cols <- 3 # this excludes the name column


# observe the changes made to the boxes. if there are unsaved changes (i.e. not equal to rv value), highlight them red.

# css red shadowy border effect on inputs and radiogroupbuttons:
# box-shadow: 0 0 3px red;
# border: 0.1em solid red;
# normal:
# box-shadow: none;
# border: 1px solid #d2d6de;

observe({
  highlights <- vector()
  for (i in 1:length(rv$nx_n)){
    req(input[[paste0("f_p_",i)]])
    req(input[[paste0("f_q_",i)]])
    req(input[[paste0("f_Stat_",i)]])
    req(input[[paste0("f_sign_",i)]])
    req(rv[[paste0("nic_p_",i)]])
    req(rv[[paste0("nic_q_",i)]])
    req(rv[[paste0("nic_Stat_",i)]])
    req(rv[[paste0("nic_sign_",i)]])
    
    warning_style <- "{box-shadow: 0 0 3px red;border: 0.1em solid red;}"
    normal_textinput_style <- "{box-shadow: none;border: 1px solid #d2d6de;}"
    normal_radiogroupbuttons_style <- "{box-shadow: none;border: #f4f4f4;}"
    
    # observe p, q, stat, sign and apply highlights if diff from saved rv value
    if (input[[paste0("f_p_",i)]] != rv[[paste0("nic_p_",i)]]){
      obs <- paste0("#", paste0("f_p_",i), warning_style)
      highlights <- c(highlights, obs)
    } else {
      obs <- paste0("#", paste0("f_p_",i), normal_textinput_style)
      highlights <- c(highlights, obs)
    }
    if (input[[paste0("f_q_",i)]] != rv[[paste0("nic_q_",i)]]){
      obs <- paste0("#", paste0("f_q_",i), warning_style)
      highlights <- c(highlights, obs)
    } else {
      obs <- paste0("#", paste0("f_q_",i), normal_textinput_style)
      highlights <- c(highlights, obs)
    }
    if (input[[paste0("f_Stat_",i)]] != rv[[paste0("nic_Stat_",i)]]){
      obs <- paste0("#", paste0("f_Stat_",i), warning_style)
      highlights <- c(highlights, obs)
    } else {
      obs <- paste0("#", paste0("f_Stat_",i), normal_textinput_style)
      highlights <- c(highlights, obs)
    }
    if (input[[paste0("f_sign_",i)]] != rv[[paste0("nic_sign_",i)]]){
      obs <- paste0("#", paste0("f_sign_",i), warning_style)
      highlights <- c(highlights, obs)
    } else {
      obs <- paste0("#", paste0("f_sign_",i), normal_radiogroupbuttons_style)
      highlights <- c(highlights, obs)
    }
  }
  
  rv$f_css_highlights <- tags$head(tags$style(HTML(
    paste0(highlights, sep=" ")
    )))
  
})

output$f_highlights <- renderUI({
  rv$f_css_highlights
})



####---------------------- SELECT DATASETS ---------------------------####

# select data
output$select_df_p2 <- renderUI({
  req(length(rv$ll) >= 1)
  checkboxGroupInput(
    inputId = "heatmap_dfs",
    label= shiny::HTML("Select 2 or more datasets:"),
    choices = rv$ll)
})

# feedback on whether the data has enough shared rows/cols
output$n_shared <- renderUI({
  # req(is.null(rv$n_sharedcols)==F)
  # req(is.null(rv$n_sharedrows)==F)
  
  msg <- vector()
  if(length(input$heatmap_dfs) < 2){ # first check if enough n is selected
    boxcolor <- "black"
    msg <- c(msg, "Please select 2 or more datasets.")
  } else { # if enough n is selected, check sharedcols
    errors <- 0
    if (length(rv$n_sharedcols)<min_shared_cols){ # check if enough shared cols
      msg <- c(msg, 
               paste0("You only have ", length(rv$n_sharedcols), " shared columns: ", 
                      paste(rv$n_sharedcols, sep=", "), " (",min_shared_cols," needed)."
                      )
               )
      errors <- errors+1
    }
    if (length(rv$n_sharedrows)<min_shared_rows){ # check if enough shared rows
      msg <- c(msg, 
               paste0("No term overlaps detected in selected datasets; <br>please check if the same gene identifiers are used."
               )
      )
      errors <- errors+1
    }
    if (errors >0){
      boxcolor="red"
    } else { 
      boxcolor = "green"
      msg <- c(msg, "Datasets ok!")
    }
  }
  
  sharemsg <- paste(msg, sep="<br>")
  
  box(
    title = NULL, background = boxcolor, solidHeader = TRUE, width=12,
    HTML(sharemsg)
  )
})


# only enable the button if all requirements satisfied
observe({
  if (length(rv$ll) >= 2 & length(rv$heatmap_i) > 1 & length(rv$n_sharedrows)>=min_shared_rows & length(rv$n_sharedcols)>=min_shared_cols){
    shinyjs::enable("n_use_data")
  } else {
    shinyjs::disable("n_use_data")
  }
})



####---------------------- APPLY FILTERS ---------------------------####

output$f_apply_filters_panel <- renderUI({
  if(is.null(rv$nx_n)==F){
    div(
      ######
      # div(id="f_show_current",
      #     
      #     div(
      #       # style="margin-bottom:10px;",
      #         HTML(paste0(
      #           "<b>Filters</b>:",
      #           add_help("f_presets_current", style="margin-left: 5px;"))
      #         )
      #     ),
      #     bsTooltip("f_presets_current", 
      #               "These are your unsaved filter selections.", 
      #               placement = "top"),
      #     
      #     # HTML("p <= 0.05 in <b><i>all</i></b>")
      #     
      #     ),
      # 
      # 
      # hr(),
      ######
      div(id="f_presets_panel",
          
          HTML(paste0(
            "<b>Filter preset shortcuts</b>:",
            add_help("f_presets_help", style="margin-left: 5px;"))
          ),
          bsTooltip("f_presets_help", 
                    "Click on these buttons to apply filter presets to all datasets (effects are stackable).<br>Click on <b>No Filter</b> to remove all filters.", 
                    placement = "top"),
          ######
          # dropdown(
          #   
          # 
          #   size = "xs",
          #   icon = icon("gear", class = "opt")
          # ),
          ######
          
          
          # dynamically render the list of presets as buttons
          tagList(lapply(1:length(filter_presets), function(i) {
            name <- names(filter_presets)[[i]]
            preset <- filter_presets[[i]]
            actionButton(inputId = paste0("fpreset_", preset[[1]]),
                         label = name, 
                         icon(preset[[7]]),
                         style=paste0("color:",preset[[8]],";background-color:",preset[[9]],";")
            )
          })),
          # dynamically render the preset tooltips
          tagList(lapply(1:length(filter_presets), function(i) {
            preset <- filter_presets[[i]]
            bsTooltip(id=paste0("fpreset_", preset[[1]]), 
                      title=preset[[6]], 
                      placement = "top")
          })),
          
          
          ),
      
      
      
      hr(),
      
      
      uiOutput("f_msg"),br(),
      
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
observeEvent(input[[paste0("fpreset_",filter_presets[[13]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[13]]
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
  # req(length(f_temp_gls())>0)
  # req(length(n_ins_gls())>0)
  
  # temp <- f_temp_gls()
  # rvgl <- n_ins_gls()
  notequal <- vector()
  for (i in 1:length(rv$nx_n)){
    # # observe by gene list
    # if (setequal(temp[[i]],rvgl[[i]])==F){
    #   notequal <- c(notequal, rv$nx_n[[i]])
    # }
    
    # observe by filter values
    req(input[[paste0("f_p_",i)]])
    req(input[[paste0("f_q_",i)]])
    req(input[[paste0("f_Stat_",i)]])
    req(input[[paste0("f_sign_",i)]])
    req(rv[[paste0("nic_p_",i)]])
    req(rv[[paste0("nic_q_",i)]])
    req(rv[[paste0("nic_Stat_",i)]])
    req(rv[[paste0("nic_sign_",i)]])
    
    if (input[[paste0("f_p_",i)]] != rv[[paste0("nic_p_",i)]] |
        input[[paste0("f_q_",i)]] != rv[[paste0("nic_q_",i)]] |
        input[[paste0("f_Stat_",i)]] != rv[[paste0("nic_Stat_",i)]] |
        input[[paste0("f_sign_",i)]] != rv[[paste0("nic_sign_",i)]]
    ) {
      notequal <- c(notequal, rv$nx_n[[i]])
    }
  }
  
  if(length(notequal)>0){
    div(
      actionButton("f_applytorv", "Save Filters", class = "btn-warning"),
      bsTooltip("f_applytorv","You can change these filters in later panels.", "right"),
      HTML(paste0(
        "<br><br><b><i>You have unsaved changes in:</b></i><br>",
        paste(notequal, collapse=", "),
        "<br><b><i>These will be lost when you leave this page.</b></i>"
      ))
    )
  } else {
    HTML(paste0(
      "<b><i>All changes saved.</b></i>"
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
      colnames(df) <- stat_replace1(colnames(df), rv$nx_n[[x]]) # replace stat string
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
      colnames(df) <- stat_replace1(colnames(df), rv$nx_n[[x]]) # replace stat string
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
                                                    stat_replace1("|Stat| >=:", rv$nx_n[[i]]),
                                                    value = 0, min = 0, max = 5, step=0.1, width="100px")),
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
                              ,style = "padding: 15px;margin-top:10px;"),
                   
            ),
            
            
            # temp table
            column(4,style="padding-right:17px;",
                   HTML("<b>Preview gene list:</b>"),
                   uiOutput(paste0("T_info",i)),
                   div(dataTableOutput(paste0('T', i)), style="font-size:90%;"),
                   div(style="position: absolute;top: 150px;right: -15px;color: cornflowerblue;font-size: 23px;",
                       icon("arrow-right")
                   )
            ),
            
            # saved table
            column(4,style="padding-left:17px;",
                   HTML("<b>Saved gene list:</b>"),br(),
                   HTML(paste0("<i>", length(n_ins_gls()[[i]]),
                               " out of ", nrow(rv$df_n), " total</i>")),
                   div(dataTableOutput(paste0('TT', i)), style="font-size:90%;")
            )
            
            
          )
        })),
        
        uiOutput("f_highlights")
        
        )
    
  }

})

