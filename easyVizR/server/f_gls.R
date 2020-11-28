#========================= APPLY FILTERS TAB ==============================#

# min amount of shared cols and rows for dataset selection (default: 1 row/ 3 cols)
min_shared_rows <- 1
min_shared_cols <- 3 # this excludes the name column





####---------------------- SELECT DATASETS ---------------------------####

# select data
output$select_df_p2 <- renderUI({
  if(rv$demo_n %% 2 == 0){
    updateCheckboxGroupInput(session, inputId = "heatmap_dfs", selected = rv$ll)
  }
  req(length(rv$ll) >= 1)
  checkboxGroupInput(
    inputId = "heatmap_dfs",
    label= shiny::HTML("Select 2 or more datasets:"),
    choices = rv$ll
    #default to select all the datasets
    #,selected = list("A549-ACE2_SARS-CoV-2_HiMOI_KEGG-WkPt-RctP-BlgP")
    )
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

# ========== assemble apply filter left panel ==============
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
            add_help("f_presets_help", style="margin-left: 5px;margin-bottom:0.8em;"))
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
          
          # reset all changes button
          actionButton(inputId = "f_reset",
                       label = "Reset changes", 
                       icon("undo-alt"),
                       style=paste0("color:#f4f4f4; background-color:#444;")
          ),
          bsTooltip(id="f_reset", 
                    title="Reset all unsaved changes", 
                    placement = "top")
          
          ),
      
      
      
      hr(),
      
      
      uiOutput("f_msg"),br(),
      
    )
  } else {
    HTML("Please select datasets to continue.")
  }
  
})

# ========== preset buttons ==============

# observe preset buttons and update filters when any is pressed
#----------------------------------------------------------------
observeEvent(input[[paste0("fpreset_",filter_presets[[1]][[1]])]], {
  apply_presets_to_filterinputs(1, "f", presets=filter_presets)
})
observeEvent(input[[paste0("fpreset_",filter_presets[[2]][[1]])]], {
  apply_presets_to_filterinputs(2, "f", presets=filter_presets)
})
observeEvent(input[[paste0("fpreset_",filter_presets[[3]][[1]])]], {
  apply_presets_to_filterinputs(3, "f", presets=filter_presets)
})
observeEvent(input[[paste0("fpreset_",filter_presets[[4]][[1]])]], {
  apply_presets_to_filterinputs(4, "f", presets=filter_presets)
})
observeEvent(input[[paste0("fpreset_",filter_presets[[5]][[1]])]], {
  apply_presets_to_filterinputs(5, "f", presets=filter_presets)
})
observeEvent(input[[paste0("fpreset_",filter_presets[[6]][[1]])]], {
  apply_presets_to_filterinputs(6, "f", presets=filter_presets)
})
observeEvent(input[[paste0("fpreset_",filter_presets[[7]][[1]])]], {
  apply_presets_to_filterinputs(7, "f", presets=filter_presets)
})
observeEvent(input[[paste0("fpreset_",filter_presets[[8]][[1]])]], {
  apply_presets_to_filterinputs(8, "f", presets=filter_presets)
})
observeEvent(input[[paste0("fpreset_",filter_presets[[9]][[1]])]], {
  apply_presets_to_filterinputs(9, "f", presets=filter_presets)
})
observeEvent(input[[paste0("fpreset_",filter_presets[[10]][[1]])]], {
  apply_presets_to_filterinputs(10, "f", presets=filter_presets)
})
observeEvent(input[[paste0("fpreset_",filter_presets[[11]][[1]])]], {
  apply_presets_to_filterinputs(11, "f", presets=filter_presets)
})
observeEvent(input[[paste0("fpreset_",filter_presets[[12]][[1]])]], {
  apply_presets_to_filterinputs(12, "f", presets=filter_presets)
})
observeEvent(input[[paste0("fpreset_",filter_presets[[13]][[1]])]], {
  apply_presets_to_filterinputs(13, "f", presets=filter_presets)
})




# observe the reset changes button
#-------------------------------------
observeEvent(input$f_reset, {
  req_filter_ns("f", input)
  req_filter_ns("nic", rv)
  update_filters("f", "nic", rv)
})

# ========== unsaved highlights ==============

# observe the changes made to the filters, and highlight unsaved changes (i.e. different from rv)
#-----------------------------------------
observe({
  req_filter_ns("f", input, guard_na=T)
  req_filter_ns("nic", rv)
  rv$f_css_highlights <- observe_filter_highlights("f", input, "nic", rv)
})

output$f_highlights <- renderUI({
  rv$f_css_highlights
})



# show a message if there are unapplied filters
#-----------------------------------------------
output$f_msg <- renderUI({
  req(nrow(rv$df_n)>0)

  notequal <- vector()
  
  # require filter namespaces to exist
  req_filter_ns("f", input)
  req_filter_ns("nic", rv)
  
  for (i in 1:length(rv$nx_n)){
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


# ========== save filters ==============

# UPON CLICKING APPLY BUTTON in the FILTER TAB, update filter values into rv
# ----------------------------------------------------
observeEvent(input$f_applytorv, {
  req(is.null(rv$nx_n)==F)
  
  req_filter_ns("f", input)
  req_filter_ns("nic", rv)
  
  update_filters_rv("nic", "f")
  # update the input by rv again to prevent reverting to input defaults.
  update_filters("f", "nic")

})


####---------------------- DETAILED GENELIST FILTERING PANELS ---------------------------####


# --------------- show temporary filtered tables ----------------
# apply cutoffs and generate temporary genelists
f_temp_gls <- reactive({
  req_filter_ns("f", input)
  
  filter_to_gls("f",input,rv$df_n)
})




# show the filtered tables based on the generated gene lists
observe({
  req(is.null(rv$nx_n)==F)
  lapply(1:length(rv$nx_n), function(x) {
    output[[paste0('T', x)]] <- DT::renderDataTable({
      
      df <- gl_to_table(name = rv$nx_n[[x]], 
                        gl = f_temp_gls()[[x]], 
                        master_df = rv$df_n, 
                        round=3)
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
      
      df <- gl_to_table(name = rv$nx_n[[x]], 
                        gl = n_ins_gls()[[x]], 
                        master_df = rv$df_n, 
                        round=3)
      rv[[paste0("f_rv_rown_",x)]] <- nrow(df) # save the rown into rv for display
      df
      
    }, options=list(scrollX=T, pageLength = 5, dom = 'tpr', pagingType = "simple"), rownames= FALSE)
  })
})

# show the filtered tables based on the stable rv
observe({
  req(is.null(rv$nx_n)==F)
  lapply(1:length(rv$nx_n), function(x) {
    output[[paste0('TT_dl', x)]] <- downloadHandler( # needs to be exactly the same as table render
      filename = function() {
        paste("genelist", "-", rv$nx_n[[x]], "-", Sys.Date(), ".csv", sep="")},
      content = function(file) {
        write.csv({
          
          df <- gl_to_table(name = rv$nx_n[[x]], 
                            gl = n_ins_gls()[[x]], 
                            master_df = rv$df_n, 
                            round=0) # no rounding
          df
          
        }, file, row.names = F, quote=TRUE)})
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
                                                    stat_replace1(
                                                      HTML(paste0(
                                                        "<b>|Stat| >=</b>:",
                                                        add_help(paste0("f_stat_help",i), style="margin-left: 5px;"))
                                                      )
                                                      , rv$nx_n[[i]]),
                                                    value = 0, min = 0, max = 5, step=0.1, width="100px")),
                                
                                bsTooltip(paste0("f_stat_help",i), 
                                          paste0(stat_replace1("Stat type: <b>", rv$nx_n[[i]]),rv$tt[[rv$nx_i[[i]]]],"</b>"), 
                                          placement = "top"),
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
                   HTML(paste0(
                     "<b>Preview gene list</b>:",
                     add_help(paste0("f_previewdf_help",i), style="margin-left: 5px;"))
                   ),
                   bsTooltip(paste0("f_previewdf_help",i), 
                             "Gene list generated by your current (unsaved) filters.", 
                             placement = "top"),
                   uiOutput(paste0("T_info",i)),
                   div(dataTableOutput(paste0('T', i)), style="font-size:90%;"),
                   
                   div(style="position: absolute;top: 150px;right: -15px;color: cornflowerblue;font-size: 23px;",
                       icon("arrow-right")
                   )
            ),
            
            # saved table
            column(4,style="padding-left:17px;",
                   HTML(paste0(
                     "<b>Saved gene list</b>:",
                     add_help(paste0("f_saveddf_help",i), style="margin-left: 5px;"))
                   ),
                   bsTooltip(paste0("f_saveddf_help",i), 
                             "Gene list from the saved filters. <br>(these are used for visualizations)", 
                             placement = "top"),
                   br(),
                   HTML(paste0("<i>", length(n_ins_gls()[[i]]),
                               " out of ", nrow(rv$df_n), " total</i>")),
                   div(dataTableOutput(paste0('TT', i)), style="font-size:90%;"),
                   
                   # dropdowns
                   div(style = "position: absolute; left: 1em; bottom: 0.5em",
                       dropdown(
                         downloadButton(paste0('TT_dl', i), "Download saved gene list")
                         ,
                         size = "xs",
                         icon = icon("download", class = "opt"),
                         up = TRUE
                       )
                   )
                   
            )
            
            
          )
        })),
        
        uiOutput("f_highlights")
        
        )
    
  }

})

