#======================================================================#
####                 UPDATE upon tab switching                      ####
#======================================================================#

# UPON ENTERING EACH TAB, update SHARED WIDGETS in that tab

observeEvent(input$tabs, {
  req(is.null(rv$nx_n)==F)
  # print(input$tabs)
  if (input$tabs == "tab_filters"){
    
    # ---------------------update FILTER VALUES
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
    
    # ---------------------move INS TABLE to current tab
    removeUI(
      selector = "#ins_table_panel"
    )
    # print("removed ui")
    insertUI(
      selector = "#ins_pg_bottom",
      where = "afterEnd",
      ui = uiOutput("ins_table_panel")
    )
    # print("inserted ui in p3")
    
    
    # ---------------------move DROPDOWN BUTTONS to current tab
    removeUI(
      selector = "#n_filters"
    )
    # print("removed ui")
    insertUI(
      selector = "#ins_filters_here",
      where = "afterEnd",
      ui = uiOutput("n_filters")
    )
    # print("inserted ui in p4")
    
  } else if (input$tabs == "tab3"){
    
    # --------------------- move INS TABLE to current tab
    removeUI(
      selector = "#ins_table_panel"
    )
    # print("removed ui")
    insertUI(
      selector = "#vis_pg_bottom",
      where = "afterEnd",
      ui = uiOutput("ins_table_panel")
    )
    # print("inserted ui in p4")
    
    # ---------------------move DROPDOWN BUTTONS to current tab
    removeUI(
      selector = "#n_filters"
    )
    # print("removed ui")
    insertUI(
      selector = "#n_filters_here",
      where = "afterEnd",
      ui = uiOutput("n_filters")
    )
    # print("inserted ui in p4")
    
    
  }
  
})



#======================================================================#
####                        FILTER DROPDOWNS                        ####
#======================================================================#

# filters are found in both the filter tab and as dropdowns in ther other 2 tabs
# former code is in f_gls.R
# code for the dropdowns is here


output$n_filters <- renderUI({
  div(
    div(id="n_filter_cuts", style="display: inline-block;margin-right: 5px;", customize_filters()),
    bsTooltip("customize_filters", "Gene list filters"),
    div(id="n_filter_gls", style="display: inline-block;margin-right: 5px;", view_genelists()),
    bsTooltip("view_genelists", "View filtered gene lists"),
    div(id="n_filter_names", style="display: inline-block;margin-right: 5px;", enter_genes()),
    bsTooltip("enter_genes", "Enter genes of interest"),
  )
})


####================= Buttons UI =====================####

customize_filters <- reactive({
  dropdown(inputId="customize_filters", align="right",
           
           tags$h3("Gene list filters"),
           
           fluidRow(
             column(3, 
                    uiOutput("n_presets"),
             ),
             column(9,
                    uiOutput("ui_n_gls_opt"),
             )
           ),
           
           actionButton("nic_applytorv", "Apply Filters", class = "btn-warning"),
           
           style = "material-circle", icon = icon("gear"),
           status = "default", width = calc_dropdown_width(length(rv$nx_n)+1, 250, 70, max_per_row=3),
           right=T, 
           animate = animateOptions(
             enter = "slideInRight",
             exit = "fadeOutRight", duration = 0.5
           ),
  )
})

view_genelists <- reactive({
  dropdown(inputId="view_genelists", align="right",
           
           tags$h3("Filtered gene lists"),
           # downloadButton("gls_dl", "Download"),
           uiOutput("n_gls_ui"),
           
           style = "material-circle", icon = icon("bars"),
           status = "default", width = calc_dropdown_width(length(rv$nx_n), 225, 70, max_per_row=3),
           right=T, 
           animate = animateOptions(
             enter = "slideInRight",
             exit = "fadeOutRight", duration = 0.5
           ),
  )
})


enter_genes <- reactive({
  dropdown(inputId="enter_genes", align="right",
           
           tags$h3("Enter genes"),
           
           
           textAreaInput("n_igl", 
                         "Enter genes of interest (separated by new line):",
                         placeholder="efk-1\nzip-2\ncep-1",
                         value="efk-1\nzip-2\ncep-1"
           ),
           actionButton("n_igl_update", "Apply"),
           bsTooltip("n_igl_update", "Only view selected genes"),
           
           actionButton("n_igl_reset", "Reset"),
           bsTooltip("n_igl_reset", "Reset entry and view all data"),
           
           br(),br(),
           uiOutput("n_igl_nm"),
           div(style="text-align:left;",
               HTML("<i>NOTE: If you are not seeing all the selected genes, 
                remove the filters in the \"Gene list filters\" dropdown.</i>")
           ),
           
           style = "material-circle", icon = icon("font"),
           status = "default", width = "250px",
           right=T, 
           animate = animateOptions(
             enter = "slideInRight",
             exit = "fadeOutRight", duration = 0.5
           ),
  )
})



####================= FILTERING DROPDOWN =====================####

# generates dynamic ui for selection

output$ui_n_gls_opt <- renderUI({
  req(nrow(rv$df_n)>0)
  
  rv$nic
  # append(rv$nic, rv$global_nic, 0) # add the global box to first item when output
})

# observeEvent(input$nic_applytoall, {
#   for (i in 1:length(rv$nx_n)){
#     updateNumericInput(session, paste0("nic_p_",i), value=input$nic_p)
#     updateNumericInput(session, paste0("nic_q_",i), value=input$nic_q)
#     updateNumericInput(session, paste0("nic_Stat_",i), value=input$nic_Stat)
#     updateRadioGroupButtons(session, paste0("nic_sign_",i), selected=input$nic_sign)
#     updateCheckboxInput(session, paste0("nic_apply_",i), value=input$nic_apply)
#     updateCheckboxInput(session, paste0("nic_na_",i), value=input$nic_na)
#   }
# })
# observeEvent(input$nic_remove_filters, {
#   for (i in 1:length(rv$nx_n)){
#     updateNumericInput(session, paste0("nic_p_",i), value=1)
#     updateNumericInput(session, paste0("nic_q_",i), value=1)
#     updateNumericInput(session, paste0("nic_Stat_",i), value=0)
#     updateRadioGroupButtons(session, paste0("nic_sign_",i), selected="All")
#     updateCheckboxInput(session, paste0("nic_apply_",i), value=T)
#     updateCheckboxInput(session, paste0("nic_na_",i), value=T)
#     updateNumericInput(session, "nic_p", value=1)
#     updateNumericInput(session, "nic_q", value=1)
#     updateNumericInput(session, "nic_Stat", value=0)
#     updateRadioGroupButtons(session, "nic_sign", selected="All")
#     updateCheckboxInput(session, "nic_apply", value=T)
#     updateCheckboxInput(session, "nic_na", value=T)
#   }
# })


output$n_presets <- renderUI({
  div(style="display: inline-block;vertical-align:top; width: 250px;",
      wellPanel(align = "left",
                HTML("<b>Filter presets:</b><br>Click to load one or multiple presets:<br>"),
                hr(),
                # dynamically render the list of presets as buttons
                tagList(lapply(1:length(filter_presets), function(i) {
                  name <- names(filter_presets)[[i]]
                  preset <- filter_presets[[i]]
                  actionButton(inputId = paste0("npreset_", preset[[1]]),
                               label = name
                  )
                })),
      )
      
  )
})

observe({
  req(nrow(rv$df_n)>0)
  
  rv$global_nic[[1]] <- div(style="display: inline-block;vertical-align:top; width: 250px;",
                            wellPanel( align = "left",
                                       
                                       
                                       
                                       #    tags$head(
                                       #      tags$style(HTML(paste0("
                                       #   #nic_p {height: 40px;}
                                       #   #nic_q {height: 40px;}
                                       #   #nic_Stat {height: 40px;}
                                       #   #nic_sign {height: 40px;}
                                       #   #nic_applytoall {height: 40px; line-height: 10px;}
                                       #   #nic_remove_filters {height: 40px;line-height: 10px;}
                                       # ")))
                                       #    ),
                                       #    fluidRow(
                                       #      column(6, align = "left", 
                                       #             numericInput("nic_p", 
                                       #                          "P filter:", value = 0.05, min = 0, max = 1, step=0.001, width="100px")),
                                       #      column(6, align = "left",
                                       #             numericInput("nic_Stat", 
                                       #                          "|Stat| filter:", value = 0, min = 0, max = 5, step=0.1, width="100px")),
                                       #    ),
                                       #    fluidRow(
                                       #      column(6, align = "left",
                                       #             numericInput("nic_q", 
                                       #                          "FDR filter:", value = 1, min = 0, max = 1, step=0.001, width="100px")),
                                       #      column(6, align = "left",
                                       #             radioGroupButtons("nic_sign", 
                                       #                               label = "Filter by sign:",
                                       #                               choices=c("All"="All", "+"="Positive", "-"="Negative"),
                                       #                               selected="All",size="s",direction = "horizontal"),
                                       #      ),
                                       #      
                                       #    ),
                                       # fluidRow(
                                       #   column(4,offset=1, align = "left",
                                       #          checkboxInput(
                                       #            inputId= "nic_apply",
                                       #            label = "Apply",
                                       #            value = T)),
                                       #   column(7, align = "left",
                                       #          checkboxInput(
                                       #            inputId= "nic_na",
                                       #            label = "Show NAs",
                                       #            value = T, ))
                                       # 
                                       # ),
                                       # actionButton("nic_applytoall", "Apply to all"),
                                       # actionButton("nic_remove_filters", "Remove filters"),
                                       
                                       style = "padding: 15px;")
                            
  )
  for (i in 1:length(rv$nx_n)){
    rv$nic[[i]] <- div(style="display: inline-block;vertical-align:top; width: 250px;",
                       tags$head(
                         tags$style(HTML(paste0("
                                      #nic_p_",i," {height: 40px;}
                                      #nic_q_",i," {height: 40px;}
                                      #nic_Stat_",i," {height: 40px;}
                                      #nic_sign_",i," {height: 40px;}
                                    ")))
                       ),
                       wellPanel( align = "left",
                                  rv$nx_n[[i]], 
                                  tags$hr(style="border-color: grey; margin:10px;"),
                                  fluidRow(
                                    column(6, align = "left", 
                                           numericInput(inputId = paste0("nic_p_",i), 
                                                        "P filter:", value = rv[[paste0("nic_p_",i)]], min = 0, max = 1, step=0.001, width="100px")),
                                    column(6, align = "left",
                                           numericInput(paste0("nic_Stat_",i), 
                                                        stat_replace1("|Stat| filter:", rv$nx_n[[i]]), 
                                                        value = rv[[paste0("nic_Stat_",i)]], min = 0, max = 5, step=0.1, width="100px")),
                                  ),
                                  fluidRow(
                                    column(6, align = "left",
                                           numericInput(inputId = paste0("nic_q_",i), 
                                                        "FDR filter:", value = rv[[paste0("nic_q_",i)]], min = 0, max = 1, step=0.001, width="100px")),
                                    column(6, align = "left",
                                           radioGroupButtons(inputId = paste0("nic_sign_",i), 
                                                             label = "Filter by sign:",
                                                             choices=c("All"="All", "+"="Positive", "-"="Negative"),
                                                             selected=rv[[paste0("nic_sign_",i)]],size="s",direction = "horizontal"),
                                    ),
                                    
                                  )
                                  # ,
                                  # fluidRow(
                                  #   column(4,offset=1, align = "left",
                                  #          checkboxInput(
                                  #            inputId= paste0("nic_apply_",i),
                                  #            label = "Apply",
                                  #            value = T)),
                                  #   column(7, align = "left",
                                  #          checkboxInput(
                                  #            inputId= paste0("nic_na_",i),
                                  #            label = "Show NAs",
                                  #            value = T, ))
                                  # 
                                  # )
                                  ,style = "padding: 15px;background:#e6f4fc;")
    )
  }
})


# observe the presets one by one
# observe these buttons and update filters when any is pressed
observeEvent(input[[paste0("npreset_",filter_presets[[1]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[1]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("nic_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("nic_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("nic_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("nic_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("npreset_",filter_presets[[2]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[2]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("nic_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("nic_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("nic_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("nic_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("npreset_",filter_presets[[3]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[3]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("nic_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("nic_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("nic_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("nic_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("npreset_",filter_presets[[4]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[4]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("nic_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("nic_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("nic_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("nic_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("npreset_",filter_presets[[5]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[5]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("nic_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("nic_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("nic_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("nic_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("npreset_",filter_presets[[6]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[6]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("nic_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("nic_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("nic_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("nic_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("npreset_",filter_presets[[7]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[7]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("nic_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("nic_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("nic_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("nic_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("npreset_",filter_presets[[8]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[8]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("nic_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("nic_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("nic_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("nic_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("npreset_",filter_presets[[9]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[9]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("nic_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("nic_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("nic_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("nic_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("npreset_",filter_presets[[10]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[10]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("nic_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("nic_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("nic_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("nic_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("npreset_",filter_presets[[11]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[11]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("nic_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("nic_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("nic_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("nic_sign_",x), selected=preset[[5]]) }
  }
})
observeEvent(input[[paste0("npreset_",filter_presets[[12]][[1]])]], {
  req(is.null(rv$nx_n)==F)
  preset=filter_presets[[12]]
  for (x in 1:length(rv$nx_n)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0("nic_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0("nic_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0("nic_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0("nic_sign_",x), selected=preset[[5]]) }
  }
})


# UPON CLICKING APPLY BUTTON in the DROPDOWN, update filter values into rv
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
    updateRadioGroupButtons(session, paste0("nic_sign_",i),
                            selected = rv[[paste0("nic_sign_",i)]]
    )
  }
})

####================= GENELISTS DROPDOWN =====================####

# format gene lists text
output$n_genelist <- renderText({
  req(length(n_ins_gls())>1)
  paste(n_ins_gls()[[1]], collapse="<br>")
})


# generates dynamic ui for showing genelists
observe({
  req(nrow(rv$df_n)>0)
  
  # prepare gene lists to render
  for (i in 1:length(rv$nx_n)){
    # apply textbox style to every text output
    rv$gls_text[[i]] <- div(style="
                white-space: pre-wrap;
                display: block;
                padding: 9.5px;
                margin: 5px 0 10px;
                font-size: 13px;
                line-height: 1.42857143;
                color: #333;
                word-break: break-all;
                word-wrap: break-word;
                background-color: #f5f5f5;
                border: 1px solid #ccc;
                border-radius: 4px;
                overflow-y: scroll;
                height: 200px;
                width: 200px;
                font-family: monospace;
                text-align: left;
                "
                            ,{
                              HTML(paste(n_ins_gls()[[i]], collapse="<br>"))
                            })
  }
  
  # render the gene lists in block format
  for (i in 1:length(rv$nx_n)){
    rv$gls_ui[[i]] <- div(style="display: inline-block;vertical-align:top; width: 225px;word-break: break-all;text-align:left;",
                          
                          # display name and list length
                          HTML(paste0("<strong>", rv$nx_n[[i]], "</strong>")), 
                          br(),
                          HTML(paste0("<i>", length(n_ins_gls()[[i]]), 
                                      " out of ", nrow(rv$df_n), " total</i>")),
                          br(),
                          # display gene list in box
                          rv$gls_text[[i]]
    )
  }
})

output$n_gls_ui <- renderUI({
  req(nrow(rv$df_n)>0)
  rv$gls_ui
})


####================= ENTER GENES DROPDOWN =====================####

# report genes that are not found
df_n_basic_nm <- reactive({
  if (nchar(rv$n_igl)>0){
    igl <- isolate(as.list(strsplit(toupper(rv$n_igl), '\\n+')))[[1]] # this is the gene list
    df <- rv$df_n
    df <- df[df$Name %in% igl,] # this is the  dataframe filtered by that gene list
    
    # if anything is found
    if (nrow(df)>0){
      notfound <- setdiff(igl, df$Name) # these are not found in df_n
      found <- setdiff(igl, notfound)
      notshown <- setdiff(found, n_ins_full()$Name) # these are found but not shown due to cutoffs
    } else { # if nothing is found
      notfound <- igl
      notshown <- vector()
    }
  }
  else{
    notfound=vector()
    notshown=vector()
  }
  out <- list("notfound"=notfound, "notshown"=notshown)
  out
  print(out)
})
output$n_igl_nm <- renderUI({
  req(length(df_n_basic_nm()$notfound)>0 | length(df_n_basic_nm()$notshown)>0)
  notfound <- df_n_basic_nm()$notfound
  notshown <- df_n_basic_nm()$notshown
  msg=vector()
  if (length(notfound)>0){
    nf <- paste(notfound, collapse=", ")
    msg1 <- paste0("<strong>Not found</strong> (",length(notfound),"): ", nf)
    msg <- c(msg, msg1)
  } 
  if (length(notshown)>0){
    ns <- paste(notshown, collapse=", ")
    msg2 <- paste0("<strong>Not shown because excluded by filters</strong> (",length(notshown),"): ", ns)
    msg <- c(msg, msg2)
  }
  print(msg)
  box(width=12,
      shiny::HTML(paste0(msg, sep="<br>")),
  )
})
observeEvent(input$n_igl_update,{
  rv$n_igl <- input$n_igl
})
observeEvent(input$n_igl_reset,{
  updateTextAreaInput(session, "n_igl", value="")
  rv$n_igl <- ""
})





