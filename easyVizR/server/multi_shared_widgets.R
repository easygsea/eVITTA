#======================================================================#
####                 UPDATE upon tab switching                      ####
#======================================================================#

# UPON ENTERING EACH TAB, update SHARED WIDGETS in that tab

observeEvent(input$tabs, {
  req(is.null(rv$nx_n)==F)
  # print(input$tabs)
  if (input$tabs == "tab_filters"){ # filter tab
    
    # initalize the filter inputs with saved values
    update_filters("f","nic", rv)
    
  } else if (input$tabs == "tab_ins"){ # intersection tab
    move_ui("ins_main_panels", "ins_main_panels_here", "afterEnd")
    move_ui("ins_table_panel", "ins_pg_bottom", "afterEnd")
    move_ui("n_filters", "ins_filters_here", "afterEnd")
    
  } else if (input$tabs == "tab3"){ # vis tab
    
    move_ui("ins_table_panel", "vis_pg_bottom", "afterEnd")
    move_ui("n_filters", "n_filters_here", "afterEnd")
    
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
    bsTooltip("customize_filters", "Apply Filters"),
    div(id="n_filter_gls", style="display: inline-block;margin-right: 5px;", view_genelists()),
    bsTooltip("view_genelists", "View filtered gene lists"),
    div(id="n_filter_names", style="display: inline-block;margin-right: 5px;", enter_genes()),
    bsTooltip("enter_genes", "Enter genes of interest"),
  )
})


####================= Buttons UI =====================####

customize_filters <- reactive({
  dropdown(inputId="customize_filters", align="right",
           
           tags$h3("Apply Filters"),
           
           fluidRow(
             column(3, 
                    uiOutput("n_presets"),
             ),
             column(9,
                    uiOutput("ui_n_gls_opt"),
             )
           ),
           
           actionButton("nic_applytorv", "Save Filters", class = "btn-warning"),
           
           uiOutput("n_highlights"),
           
           style = "material-circle", icon = icon("filter"),
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
           
           tags$h3("Saved gene lists"),
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
  placeholder <- paste0(head(rv$df_n,3)$Name, collapse="\n")
  
  dropdown(inputId="enter_genes", align="right",
           
           tags$h3("Enter genes"),
           
           
           textAreaInput("n_igl", 
                         "Enter genes of interest (separated by new line):",
                         placeholder=placeholder
           ),
           actionButton("n_igl_update", "View genes", class = "btn-warning"),
           bsTooltip("n_igl_update", "Only view entered genes of interest"),
           
           actionButton("n_igl_reset", "Reset"),
           bsTooltip("n_igl_reset", "Reset entry and view all data"),
           
           br(),br(),
           uiOutput("n_igl_nm"),
           
           style = "material-circle", icon = icon("font"),
           status = "default", width = "250px",
           right=T, 
           animate = animateOptions(
             enter = "slideInRight",
             exit = "fadeOutRight", duration = 0.5
           ),
  )
})



####================= FILTER DROPDOWN =====================####


# ========== assemble filter preset left panel ==============
output$n_presets <- renderUI({
  div(style="display: inline-block;vertical-align:top; width: 250px;",
      wellPanel(align = "left",
                HTML(paste0(
                  "<b>Filter preset shortcuts</b>:",
                  add_help("n_presets_help", style="margin-left: 5px;margin-bottom:0.8em;"))
                ),
                bsTooltip("n_presets_help", 
                          "Click on these buttons to apply filter presets to all datasets (effects are stackable).<br>Click on <b>No Filter</b> to remove all filters.", 
                          placement = "top"),
                
                # dynamically render the list of presets as buttons
                tagList(lapply(1:length(filter_presets), function(i) {
                  name <- names(filter_presets)[[i]]
                  preset <- filter_presets[[i]]
                  actionButton(inputId = paste0("npreset_", preset[[1]]),
                               label = name, 
                               icon(preset[[7]]),
                               style=paste0("color:",preset[[8]],";background-color:",preset[[9]],";")
                  )
                })),
                # dynamically render the preset tooltips
                tagList(lapply(1:length(filter_presets), function(i) {
                  preset <- filter_presets[[i]]
                  bsTooltip(id=paste0("npreset_", preset[[1]]), 
                            title=preset[[6]], 
                            placement = "top")
                })),
                
                # reset all changes button
                actionButton(inputId = "n_reset",
                             label = "Reset changes", 
                             icon("undo-alt"),
                             style=paste0("color:#f4f4f4; background-color:#444;")
                ),
                bsTooltip(id="n_reset", 
                          title="Reset all unsaved changes", 
                          placement = "top")
                
      
      )
      
  )
})


# ========== preset buttons ==============

# observe these buttons and update filters when any is pressed
#----------------------------------------------------------------
observeEvent(input[[paste0("npreset_",filter_presets[[1]][[1]])]], {
  apply_presets_to_filterinputs(1, "nic", presets=filter_presets)
})
observeEvent(input[[paste0("npreset_",filter_presets[[2]][[1]])]], {
  apply_presets_to_filterinputs(2, "nic", presets=filter_presets)
})
observeEvent(input[[paste0("npreset_",filter_presets[[3]][[1]])]], {
  apply_presets_to_filterinputs(3, "nic", presets=filter_presets)
})
observeEvent(input[[paste0("npreset_",filter_presets[[4]][[1]])]], {
  apply_presets_to_filterinputs(4, "nic", presets=filter_presets)
})
observeEvent(input[[paste0("npreset_",filter_presets[[5]][[1]])]], {
  apply_presets_to_filterinputs(5, "nic", presets=filter_presets)
})
observeEvent(input[[paste0("npreset_",filter_presets[[6]][[1]])]], {
  apply_presets_to_filterinputs(6, "nic", presets=filter_presets)
})
observeEvent(input[[paste0("npreset_",filter_presets[[7]][[1]])]], {
  apply_presets_to_filterinputs(7, "nic", presets=filter_presets)
})
observeEvent(input[[paste0("npreset_",filter_presets[[8]][[1]])]], {
  apply_presets_to_filterinputs(8, "nic", presets=filter_presets)
})
observeEvent(input[[paste0("npreset_",filter_presets[[9]][[1]])]], {
  apply_presets_to_filterinputs(9, "nic", presets=filter_presets)
})
observeEvent(input[[paste0("npreset_",filter_presets[[10]][[1]])]], {
  apply_presets_to_filterinputs(10, "nic", presets=filter_presets)
})
observeEvent(input[[paste0("npreset_",filter_presets[[11]][[1]])]], {
  apply_presets_to_filterinputs(11, "nic", presets=filter_presets)
})
observeEvent(input[[paste0("npreset_",filter_presets[[12]][[1]])]], {
  apply_presets_to_filterinputs(12, "nic", presets=filter_presets)
})
observeEvent(input[[paste0("npreset_",filter_presets[[13]][[1]])]], {
  apply_presets_to_filterinputs(13, "nic", presets=filter_presets)
})

# observe the reset changes button
#-------------------------------------
observeEvent(input$n_reset, {
  update_filters("nic", "nic", rv)
})



# ========== unsaved highlights ==============

# observe the changes made to the filters, and highlight unsaved changes (i.e. different from rv)
#-----------------------------------------
observe({
  req_filter_ns("nic", input)
  req_filter_ns("nic", rv)
  
  rv$n_css_highlights <- observe_filter_highlights("nic", input, "nic", rv)
})

output$n_highlights <- renderUI({
  rv$n_css_highlights
})


# ========== save filters ==============

# UPON CLICKING APPLY BUTTON in the DROPDOWN, update filter values into rv
#-------------------------------------
observeEvent(input$nic_applytorv, {
  req(is.null(rv$nx_n)==F)
  
  req_filter_ns("nic",input)
  update_filters_rv("nic", "nic", input)
  update_filters("nic", "nic", rv)
  
})


# ========== assemble filter ui ==============
observe({
  req(nrow(rv$df_n)>0)
  
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
                                                        "P <=:", value = rv[[paste0("nic_p_",i)]], min = 0, max = 1, step=0.001, width="100px")),
                                    column(6, align = "left",
                                           numericInput(paste0("nic_Stat_",i), 
                                                        stat_replace1(
                                                          HTML(paste0(
                                                            "<b>|Stat| >=</b>:",
                                                            add_help(paste0("n_stat_help",i), style="margin-left: 5px;"))
                                                          )
                                                          , rv$nx_n[[i]]),
                                                        value = rv[[paste0("nic_Stat_",i)]], min = 0, max = 5, step=0.1, width="100px")),
                                    bsTooltip(paste0("n_stat_help",i), 
                                              paste0(stat_replace1("Stat type: <b>", rv$nx_n[[i]]),rv$tt[[rv$nx_i[[i]]]],"</b>"), 
                                              placement = "top"),
                                  ),
                                  fluidRow(
                                    column(6, align = "left",
                                           numericInput(inputId = paste0("nic_q_",i), 
                                                        "FDR <=:", value = rv[[paste0("nic_q_",i)]], min = 0, max = 1, step=0.001, width="100px")),
                                    column(6, align = "left",
                                           radioGroupButtons(inputId = paste0("nic_sign_",i), 
                                                             label = "Direction:",
                                                             choices=c("All"="All", "+"="Positive", "-"="Negative"),
                                                             selected=rv[[paste0("nic_sign_",i)]],size="s",direction = "horizontal"),
                                    ),
                                    
                                  )
                                  ,style = "padding: 15px;background:#e6f4fc;")
    )
  }
  saveRDS(rv$nic, file = "rvs/nic.rds")
})


# generates dynamic ui for selection
output$ui_n_gls_opt <- renderUI({
  req(nrow(rv$df_n)>0)
  
  rv$nic
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
    #saveRDS(rv$gls_ui, file = "rvs/gls_ui.rds")
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
  # print(out)
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
    msg2 <- HTML(paste0(
      paste0("<strong>Not shown because excluded by filters</strong> (",length(notshown),"): ", ns),
      add_help("n_gls_notshown", style="margin-left: 5px;"))
    )
      
    msg <- c(msg, msg2)
  }
  # print(msg)
  box(width=12,
      shiny::HTML(paste0(msg, sep="<br>")),
      bsTooltip("n_gls_notshown",
                "To view these, remove all filters in the \"Apply Filters\" dropdown.", 
                placement = "top"),
  )
})
observeEvent(input$n_igl_update,{
  rv$n_igl <- input$n_igl
})
observeEvent(input$n_igl_reset,{
  updateTextAreaInput(session, "n_igl", value="")
  rv$n_igl <- ""
})





