####---------------------- Sidebar ---------------------------####

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





#======================================================================#
####                        Heatmap UI                              ####
#======================================================================#

output$n_ui_basic <- renderUI({
  req(rv$n_ui_showpanel == "Heatmap")
  div(
    heatmap_panel(),
    # hm_table_panel()
    
  )
  
})


heatmap_panel <- reactive({
  #----------------- heatmap --------------------
  box(
    title = span( icon("chart-area"), "Heatmap"), status = "primary", solidHeader = F, width=8,
    
    div(id="n1_3",
        uiOutput("n_heatmap")
    ), 
    
    div(style = "position: absolute; left: 1em; bottom: 1em",id="n1_3b",
        dropdown(
          
          uiOutput("select_sortby_p2"),
          uiOutput("n_to_plot"),
          
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE, width=300
        )
    ),
    div(style = "position: absolute; left: 4em; bottom: 1em", id="n1_3c",
        dropdown(
          downloadButton("n_hm_dl", "Download plot")
          ,
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE
        )
    )
    
    
  )
})

hm_table_panel <- reactive({
  #----------------- table --------------------
  box(
    title = span( icon("table"), "Table"), status = "primary", solidHeader = F, width=12, height="610px",
    
    div(id="n1_4", 
        dataTableOutput("df_n_tbl")
    ),
    
    div(style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          downloadButton("download_n_df", "Download data")
          ,
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE
        )
    )
    
  )
})

#======================================================================#
####                     intersection UI                            ####
#======================================================================#

output$n_ui_intersect <- renderUI({
  req(rv$n_ui_showpanel == "Intersection")
  
  
  div(
    
    #----------------- venn --------------------
    
    conditionalPanel("output.n_venn_status == 'ok'",
                     ins_venn_panel(),
                     
    ),
    
    conditionalPanel("output.n_venn_status == 'no'",
                     uiOutput("n_venn_placeholder")
    ),
    
    
    
    #----------------- upset --------------------
    conditionalPanel("output.n_venn_status == 'ok'",
                     ins_upset_panel(),
    ),
    conditionalPanel("output.n_venn_status == 'no'",
                     uiOutput("n_upset_placeholder")
    ),
    
    
    
  )
})

#----------------- venn --------------------
ins_venn_panel <- reactive({
  box(title = span( icon("chart-area"), "Venn Diagram"), status = "primary", solidHeader = F, width=6,
      
      tabBox(width=12,
             tabPanel("Basic",
                      plotOutput("df_n_npvenn", width="100%")
             ),
             tabPanel("Area proportional",
                      plotOutput("df_n_venn", width="100%")
             )
             
      ),
      
      
      
      div(style = "position: absolute; left: 1em; bottom: 1em", 
          dropdown(
            checkboxGroupInput(
              inputId = "n_venn_label",
              label= "Show in label:",
              choices = c("Counts"="counts", "Percent"="percent"),
              selected="counts",
              inline=T, width="250px"
            )
            ,
            size = "xs",
            icon = icon("gear", class = "opt"),
            up = TRUE
          )
      ),
      div(style = "position: absolute; left: 4em; bottom: 1em", 
          dropdown(
            downloadButton("n_npvenn_dl", "Download basic"),
            downloadButton("n_venn_dl", "Download area-proportional"),
            
            size = "xs",
            icon = icon("download", class = "opt"),
            up = TRUE, width=300
          )
          
      )
      
      
  )
})

#----------------- upset --------------------
ins_upset_panel <- reactive({
  
  box(
    title = span( icon("chart-area"), "UpSet Plot"), status = "primary", solidHeader = F, width=6,
    
    plotOutput("df_n_upset", width = "100%"),
    
    div(style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          selectInput(
            inputId = "n_upset_sortby",
            label = "Order by:",
            choices = c("Frequency"="freq", "Degree"="degree"),
            selected = "freq"),
          materialSwitch(
            inputId = "n_upset_showempty", label = "Show empty intersections?", status="primary",
            value = FALSE
          ),
          
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE, width=300
        )
        
    ),
    div(style = "position: absolute; left: 4em; bottom: 1em",
        dropdown(
          downloadButton("n_upset_dl", "Download plot"),
          
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE, width=300
        )
        
    )
    
  )
})

output$n_venn_placeholder <- renderUI({
  box(
    title = span( icon("chart-area"), "Venn Diagram"), status = "warning", solidHeader = F, width=6,
    paste0("Venn diagram is only available for 5 or less datasets. You have selected ", length(rv$nx_i)," datasets.")
  )
})
output$n_upset_placeholder <- renderUI({
  box(
    title = span( icon("chart-area"), "UpSet Plot"), status = "warning", solidHeader = F, width=6,
    paste0("UpSet plot is only available for 5 or less datasets. You have selected ", length(rv$nx_i)," datasets.")
  )
})

#----------------- table --------------------

# summarizes verbally the filters used to trim gene lists
output$filters_summary <- renderUI({
  
  desc <-list()
  
  for (i in 1:length(rv$nx_n)){
    req(rv[[paste0("nic_p_",i)]])
    req(rv[[paste0("nic_q_",i)]])
    req(rv[[paste0("nic_Stat_",i)]])
    req(rv[[paste0("nic_sign_",i)]])
    
    name <- rv$nx_n[[i]]
    cur_p <- rv[[paste0("nic_p_",i)]]
    cur_q <- rv[[paste0("nic_q_",i)]]
    cur_Stat <- rv[[paste0("nic_Stat_",i)]]
    if (rv[[paste0("nic_sign_",i)]]=="All"){
      cur_sign = "Positive and Negative"
    } else {
      cur_sign <- rv[[paste0("nic_sign_",i)]]
    }
    
    nterms <- length(n_ins_gls()[[i]]) # number of genes in genelist
    
    adddesc <- paste(name, ": ",
                     nterms, " ",
                     cur_sign, " entries with ",
                     "p <= ",cur_p, ", ", 
                     "FDR <= ", cur_q, ", ", 
                     "|Stat| >= ", cur_Stat,
                     sep="")
    desc <- c(desc, adddesc)
  }
  text <- paste(desc, collapse="<br>")
  
  
  HTML(text)
})

# summarizes verbally what is in the selected intersection
output$intersection_summary <- renderUI({
  req(is.null(rv$ins_criteria)==F)
  req(length(rv$ins_criteria)>0)
  
  desc <-list()
  criteria <- rv$ins_criteria
  
  for (i in 1:length(rv$nx_n)){
    req(is.null(rv[[paste0("nic_p_",i)]])==F)
    req(is.null(rv[[paste0("nic_q_",i)]])==F)
    req(is.null(rv[[paste0("nic_Stat_",i)]])==F)
    req(is.null(rv[[paste0("nic_sign_",i)]])==F)
    
    name <- rv$nx_n[[i]]
    cur_p <- rv[[paste0("nic_p_",i)]]
    cur_q <- rv[[paste0("nic_q_",i)]]
    
    cur_Stat <- rv[[paste0("nic_Stat_",i)]]
    req(is.null(criteria[[name]])==F)
    if (is.na(criteria[[name]])==T){
      stat_text=""
    } else if (is.na(criteria[[name]])==F){
      if (rv[[paste0("nic_sign_",i)]]=="All"){
        stat_text <- paste0("|Stat| >= ", cur_Stat)
      } else if (rv[[paste0("nic_sign_",i)]]=="Positive"){
        stat_text <- paste0("Stat >= ", cur_Stat)
      } else if (rv[[paste0("nic_sign_",i)]]=="Negative") {
        stat_text <- paste0("Stat <=  ", cur_Stat)
      }
    } else if (criteria[[name]]==F){
      if (rv[[paste0("nic_sign_",i)]]=="All"){
        stat_text <- paste0("|Stat| < ", cur_Stat)
      } else if (rv[[paste0("nic_sign_",i)]]=="Positive"){
        stat_text <- paste0("Stat < ", cur_Stat)
      } else if (rv[[paste0("nic_sign_",i)]]=="Negative") {
        stat_text <- paste0("Stat >  ", cur_Stat)
      }
    }
    
    
    if (is.na(criteria[[name]])==F){
      if (criteria[[name]]==T){
        adddesc <- paste(
          "p <= ",cur_p, " AND ", 
          "FDR <= ", cur_q, " AND ", 
          stat_text,
          " in ", name,
          sep="")
      } else if (criteria[[name]]==F){
        adddesc <- paste(
          "p > ",cur_p, " OR ", 
          "FDR > ", cur_q, " OR ", 
          stat_text, 
          " in ", name,
          sep="")
      } 
      # only append a description when it's not on ignore
      desc <- c(desc, adddesc)
    }
    

    
  }
  text <- paste(desc, collapse="; <br>")
  
  
  HTML(text)
})


ins_table_panel <- reactive({
  
  box(
    width = 12, status = "primary",solidHeader = F, height=750,
    title = span(icon("table"),"Select and view intersection"),
    
    div(id="n2_4",
        
        wellPanel(
          
          "Combine options below to view specific intersections: ",br(),br(),
          uiOutput("ui_intersections"),
          uiOutput("intersection_summary")
        ),
    ),
    
    
    
    dataTableOutput("n_ins_tbl"),
    
    
    div(style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          radioGroupButtons(
            inputId = "n_ins_view",
            label = "Choose view:",
            choices = c("Full", "Minimized", "T/F Matrix"),
            selected= "Full", direction="vertical"
          ),
          
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE, width=300
        )
        
    ),
    
    div(style = "position: absolute; left: 4em; bottom: 1em", id="n2_4b",
        dropdown(
          column(8,
                 column(2, textInput("n_ins_wc_sep", "Separator:", value="_")),
                 column(10, textInput("n_ins_Wc_ignore", "Ignore strings: (separated by spaces)", value="and or of GO KEGG WP")),
                 
                 plotOutput("n_ins_wc"),
          ),
          column(4,
                 dataTableOutput("n_ins_wc_df"),
          ),
          
          size = "xs",
          icon = icon("cloud", class = "opt", lib="glyphicon"),
          up = TRUE, width=800
        )
        
    ),
    div(style = "position: absolute; left: 7em; bottom: 1em",
        dropdown(
          downloadButton("download_ins_df", "Download current table"),
          downloadButton("download_ins_gl", "Download gene list"),
          
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE, width=300
        )
        
    )
    
    
  )
})


#======================================================================#
####                     scatter UI                                 ####
#======================================================================#


output$n_ui_scatter <- renderUI({
  req(rv$n_ui_showpanel == "Scatter")
  div(
    fluidRow(
      
      column(4,
             box(
               title = NULL, status = "primary", solidHeader = F, width=12,
               div(id="n3_3",
                   selectInput(
                     inputId = "nxy_selected_x",
                     label = "Selected x:",
                     choices = rv$nx_n,
                     selected = rv$nx_n[[1]]
                   ),
                   selectInput(
                     inputId = "nxy_selected_y",
                     label = "Selected y:",
                     choices = rv$nx_n,
                     selected = rv$nx_n[[2]]
                   ),
                   selectInput(
                     inputId = "nxy_selected_z",
                     label = "Selected z (optional):",
                     choices = c("None", rv$nx_n),
                     selected = "None"
                   ),
               )
               
               
             ),
             uiOutput("nxy_sc_cor_panel")
             
      ),
      
      column(8,
             uiOutput("nxy_3ds_panel"),
             uiOutput("nxy_sc_panel")
             
             
      ),
      
    ),
    
    # fluidRow(
    #   column(12,
    #          sc_table_panel()
    #          
    #   )
    #   
    # )
    
  )
})
output$nxy_sc_panel <- renderUI({
  req(rv$nxy_selected_z =="None")
  
  box(
    title = span( icon("chart-area"), "Scatter"), status = "primary", solidHeader = F, width=12,
    
    plotlyOutput("df_nxy_scatter",
                 width = "100%",height = "600px")
    ,
    div(style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          
          radioGroupButtons("n_sc_logic",
                            label = "Cutoff mode:",
                            choices=c("OR" ="Either", "AND" = "Both"),
                            selected="Both",size="s")
          , 
          uiOutput("nxy_logic_caption"),
          size = "xs",
          icon = icon("cut", class = "opt"),
          up = TRUE, width=200
        )
    ),
    div(style = "position: absolute; left: 4em; bottom: 1em",
        dropdown(
          uiOutput("nxy_colormode"),
          uiOutput("nxy_sig"),
          uiOutput("nxy_thresh"),
          numericInput(
            inputId = "nxy_sc_size",
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
          downloadButton("scatter_nxy_dl", "Download plot")
          ,
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE
        )
    )
    
    
  )
})

output$nxy_logic_caption <- renderUI({
  if (rv$n_sc_logic == "Both"){
    HTML("<strong>AND</strong>: shows entries that satisfy the filters in <strong>ALL</strong> of the selected datasets.")
  } else if (rv$n_sc_logic == "Either"){
    HTML("<strong>OR</strong>: shows entries that satisfy the filters in <strong>ANY</strong> of the selected datasets.")
  }
})
output$nxyz_logic_caption <- renderUI({
  if (rv$n_sc_logic == "Both"){
    HTML("<strong>AND</strong>: shows entries that satisfy the filters in <strong>ALL</strong> of the selected datasets.")
  } else if (rv$n_sc_logic == "Either"){
    HTML("<strong>OR</strong>: shows entries that satisfy the filters in <strong>ANY</strong> of the selected datasets.")
  }
})

output$nxy_sc_cor_panel <- renderUI({
  req(rv$nxy_selected_z =="None")
  
  box(
    title = span( icon("calculator"), "Correlation"), status = "primary", solidHeader = F, width=12,
    
    uiOutput("nxy_corline"),br(),
    verbatimTextOutput("nxy_cor_summary")
    
  )
})

output$nxy_3ds_panel <- renderUI({
  req(is.null(rv$nxy_selected_z)==F)
  req(rv$nxy_selected_z !="None")
  
  box(
    title = span( icon("chart-area"), "3D Scatter"), status = "primary", solidHeader = F, width=12,
    plotlyOutput("df_n_3ds",
                 width = "100%",height = "600px"),
    
    div(style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          
          radioGroupButtons("nxyz_sc_logic",
                            label = "Cutoff mode:",
                            choices=c("OR" ="Either", "AND" = "Both"),
                            selected="Both",size="s")
          , 
          uiOutput("nxyz_logic_caption"),
          size = "xs",
          icon = icon("cut", class = "opt"),
          up = TRUE, width=200
        )
    ),
    div(style = "position: absolute; left: 4em; bottom: 1em",
        dropdown(
          "Color threshold options:",
          sliderTextInput("n_3ds_p",
                          label = "Select P threshold:",
                          choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                          selected=0.05, grid=T, force_edges=T),
          sliderTextInput("n_3ds_q",
                          label = "Select FDR threshold:",
                          choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                          selected=1, grid=T, force_edges=T),
          sliderTextInput("n_3ds_Stat",
                          label = "Select |Stat| threshold:",
                          choices= c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5),
                          selected=0, grid=T, force_edges=T),
          "Note: This will be applied to all columns.",
          
          
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE, width=300
        )
    ),
    div(style = "position: absolute; left: 7em; bottom: 1em",
        dropdown(
          strong("Summary:"),
          dataTableOutput("n_3ds_prop_tbl"),
          
          
          size = "xs",
          icon = icon("table", class = "opt"),
          up = TRUE, width=300
        )
    ),
    div(style = "position: absolute; left: 10em; bottom: 1em",
        dropdown(
          downloadButton("n_3ds_dl", "Download plot"),
          downloadButton("download_3ds_df", "Download summary")
          ,
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE
        )
    )
    
    
    
  )
})


sc_table_panel <- reactive({
  
  box(
    title = span( icon("table"), "Table"), status = "primary", solidHeader = F, width=12,
    
    div(id="n3_4",
        dataTableOutput("df_nxy_tbl", width = "100%",height="100%") 
    ),
    
    div(style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          downloadButton("download_nxy_df",
                         label = "Download Table")
          ,
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE
        )
    )
    
    
  )
})


####================= MULTIPLE FILTERING MENU =====================####

# generates dynamic ui for selection

output$ui_n_gls_opt <- renderUI({
  req(nrow(rv$df_n)>0)
  
  append(rv$nic, rv$global_nic, 0) # add the global box to first item when output
})

observeEvent(input$nic_applytoall, {
  for (i in 1:length(rv$nx_n)){
    updateNumericInput(session, paste0("nic_p_",i), value=input$nic_p)
    updateNumericInput(session, paste0("nic_q_",i), value=input$nic_q)
    updateNumericInput(session, paste0("nic_Stat_",i), value=input$nic_Stat)
    updateRadioGroupButtons(session, paste0("nic_sign_",i), selected=input$nic_sign)
    updateCheckboxInput(session, paste0("nic_apply_",i), value=input$nic_apply)
    updateCheckboxInput(session, paste0("nic_na_",i), value=input$nic_na)
  }
})
observeEvent(input$nic_resetall, {
  for (i in 1:length(rv$nx_n)){
    updateNumericInput(session, paste0("nic_p_",i), value=0.05)
    updateNumericInput(session, paste0("nic_q_",i), value=1)
    updateNumericInput(session, paste0("nic_Stat_",i), value=0)
    updateRadioGroupButtons(session, paste0("nic_sign_",i), selected="All")
    updateCheckboxInput(session, paste0("nic_apply_",i), value=T)
    updateCheckboxInput(session, paste0("nic_na_",i), value=T)
    updateNumericInput(session, "nic_p", value=0.05)
    updateNumericInput(session, "nic_q", value=1)
    updateNumericInput(session, "nic_Stat", value=0)
    updateRadioGroupButtons(session, "nic_sign", selected="All")
    updateCheckboxInput(session, "nic_apply", value=T)
    updateCheckboxInput(session, "nic_na", value=T)
  }
})

observe({
  req(nrow(rv$df_n)>0)
  
  rv$global_nic[[1]] <- div(style="display: inline-block;vertical-align:top; width: 250px;",
                            wellPanel( align = "left",
                                       
                                       tags$head(
                                         tags$style(HTML(paste0("
                                      #nic_p {height: 40px;}
                                      #nic_q {height: 40px;}
                                      #nic_Stat {height: 40px;}
                                      #nic_sign {height: 40px;}
                                      #nic_applytoall {height: 40px; line-height: 10px;}
                                      #nic_resetall {height: 40px;line-height: 10px;}
                                    ")))
                                       ),
                                       fluidRow(
                                         column(6, align = "left", 
                                                numericInput("nic_p", 
                                                             "P filter:", value = 0.05, min = 0, max = 1, step=0.001, width="100px")),
                                         column(6, align = "left",
                                                numericInput("nic_Stat", 
                                                             "|Stat| filter:", value = 0, min = 0, max = 5, step=0.1, width="100px")),
                                       ),
                                       fluidRow(
                                         column(6, align = "left",
                                                numericInput("nic_q", 
                                                             "FDR filter:", value = 1, min = 0, max = 1, step=0.001, width="100px")),
                                         column(6, align = "left",
                                                radioGroupButtons("nic_sign", 
                                                                  label = "Filter by sign:",
                                                                  choices=c("All"="All", "+"="Positive", "-"="Negative"),
                                                                  selected="All",size="s",direction = "horizontal"),
                                         ),
                                         
                                       ),
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
                                       actionButton("nic_applytoall", "Apply to all"),
                                       actionButton("nic_resetall", "Reset all"),
                                       
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
                                                        "P filter:", value = 0.05, min = 0, max = 1, step=0.001, width="100px")),
                                    column(6, align = "left",
                                           numericInput(paste0("nic_Stat_",i), 
                                                        "|Stat| filter:", value = 0, min = 0, max = 5, step=0.1, width="100px")),
                                  ),
                                  fluidRow(
                                    column(6, align = "left",
                                           numericInput(inputId = paste0("nic_q_",i), 
                                                        "FDR filter:", value = 1, min = 0, max = 1, step=0.001, width="100px")),
                                    column(6, align = "left",
                                           radioGroupButtons(inputId = paste0("nic_sign_",i), 
                                                             label = "Filter by sign:",
                                                             choices=c("All"="All", "+"="Positive", "-"="Negative"),
                                                             selected="All",size="s",direction = "horizontal"),
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

####================= filter button UI =====================####

customize_filters <- reactive({
  dropdown(align="right",
           
           tags$h3("Customize Filters"),
           
           uiOutput("ui_n_gls_opt"),
           
           style = "material-circle", icon = icon("gear"),
           status = "default", width = "800px",
           right=T, 
           animate = animateOptions(
             enter = "slideInRight",
             exit = "fadeOutRight", duration = 0.5
           ),
  )
})


output$n_floating_buttons <- renderUI({
  
  if (is.null(rv$df_n)==T){
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_n_pre", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        ),
    )
  } else {
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_n_post", label=NULL,
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        ),
    )
    
    
  }
  
  
})


#======================================================================#
####                     ASSEMBLE INTO MAIN BODY UI                 ####
#======================================================================#

output$n_panels <- renderUI({
  if(is.null(rv$df_n)==T){
    box(
      title = span( icon("exclamation"), "Notification"), status = "warning", width=8,
      "No data selected."
    )
  }
  else{
    div(
      fluidRow(
        
        column(6,
               div(id="n1_1",
                   radioGroupButtons("n_ui_showpanel",
                                     choices=c("Intersection", "Heatmap", "Scatter"),
                                     selected="Intersection", status="primary",
                                     checkIcon = list(
                                       yes = tags$i(class = "fa fa-check-square", 
                                                    style = "color: white"),
                                       no = tags$i(class = "fa fa-square-o", 
                                                   style = "color: white"))
                   ),
               )
               
        ),
        column(6, align= "right",
               
               div(id="n1_2",
                   
                   customize_filters(),
                   
               )
               
               
        )
      ),
      
      fluidRow(
        column(12,
               box(
                 width = 12, status = "primary",solidHeader = F,
                 title = span(icon("table"),"Filter summary"),
                 uiOutput("filters_summary")
               ),
               
        ),
      ),
      
      uiOutput("n_ui_intersect"),
      uiOutput("n_ui_basic"),
      uiOutput("n_ui_scatter"),
      #----------------- Intersect table--------------------
      
      ins_table_panel()
      
      
    )   
  }
  
})
