####---------------------- SIDEBAR ---------------------------####


#select data to show
output$select_df <- renderUI({
  selectInput(
    inputId = "show_df",
    label = "Select from uploaded datasets:",
    choices = rv$ll
  )
})


# selectively enables inputs according to state
observe({
  
  if (length(rv$ll) >= 1 & is.null(input$show_df)==F){
    shinyjs::enable("mode")
    shinyjs::enable("genelist_p1")
  } else {
    shinyjs::disable("mode")
    shinyjs::disable("genelist_p1")
  }
  
  if (length(rv$ll) >= 1 & is.null(input$show_df)==F & is.null(input$mode)==F){
    shinyjs::enable("submit_x")
  } else {
    shinyjs::disable("submit_x")
  }
  
  if (length(rv$ll) >= 1 & is.null(input$show_df)==F & is.null(input$mode)==F & is.null(input$genelist_p1)==F & nchar(input$genelist_p1)>0){
    shinyjs::enable("submit_genelist")
  } else {
    shinyjs::disable("submit_genelist")
  }
})


####---------------------- MAIN ---------------------------####

####---------------------- Visualization panels ui ---------------------------####
# must be put into reactives

# ---------------- single full dataset volcano
x_fs_volcano <- reactive({
  box(
    title = span( icon("chart-area"), "Volcano"), status = "primary", solidHeader = TRUE, width=12,
    plotlyOutput("p1_fs_volcano",
                 width = "100%",height = "400px"),
    
    div(style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          sliderTextInput("fs_volcano_p",
                          label = "Select P threshold:",
                          choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                          selected=0.05, grid=T, force_edges=T),
          sliderInput("fs_volcano_Stat",
                      label = "Select |Stat| threshold:",
                      min=0, max=5, step=0.1, value=0)
          
          ,
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE, width=300
        )
    ),
    div(style = "position: absolute; left: 4em; bottom: 1em",
        dropdown(
          downloadButton("x_vol_dl", "Download plot")
          ,
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE
        )
    )
  )
})
  

# ---------------- single full dataset table

x_fs_table <- reactive({
  box(
    title = span( icon("table"), "Table"), status = "primary", solidHeader = TRUE, width=12,
    dataTableOutput("single_tbl", width = "100%",height="100%"),
    
    div(style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          sliderTextInput("df_x_p",
                          label = "Select P cutoff:",
                          choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                          selected=0.05, grid=T, force_edges=T),
          sliderTextInput("df_x_q",
                          label = "Select P cutoff:",
                          choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                          selected=1, grid=T, force_edges=T),
          sliderInput("df_x_Stat", "Select |Stat| cutoff:", min = 0, max = 5,
                      value = 0, step = 0.25)
          ,
          size = "xs",
          icon = icon("cut", class = "opt"),
          up = TRUE, width=300
        )
    ),
    div(style = "position: absolute; left: 4em; bottom: 1em;",
        dropdown(
          downloadButton("downloaddf","Download table"),
          downloadButton("downloadrnk","Download RNK")
          ,
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE
        )
    ),
    
  )
})
  

# ---------------- single gene list volcano

x_gl_volcano <- reactive({
  box(
    title = span( icon("chart-area"), "Volcano"), status = "primary", solidHeader = TRUE, width=12,
    plotlyOutput("p1_gl_volcano",
                 width = "100%",height = "400px"),
    
    div(style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          sliderTextInput("gl_volcano_p",
                          label = "Select P threshold:",
                          choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                          selected=0.05, grid=T, force_edges=T),
          sliderInput("gl_volcano_Stat",
                      label = "Select |Stat| threshold:",
                      min=0, max=5, step=0.1, value=0)
          
          ,
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE, width=300
        )
    )
  )
})
  

# ---------------- single gene list bar

x_gl_bar <- reactive({
  box(
    title = span( icon("chart-area"), "Bar"), status = "primary", solidHeader = TRUE, width=12,
    uiOutput("gl_bar_fig"),
    div(style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          radioButtons(
            inputId = "p1_bar_data",
            label = "Data to plot in bar graph:",
            choices = rv$gl_cols, #can select any column except gene name
            selected = rv$gl_cols[[1]]
          ),
          radioButtons(
            inputId = "p1_bar_sig",
            label = "Color by significance:",
            choices = c("PValue","FDR"),
            selected = "PValue"
          )
          
          ,
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE, width=200
        )
    ),
    div(style = "position: absolute; left: 4em; bottom: 1em",
        dropdown(
          downloadButton("x_bar_dl", "Download plot")
          ,
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE
        )
    )
  )
})
  


# ---------------- single gene list table

x_gl_table <- reactive({
  box(
    title = span( icon("table"), "Table"), status = "primary", solidHeader = TRUE, width=12,
    dataTableOutput("single_gl_tbl", width = "100%",height="100%"),
    
    div(style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          sliderTextInput("df_gl_p",
                          label = "Select P cutoff:",
                          choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                          selected=1, grid=T, force_edges=T),
          sliderTextInput("df_gl_q",
                          label = "Select P cutoff:",
                          choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                          selected=1, grid=T, force_edges=T),
          sliderInput("df_gl_Stat", "Select |Stat| cutoff:", min = 0, max = 5,
                      value = 0, step = 0.25)
          ,
          size = "xs",
          icon = icon("cut", class = "opt"),
          up = TRUE, width=300
        )
    ),
    div(style = "position: absolute; left: 4em; bottom: 1em;",
        dropdown(
          downloadButton("downloadgldf","Download table"),
          downloadButton("downloadrnk","Download RNK")
          ,
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE
        )
    ),
    
  )
})





####---------------------- Assemble into dashboard ---------------------------####

output$x_floating_buttons <- renderUI({
  if (is.null(rv$mode)==T){
    div(style="margin-top:10px",
        
        actionBttn(
          inputId = "help_x_pre", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        ),
    )
  } else {
    div(style="margin-top:10px",
        
        actionBttn(
          inputId = "help_x_post", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        ),
    )
  }
})


output$single_panels <- renderUI({
  if(is.null(rv$mode)==T){
    div(
      box(
        title = span( icon("exclamation"), "Notification"), status = "warning", width=8,
        "No data selected."
      )
      
    )
  }
  
  else if (rv$mode == "All genes"){
    div(
      
      fluidRow(
        column(6,
               x_fs_volcano()
               
        ),
        column(6,
               box(
                 title = span( icon("chart-area"), "Bar"), status = "warning", solidHeader = TRUE, width=12,
                 
                 "Bar plot is only available for gene list mode."
               ),
               x_fs_table()
        )
        
        
      )
    )
  }
  else if (rv$mode == "List of genes"){
    div(
      fluidRow(
        column(6,
               x_gl_volcano()
        ),
        column(6,
               x_gl_bar(),
               
               x_gl_table()
        )
        
        
      )
    )
  }
})