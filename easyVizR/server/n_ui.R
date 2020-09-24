
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
    div(style = "position: absolute; left: 4em; bottom: 1em; width:300px;", id="n1_3d",
        dropdown(
          materialSwitch(
            inputId = "n_hm_ylabs", label = "Show y labels?", status="primary",
            value = FALSE
          )
          ,
          size = "xs",
          icon = icon("palette", class = "opt"),
          up = TRUE
        )
    ),
    div(style = "position: absolute; left: 7em; bottom: 1em", id="n1_3c",
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
          radioButtons(
            inputId = "nxy_colormode",
            label = "Represent significance by:",
            choices = c("None", "Two colors", "Color and size")
            ),
          uiOutput("nxy_colormode_options"),
          

          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE, width=230
        )
    ),
    div(style = "position: absolute; left: 4em; bottom: 1em",
        dropdown(
          
          numericInput(
            inputId = "nxy_sc_size",
            label = "Dot size:",
            value = 3, step=0.5, width="100px")
          ,
          size = "xs",
          icon = icon("palette", class = "opt"),
          up = TRUE, width=200
        )
    ),
    div(style = "position: absolute; left: 7em; bottom: 1em",
        dropdown(
          uiOutput("nxy_color_summary_panel"),
          
          size = "xs",
          icon = icon("eye-dropper", class = "opt"),
          up = TRUE, width=300
        )
    ),
    div(style = "position: absolute; left: 10em; bottom: 1em",
        dropdown(
          downloadButton("scatter_nxy_dl", "Download plot"),
          
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE
        )
    )
    
    
  )
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
          radioButtons(
            inputId = "nxyz_colormode",
            label = "Represent significance by:",
            choices = c("None", "Two colors"), 
            selected = "None"
          ),
          uiOutput("nxyz_colormode_options"),
          
          
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE, width=300
        )
    ),
    div(style = "position: absolute; left: 4em; bottom: 1em",
        dropdown(
          numericInput(
            inputId = "nxyz_sc_size",
            label = "Dot size:",
            value = 3, step=0.5, width="100px")
          ,
          size = "xs",
          icon = icon("palette", class = "opt"),
          up = TRUE, width=200
        )
    ),
    div(style = "position: absolute; left: 7em; bottom: 1em",
        dropdown(
          strong("Color Summary:"),
          dataTableOutput("n_3ds_prop_tbl"),br(),
          downloadButton("download_3ds_df", "Download color summary"),
          
          size = "xs",
          icon = icon("eye-dropper", class = "opt"),
          up = TRUE, width=300
        )
    ),
    div(style = "position: absolute; left: 10em; bottom: 1em",
        dropdown(
          downloadButton("n_3ds_dl", "Download plot"),
          
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

#======================================================================#
####                     single UI                                  ####
#======================================================================#

output$n_ui_single <- renderUI({
  req(rv$n_ui_showpanel == "Single")
  div(
    fluidRow(
      column(6,
             box(
               title = NULL, status = "primary", solidHeader = F, width=12,
               div(id="n_select_single",
                   selectInput(
                     inputId = "nx_selected",
                     label = "View dataset:",
                     choices = rv$nx_n,
                     selected = rv$nx_n[[1]]
                   )
              )
               
             ),
             uiOutput("nx_bar_panel"),
             
      ),
      
      column(6,
             box(
               title = span( icon("chart-area"), "Volcano"), status = "primary", solidHeader = F, width=12,
               plotlyOutput("nx_vol"),
               
               div(id="nx_vol_opt", style = "position: absolute; left: 1em; bottom: 1em; width:400px;",
                   dropdown(
                     radioButtons(
                       inputId = "nx_vol_plotmode",
                       HTML(paste0(
                         "Plotting mode:",
                         add_help("vol_plotmode_help", style="margin-left: 5px;"))
                       ),
                       choices = c("Focus", "Context")
                     ),
                     
                     bsTooltip("vol_plotmode_help", 
                               "Focus: only plot genes in intersection;<br>Context: plot all genes.", 
                               placement = "right"),
                     
                     
                     br(),
                     strong("Color threshold:"),
                     br(),
                     numericInput("nx_p", 
                                  "P <= :", value = 0.05, min = 0, max = 1, step=0.001, width="100px"),
                     numericInput("nx_Stat", 
                                  "|Stat| >= :", 
                                  value = 0, min = 0, max = 1, step=0.001, width="100px"),
                     
                     size = "xs",
                     icon = icon("gear", class = "opt"),
                     up = TRUE
                   )
               ),
               
               div(id="nx_vol_aes", style = "position: absolute; left: 4em; bottom: 1em; width:300px;",
                   dropdown(
                     selectInput("nx_vol_c1", "Primary color:",
                       choices = default_colors,
                       selected="red"
                       ),
                     selectInput("nx_vol_c2", "Secondary color:",
                                 choices = default_colors,
                                 selected="black"
                     ),
                     selectInput("nx_vol_c3", "Tertiary color:",
                                 choices = default_colors,
                                 selected="gray"
                     ),
                     
                     size = "xs",
                     icon = icon("palette", class = "opt"),
                     up = TRUE
                   )
               ),
               
               div(id="nx_vol_dl", style = "position: absolute; left: 7em; bottom: 1em",
                   dropdown(
                     downloadButton("nx_vol_dl", "Download plot"),
                     
                     size = "xs",
                     icon = icon("download", class = "opt"),
                     up = TRUE
                   )
               )
               ),
             
             
      ),
      
    ),
    
    
  )
})




#======================================================================#
####                     Network UI                                 ####
#======================================================================#

output$n_ui_network <- renderUI({
  req(rv$n_ui_showpanel == "Network")
  div(
    fluidRow(
      column(3,
             box(
               title = NULL, status = "primary", solidHeader = F, width=12,
               div(id="n_nw_select",
                   selectInput(
                     inputId = "nw_selected_n",
                     label = "Select dataset:",
                     choices = rv$nx_n,
                     selected = rv$nx_n[[1]]
                   ),
                   uiOutput("nw_set_le_ui"),
                   )
               
             ),
             
             ),
      column(9,
             uiOutput("vis_network_panel"),
             
             
             )
    )
    
    
  )
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
      # fluidRow(
      #   column(12,
      #          wellPanel(
      #            progress_box(id="infobox_1", prompt="To-dos:",
      #            msg=c("1. Adjust filters", "2. Select an intersection", "3. Try out visualizations"),
      #            condition=c(!is.null(input$nic_p), !is.null(rv$criteria), T),
      #            bttn_id="next_p1", bttn_text="Continue to next panel"
      #            )
      #          ))
      # ),
      fluidRow(
        div(style="height:3.5em",
            column(6,
                   div(id="n1_1",
                       radioGroupButtons("n_ui_showpanel",
                                         choices=c("Heatmap", "Scatter", "Single", "Network"),
                                         selected="Heatmap", status="primary",
                                         checkIcon = list(
                                           yes = tags$i(class = "fa fa-check-square", 
                                                        style = "color: white"),
                                           no = tags$i(class = "fa fa-square-o", 
                                                       style = "color: white"))
                       ),
                   )
                   
            ),
            column(6, align= "right",
                   div(id="n_filters_here"),
                   # uiOutput("n_filters")
                   
                   
                   
            )
            )
        
      ),
      
      # fluidRow(
      #   column(12,
      #          box(
      #            width = 12, status = "primary",solidHeader = F,
      #            title = span(icon("table"),"Filter summary"),
      #            uiOutput("filters_summary")
      #          ),
      #          
      #   ),
      # ),
      
      
      uiOutput("n_ui_basic"),
      uiOutput("n_ui_scatter"),
      uiOutput("n_ui_single"),
      uiOutput("n_ui_network"),
      
      #----------------- Intersect table--------------------
      
      
      fluidRow(
        column(12,
               box(
                 width = 12, status = "primary",solidHeader = F,
                 title = span(icon("table"),"Intersection of Interest"),
                 div(id= "vis_pg_bottom"),
               )
               
        )
      ),
      # uiOutput("ins_table_panel")
      
      
    )   
  }
  
})


