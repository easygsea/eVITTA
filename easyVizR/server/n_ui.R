

#-------------- scatter plot options -------------------#

# ------------- display excluded data points (plot mode)
plotmode_label <- "Show excluded datapoints in the background?"
plotmode_explanation <- "Whether or not to plot excluded datapoints in the background."


# ------------- which data to display (logic)
dflogic_explanation <- "Determine which data to show in this plot."
# text for 2 way scatter:
dflogic_choices <- c("Current intersection" ="Ins", 
                     "Both X and Y" = "Both", 
                     "Either X or Y" ="Either",
                     "Common Intersection" ="All_Both",
                     "Any Intersection" ="All_Either"
                     )
dflogic_explanation_1 <- "Show currently selected intersection"
dflogic_explanation_2 <- "Show those fulfilling filters in BOTH x and y datasets"
dflogic_explanation_3 <- "Show those fulfilling filters in EITHER x or y dataset"
dflogic_explanation_4 <- "Show those fulfilling filters in ALL datasets (= the intersection shared between ALL Venn circles)"
dflogic_explanation_5 <- "Show those fulfilling filters in ANY dataset (= contained in ANY one of the Venn circles)"

# text for 3d scatter:
dflogic_xyz_choices <- c("Current intersection" ="Ins", 
                     "X AND Y AND Z" = "Both", 
                     "X OR Y OR Z" ="Either",
                     "Common Intersection" ="All_Both",
                     "Any Intersection" ="All_Either"
)
dflogic_xyz_explanation_1 <- "Show currently selected intersection"
dflogic_xyz_explanation_2 <- "Show those fulfilling filters in all 3 datasets X, Y and Z"
dflogic_xyz_explanation_3 <- "Show those fulfilling filters in any of the 3 datasets X, Y or Z"
dflogic_xyz_explanation_4 <- "Show those fulfilling filters in ALL datasets (= the intersection shared between ALL Venn circles)"
dflogic_xyz_explanation_5 <- "Show those fulfilling filters in ANY dataset (= contained in ANY one of the Venn circles)"

# ------------- conditional coloring
# for xy scatter
sc_coloring_choices <- c("None"="None", 
                         "Discrete colors"="Two colors", 
                         "Color and size"="Color and size")
# for 3d scatter
sc_xyz_coloring_choices <- c("None"="None", 
                             "Discrete colors"="Two colors")
sc_coloring_explanation_1 <- "Color all points black"
sc_coloring_explanation_2 <- "Highlights datapoints fulfilling certain thresholds"
sc_coloring_explanation_3 <- "Displays significance value of X and Y as color and size, respectively"




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
          
          
          radioGroupButtons("nxy_sc_dflogic",
                            label = HTML(paste0(
                              "Plot which data?",
                              add_help("nxy_sc_dflogic_help", style="margin-left: 5px;"))
                            ),
                            choices=dflogic_choices,
                            selected="Ins",size="s", direction="vertical"), 
          bsTooltip("nxy_sc_dflogic_help",
                    dflogic_explanation,
                    placement = "right"),
          radioTooltip(id = "nxy_sc_dflogic", choice = "Ins", 
                       title = dflogic_explanation_1, 
                       placement = "right"),
          radioTooltip(id = "nxy_sc_dflogic", choice = "Both", 
                       title = dflogic_explanation_2, 
                       placement = "right"),
          radioTooltip(id = "nxy_sc_dflogic", choice = "Either", 
                       title = dflogic_explanation_3, 
                       placement = "right"),
          radioTooltip(id = "nxy_sc_dflogic", choice = "All_Both", 
                       title = dflogic_explanation_4, 
                       placement = "right"),
          radioTooltip(id = "nxy_sc_dflogic", choice = "All_Either", 
                       title = dflogic_explanation_5, 
                       placement = "right"),
          
          
          
          radioButtons(
            inputId = "nxy_sc_plotmode",
            HTML(paste0(
              plotmode_label,
              add_help("nxy_sc_plotmode_help", style="margin-left: 5px;"))
            ),
            choices = c("No"="Focus","Yes"="Context")
          ),
          bsTooltip("nxy_sc_plotmode_help", 
                    plotmode_explanation, 
                    placement = "right"),
          
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE, width=300
        )
    ),
    div(style = "position: absolute; left: 4em; bottom: 1em",
        dropdown(
          
          radioButtons(
            inputId = "nxy_colormode",
            label = "Conditional coloring:",
            choices = sc_coloring_choices
            ),
          radioTooltip(id = "nxy_colormode", choice = sc_coloring_choices[[1]], 
                       title = sc_coloring_explanation_1, 
                       placement = "right"),
          radioTooltip(id = "nxy_colormode", choice = sc_coloring_choices[[2]], 
                       title = sc_coloring_explanation_2, 
                       placement = "right"),
          radioTooltip(id = "nxy_colormode", choice = sc_coloring_choices[[3]], 
                       title = sc_coloring_explanation_3, 
                       placement = "right"),
          uiOutput("nxy_colormode_options"),
          

          size = "xs",
          icon = icon("fill-drip", class = "opt"),
          up = TRUE, width=300
        )
    ),
    div(style = "position: absolute; left: 7em; bottom: 1em",
        dropdown(
          fluidRow(
            column(6,
                   numericInput(
                     inputId = "nxy_sc_size",
                     label = "Dot size:",
                     value = 3, step=0.5, width="100px")
                   ,
                   numericInput(
                     inputId = "nxy_sc_opacity",
                     label = "Opacity:",
                     value = 0.7, step=0.1, max=1, min=0, width="100px")
                   ,
                   ),
            column(6,
                   numericInput(
                     inputId = "nxy_sc_outlinewidth",
                     label = "Outline width:",
                     value = 1, step=0.1, min=0, width="100px")
                   ,
                   selectInput("nxy_sc_outlinecolor", 
                               "Outline color:",
                               choices = default_colors,
                               selected="white", width = "100px")
                   ,
                   )
            ),
          size = "xs",
          icon = icon("palette", class = "opt"),
          up = TRUE, width=270
        )
    ),
    div(style = "position: absolute; left: 10em; bottom: 1em",
        dropdown(
          uiOutput("nxy_color_summary_panel"),
          
          size = "xs",
          icon = icon("table", class = "opt"),
          up = TRUE, width=300
        )
    ),
    div(style = "position: absolute; left: 13em; bottom: 1em",
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
          radioGroupButtons("nxyz_sc_dflogic",
                            label = HTML(paste0(
                              "Plot which data?",
                              add_help("nxyz_sc_dflogic_help", style="margin-left: 5px;"))
                            ),
                            choices=dflogic_xyz_choices,
                            selected="Ins",size="s", direction="vertical"), 
          bsTooltip("nxyz_sc_dflogic_help",
                    dflogic_explanation,
                    placement = "right"),
          radioTooltip(id = "nxyz_sc_dflogic", choice = "Ins", 
                       title = dflogic_xyz_explanation_1, 
                       placement = "right"),
          radioTooltip(id = "nxyz_sc_dflogic", choice = "Both", 
                       title = dflogic_xyz_explanation_2, 
                       placement = "right"),
          radioTooltip(id = "nxyz_sc_dflogic", choice = "Either", 
                       title = dflogic_xyz_explanation_3, 
                       placement = "right"),
          radioTooltip(id = "nxyz_sc_dflogic", choice = "All_Both", 
                       title = dflogic_xyz_explanation_4, 
                       placement = "right"),
          radioTooltip(id = "nxyz_sc_dflogic", choice = "All_Either", 
                       title = dflogic_xyz_explanation_5, 
                       placement = "right"),
          
          
          radioButtons(
            inputId = "nxyz_sc_plotmode",
            HTML(paste0(
              plotmode_label,
              add_help("nxyz_sc_plotmode_help", style="margin-left: 5px;"))
            ),
            choices = c("No"= "Focus", "Yes"= "Context")
          ),
          bsTooltip("nxyz_sc_plotmode_help", 
                    plotmode_explanation, 
                    placement = "right"),
          
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE, width=300
        )
    ),
    div(style = "position: absolute; left: 4em; bottom: 1em",
        dropdown(
          
          radioButtons(
            inputId = "nxyz_colormode",
            label = "Conditional coloring:",
            choices = sc_xyz_coloring_choices, 
            selected = "None"
          ),
          radioTooltip(id = "nxyz_colormode", choice = sc_xyz_coloring_choices[[1]], 
                       title = sc_coloring_explanation_1, 
                       placement = "right"),
          radioTooltip(id = "nxyz_colormode", choice = sc_xyz_coloring_choices[[2]], 
                       title = sc_coloring_explanation_2, 
                       placement = "right"),
          
          uiOutput("nxyz_colormode_options"),
          
          
          size = "xs",
          icon = icon("fill-drip", class = "opt"),
          up = TRUE, width=300
        )
    ),
    div(style = "position: absolute; left: 7em; bottom: 1em",
        dropdown(
          fluidRow(
            column(6,
                   numericInput(
                     inputId = "nxyz_sc_size",
                     label = "Dot size:",
                     value = 3, step=0.5, width="100px")
                   ,
                   numericInput(
                     inputId = "nxyz_sc_opacity",
                     label = "Opacity:",
                     value = 0.7, step=0.1, max=1, min=0, width="100px")
                   ,
            ),
            column(6,
                   numericInput(
                     inputId = "nxyz_sc_outlinewidth",
                     label = "Outline width:",
                     value = 0, step=0.1, min=0, width="100px")
                   ,
                   selectInput("nxyz_sc_outlinecolor", 
                               "Outline color:",
                               choices = default_colors,
                               selected="white", width = "100px")
                   ,
            )
          ),
          
          size = "xs",
          icon = icon("palette", class = "opt"),
          up = TRUE, width=270
        )
    ),
    div(style = "position: absolute; left: 10em; bottom: 1em",
        dropdown(
          HTML(paste0(
            "<b>Color summary</b>:",
            add_help("nxyz_cdf_help", style="margin-left: 5px;"))
          ),
          bsTooltip("nxyz_cdf_help", 
                    "Summarizes terms by their displayed color", 
                    placement = "top"),
          dataTableOutput("n_3ds_prop_tbl"),br(),
          downloadButton("download_3ds_df", "Download color summary"),
          
          size = "xs",
          icon = icon("table", class = "opt"),
          up = TRUE, width=300
        )
    ),
    div(style = "position: absolute; left: 13em; bottom: 1em",
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
                         plotmode_label,
                         add_help("vol_plotmode_help", style="margin-left: 5px;"))
                       ),
                       choices = c("No"="Focus","Yes"="Context")
                     ),
                     
                     bsTooltip("vol_plotmode_help", 
                               plotmode_explanation, 
                               placement = "right"),
                     
                     
                     br(),
                     uiOutput("nx_vol_colthresh_opt"),
                     
                     size = "xs",
                     icon = icon("gear", class = "opt"),
                     up = TRUE
                   )
               ),
               
               div(id="nx_vol_aes", style = "position: absolute; left: 4em; bottom: 1em; width:300px;",
                   dropdown(
                     selectInput("nx_vol_c1", 
                                 HTML(paste0(
                                   "<b>Primary color:</b>",
                                   add_help("nx_vol_col1_help", style="margin-left: 5px;"))
                                 ),
                       choices = default_colors,
                       selected="red"
                       ),
                     selectInput("nx_vol_c2", 
                                 HTML(paste0(
                                   "<b>Secondary color:</b>",
                                   add_help("nx_vol_col2_help", style="margin-left: 5px;"))
                                 ),
                                 choices = default_colors,
                                 selected="black"
                     ),
                     selectInput("nx_vol_c3", 
                                 HTML(paste0(
                                   "<b>Tertiary color:</b>",
                                   add_help("nx_vol_col3_help", style="margin-left: 5px;"))
                                 ),
                                 choices = default_colors,
                                 selected="gray"
                     ),
                     
                     
                     bsTooltip("nx_vol_col1_help", 
                               "Highlight color for intersection terms within the threshold.", 
                               placement = "top"),
                     bsTooltip("nx_vol_col2_help", 
                               "Color for intersection terms that are NOT within the threshold.", 
                               placement = "top"),
                     bsTooltip("nx_vol_col3_help", 
                               "Color for terms that are NOT in the intersection. (only applicable for Context mode)", 
                               placement = "top"),
                     
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


