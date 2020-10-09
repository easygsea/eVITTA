# --------- numeric ID selection ----------
r_num_acc <- reactive({
  div(
    selectizeInput(
      "num_acc",
      HTML(paste0("Numeric IDs treated as:",add_help("num_acc_hp", style="padding:3px 0 0 0;position:absolute;right:1em;"))),
      choices = num_space[input$selected_species][[1]],
      selected = num_space[input$selected_species][[1]][grepl('ENTREZGENE',num_space[input$selected_species][[1]])]
    )
    ,bsTooltip("num_acc_hp","The identifier for fully numeric IDs"
               ,placement = "top")
  )
})

# ----------- bar plot dropdowns ---------
bar_gear <- reactive({
  if(input$selected_species != "other"){
    dbs = rv$dbs
  }else{
    dbs = rv$gmt_cs
  }
  
  div(
    style = "position: relative",
    box(
      width = 12,height = rv$box_h_a,align = "center",
      status = "primary", 
      div(
        style="overflow-y:scroll; overflow-x:scroll", #max-height:600px;
        uiOutput("plot_bar_none"),
        plotlyOutput("plot_bar", width = "100%", height = rv$box_h)
      ),
      div(
        align = "left",
        style = "position: absolute; left: 1em; bottom: 1em;",
        dropdown(
          selectizeInput("pathway_to_plot_bar",
                         "Select database(s) to plot",
                         choices = dbs,
                         selected = rv$bar_pathway,
                         multiple = TRUE),
          uiOutput("bar_top"),
          
          splitLayout(
            sliderTextInput("cutoff_bar_p",
                            label = "Adjust P threshold:",
                            choices= cutoff_slider,
                            selected=rv$bar_p_cutoff, grid=T, force_edges=T
            ),
            sliderTextInput("cutoff_bar_q",
                            label = "Adjust P.adj threshold:",
                            choices= cutoff_slider,
                            selected=rv$bar_q_cutoff, grid=T, force_edges=T
            )
          ),
          fluidRow(
            column(
              width = 4,
              radioGroupButtons(
                inputId = "p_or_q_bar",
                label = "Color by P or P.adj",
                choiceNames = c("P", "P.adj"),
                choiceValues = c("pval", "padj"),
                selected = rv$bar_pq,
                direction = "horizontal"
              )
            ),
            column(
              width = 5,
              radioGroupButtons(
                inputId = "abb_bar",
                label = "Abbreviate y axis labels",
                choiceNames = c("Yes", "No"),
                choiceValues = c("y", "n"),
                selected = rv$bar_abb,
                direction = "horizontal"
              )
            ),
            column(
              width = 3,
              uiOutput("ui_bar_abb_n")
            ),
            fluidRow(
              column(
                width = 3, offset = 9,
                bsButton("bar_confirm",tags$b("Replot!"),style = "danger")
              )
            )
            
          )
          
          ,
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE,width = "410px"
        )
      ),
      div(
        style = "position: absolute; left: 4.5em; bottom: 1em;",
        dropdown(
          downloadButton(outputId = "download_bar", label = "Download plot"),
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE
        )
      )
    )
  )
})

# --------------- manhattan plot dropdowns -------------
manhattan_gear <- reactive({
  div(
    style = "position: relative",
    box(
      width = 12,height = rv$box_h_a,align = "center",
      status = "primary", 
      div(
        style="overflow-y:scroll; overflow-x:scroll", #max-height:600px;
        plotlyOutput("plot_manhattan", width = "100%", height = rv$box_h)
      ),
      div(
        align = "left",
        style = "position: absolute; left: 1em; bottom: 1em;",
        dropdown(
          radioGroupButtons(
            inputId = "p_or_q_manhattan",
            label = "Threshold by P or P.adj",
            choiceNames = c("P", "P.adj"),
            choiceValues = c("pval", "padj"),
            selected = rv$volcano_pq,
            direction = "horizontal"
          ),
          fluidRow(
            column(
              width = 9,
              sliderTextInput("cutoff_manhattan",
                              label = "Adjust P or P.adj threshold:",
                              choices= cutoff_slider,
                              selected=rv$volcano_cutoff, grid=T, force_edges=T
              )
            ),
            column(
              width = 3,
              br(),br(),
              bsButton("manhattan_confirm",tags$b("Replot!"),style = "danger")
            )
          ),
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE,width = "410px"
        )
      ),
      div(
        style = "position: absolute; left: 4.5em; bottom: 1em;",
        dropdown(
          downloadButton(outputId = "download_manhattan", label = "Download plot"),
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE
        )
      )
    )
  )
})

# --------------- bubble plot dropdowns -----------------
bubble_gear <- reactive({
  if(input$selected_species != "other"){
    dbs = rv$dbs
  }else{
    dbs = rv$gmt_cs
  }
  
  div(
    style = "position: relative",
    box(
      width = 12,height = rv$box_h_a,align = "center",
      status = "primary", 
      div(
        style="overflow-y:scroll; overflow-x:scroll", #max-height:600px;
        uiOutput("plot_bubble_none"),
        plotlyOutput("plot_bubble", width = "100%",height = rv$box_h)
      ),
      div(
        align = "left",
        style = "position: absolute; left: 1em; bottom: 1em;",
        dropdown(
          selectizeInput("pathway_to_plot_bubble",
                         "Select database(s) to plot",
                         choices = dbs,
                         selected = rv$bar_pathway,
                         multiple = TRUE),
          uiOutput("bubble_top"),
          splitLayout(
            sliderTextInput("cutoff_p_bubble",
                            label = "Adjust P threshold:",
                            choices= cutoff_slider,
                            selected=rv$bar_p_cutoff, grid=T, force_edges=T
            ),
            sliderTextInput("cutoff_q_bubble",
                            label = "Adjust P.adj threshold:",
                            choices= cutoff_slider,
                            selected=rv$bar_q_cutoff, grid=T, force_edges=T
            )
          ),
          
          fluidRow(
            column(
              width = 4,
              radioGroupButtons(
                inputId = "p_or_q_bubble",
                label = "Color by P or P.adj",
                choiceNames = c("P", "P.adj"),
                choiceValues = c("pval", "padj"),
                selected = rv$bar_pq,
                direction = "horizontal"
              )
            ),
            column(
              width = 5,
              radioGroupButtons(
                inputId = "abb_bubble",
                label = "Abbreviate y axis labels",
                choiceNames = c("Yes", "No"),
                choiceValues = c("y", "n"),
                selected = rv$bar_abb,
                direction = "horizontal"
              )
            ),
            column(
              width = 3,
              uiOutput("ui_bubble_abb_n")
            )
          ),
          fluidRow(
            column(
              width = 9,
              sliderInput("bubble_slider", "Bubble size range", min = 0.5, max = 30, step = 0.5,
                          value = c(2.5, 9.5))
            ),
            column(
              width = 3,
              br(),br(),
              bsButton("bubble_confirm",tags$b("Replot!"),style = "danger")
            )
          )
          ,
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE,width = "410px"
        )
      ),
      div(
        style = "position: absolute; left: 4.5em; bottom: 1em;",
        dropdown(
          downloadButton(outputId = "download_bubble", label = "Download plot"),
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE
        )
      )
    )
  )
})

#--------------- volcano plot dropdowns ------------------
volcano_gear <- reactive({
  if(input$selected_species != "other"){
    dbs = rv$dbs
  }else{
    dbs = rv$gmt_cs
  }
  
  div(
    style = "position: relative",
    box(
      width = 12,height = rv$box_h_a,align = "center",
      status = "primary", 
      div(
        div(
          style="overflow-y:scroll; overflow-x:scroll", #max-height:600px; 
          uiOutput("ui_volcano_glist"),
          uiOutput("ui_volcano")
        ),
        div(
          align = "left",
          style = "position: absolute; left: 1em; bottom: 1em;",
          dropdown(
            selectizeInput("pathway_to_plot_volcano",
                           "Select database(s) to plot",
                           choices = dbs,
                           selected = rv$volcano_pathway,
                           multiple = TRUE),
            fluidRow(
              column(
                width = 12,
                radioGroupButtons(
                  inputId = "p_or_q_volcano",
                  label = "Color by P or P.adj",
                  choiceNames = c("P", "P.adj"),
                  choiceValues = c("pval", "padj"),
                  selected = rv$volcano_pq,
                  direction = "horizontal"
                )
              )
              
            ),
            fluidRow(
              column(
                width = 12,
                radioGroupButtons(
                  inputId = "volcano_mode",
                  label = "Mode of plots",
                  choiceNames = c("Continuous", "Discrete","Static"),
                  choiceValues = c("plotly", "plotly2","ggplot"),
                  selected = rv$volcano_mode,
                  direction = "horizontal"
                )
              )
            ),
            uiOutput("ui_volcano_cutoff"),
            # uiOutput("ui_volcano_top"),
            fluidRow(
              column(
                width = 3, offset = 9,
                br(),
                bsButton("volcano_confirm",tags$b("Replot!"),style = "danger")
              )
            ),
            size = "xs",
            icon = icon("gear", class = "opt"),
            up = TRUE,width = "410px"
          ),
        ),
        div(
          style = "position: absolute;left: 4.5em;bottom: 1em",
          dropdown(
            downloadButton(outputId = "download_volcano", label="Download plot"),
            size = "xs",
            icon = icon("download",class="opt"),
            up=TRUE
          )
        )
      )
    )
  )
})

# ----------- keywords plot dropdowns ----------------
keywords_gear <- reactive({
  if(input$selected_species != "other"){
    dbs = rv$dbs
  }else{
    dbs = rv$gmt_cs
  }
  
  div(
    style = "position: relative",
    box(
      width = 12,height = rv$box_h_a,align = "center",
      status = "primary", 
      div(
        style="overflow-y:scroll; overflow-x:scroll", #max-height:600px;
        uiOutput("plot_word_none"),
        plotlyOutput("plot_word", width = "100%",height = rv$box_h)
      ),
      div(
        align = "left",
        style = "position: absolute; left: 1em; bottom: 1em;",
        dropdown(
          selectizeInput("pathway_to_plot_word",
                         "Select database(s) to plot",
                         choices = dbs,
                         selected = rv$bar_pathway,
                         multiple = TRUE),
          splitLayout(
            sliderTextInput("cutoff_word_p",
                            label = "Adjust P.adj threshold:",
                            choices= cutoff_slider,
                            selected=rv$bar_p_cutoff, grid=T, force_edges=T
            ),
            sliderTextInput("cutoff_word_q",
                            label = "Adjust P.adj threshold:",
                            choices= cutoff_slider,
                            selected=rv$bar_q_cutoff, grid=T, force_edges=T
            )
          ),
          numericInput("n_word",
                       "# of top words",
                       rv$n_word, min=1,
                       width = "50%"
          ),
          fluidRow(
            column(
              width = 12, align = "right",
              br(),br(),
              bsButton("word_confirm",tags$b("Replot!"),style = "danger")
            )
          )
          ,
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE,width = "410px"
        )
      ),
      div(
        style = "position: absolute; left: 4.5em; bottom: 1em;",
        dropdown(
          downloadButton(outputId = "download_word", label = "Download plot"),
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE
        )
      )
    )
  )
})