# ------------ overall visualization tab UI ---------------
output$ui_vis <- renderUI({
  if(!is.null(rv$demo_save) && rv$demo_save == "yes"){
    if(rv$run_mode == "manual"){
      saveRDS(rv$fddf_o, file ="rvs2/fddf_o.rds")
      saveRDS(rv$dmdf, file = "rvs2/dmdf.rds")
      saveRDS(rv$all_sample, file = "rvs2/all_samples.rds")
      saveRDS(rv$pdata, file = "rvs2/pdata.rds")
      saveRDS(rv$fddf, file = "rvs2/fddf.rds")
      saveRDS(rv$sup_source, file = "rvs2/sup_source.rds")
      saveRDS(rv$suplist, file = "rvs2/suplist.rds")
      saveRDS(rv$deg, file = "rvs2/deg.rds")
      saveRDS(rv$deg_counts, file = "rvs2/deg_counts.rds")
      saveRDS(rv$c_var, file = "rvs2/c_var.rds")
      saveRDS(rv$c_level, file = "rvs2/c_level.rds")
      saveRDS(rv$t_level, file = "rvs2/t_level.rds")
      saveRDS(rv$samples_c, file = "rvs2/samples_c.rds")
      saveRDS(rv$samples_t, file = "rvs2/samples_t.rds")
      saveRDS(rv$deg_pdata, file = "rvs2/deg_pdata.rds")
      saveRDS(rv$gpl_tooltips, file = "rvs2/gpl_tooltips.rds")
      saveRDS(rv$text, file = "rvs2/text.rds")
      saveRDS(rv$matrix_ready, file = "rvs2/matrix_ready.rds")
      saveRDS(rv$samples, file = "rvs2/samples.rds")
      saveRDS(rv$dmdf_samples, file = "rvs2/dmdf_samples.rds")
    } else {
      saveRDS(rv$pdata, file = "rvs/pdata.rds")
      saveRDS(rv$fddf, file = "rvs/fddf.rds")
      saveRDS(rv$sup_source, file = "rvs/sup_source.rds")
      saveRDS(rv$suplist, file = "rvs/suplist.rds")
      saveRDS(rv$deg, file = "rvs/deg.rds")
      saveRDS(rv$deg_counts, file = "rvs/deg_counts.rds")
      saveRDS(rv$c_var, file = "rvs/c_var.rds")
      saveRDS(rv$c_level, file = "rvs/c_level.rds")
      saveRDS(rv$t_level, file = "rvs/t_level.rds")
      saveRDS(rv$samples_c, file = "rvs/samples_c.rds")
      saveRDS(rv$samples_t, file = "rvs/samples_t.rds")
      saveRDS(rv$deg_pdata, file = "rvs/deg_pdata.rds")
      saveRDS(rv$gpl_tooltips, file = "rvs/gpl_tooltips.rds")
      saveRDS(rv$text, file = "rvs/text.rds")
      saveRDS(rv$matrix_ready, file = "rvs/matrix_ready.rds")
    }
  }
  if(is.null(rv$deg)){
    msg = "Visualization available upon successful DE run."
    panel_null(text = msg)
  }else{
    tabBox(
      title = "DE Visualization", width = 12,
      id = "visDEG", height = "720px",
      
      tabPanel(
        "Volcano plot", value="volcano",
        column(
          width = 8,
          uiOutput("ui_volcano")
        ),
        column(
          width = 4,
          uiOutput("vplot_parameters")
        )
      ),
      
      tabPanel(
        span(id = "heatmap_text", "Heatmap"), value="heatmap",
        column(
          width = 8,
          uiOutput("hm_area")
        ),
        column(
          width = 4,
          uiOutput("hplot_parameters")
        )
      ),
      
      tabPanel(
        span(id ="explore_text", "Explore a gene"), value="gene",
        column(
          width = 8,
          radioGroupButtons(
            inputId = "a_type",
            label = NULL,
            #choices = list("Violin plot"="violin", HTML("<p>Box plot</p>")="box"),
            choiceNames = list("Violin plot", HTML("<span id = 'boxplot_text'>Box plot</span>")),
            choiceValues = list("violin", "box")
          ),
          # tooltips for radioGroupbuttons
          radioTooltip(id = "a_type", choice = "violin", 
                       title = "Wiki: violin plot is a box plot with the addition of a rotated kernel density plot on each side.",
                       placement = "bottom", trigger = "hover"),
          radioTooltip(id = "a_type", choice = "box",
                       title = "Wiki: a box plot is a method for graphically depicting groups of numerical data through their quartiles.",
                       placement = "bottom", trigger = "hover"),
          
          plotOutput("ui_aplot",width = "100%", height = "600px")
        ),
        column(
          width = 4,
          uiOutput("aplot_parameters")
        )
        
      )
    )
    
  }
})

#---------------volcano: parameters------------------
# volcano parameters UI
output$vplot_parameters <- renderUI({
  wellPanel(
    # adj.P.Val cutoff
    sliderTextInput(
      inputId = "v_q_cutoff",
      label = "Threshold of adj.P.Val",
      choices = cutoff_slider,
      selected = rv$plot_q, grid=T, force_edges=T,
      width = "90%"
    ),
    # |logFC| cutoff
    numericInput(
      "v_logfc_cutoff",
      "Threshold of |logFC|",
      rv$plot_logfc,min=0
    )
    
    ,tags$hr(style="border-color: grey;")
    ,fluidRow(
      column(
        7,
        checkboxGroupButtons(
          inputId = "show_padj_logfc",
          label = "Plot line(s)",
          choices = c("adj.P.Val"="padj", 
                      "|logFC|"="fc"),
          selected = rv$show_padj_logfc, 
          size="s",
          checkIcon = list(
            no = tags$i(class = "fa fa-times", 
                        style = "color: crimson"),
            yes = tags$i(class = "fa fa-check", 
                         style = "color: green"))
        )
      )
      ,column(
        5,
        uiOutput("ui_v_line_type")
      )
    )

    ,tags$hr(style="border-color: grey;")
    
    # mode of volcano
    ,radioGroupButtons(
      "volcano_mode",
      "Mode of volcano plot",
      choices = list("Static"="static","Interactive"="interactive"),
      selected = rv$v_mode
    ),
    # tooltips for radioGroupbuttons
    radioTooltip(id = "volcano_mode", choice = "static", 
                 title = "In Static, notable genes can be highlighted with colors and labelled with names"
                 , placement = "right", trigger = "hover"),
    radioTooltip(id = "volcano_mode", choice = "interactive",
                 title = "In Interactive, genes that pass the defined adj.P.Val and |logFC| thresholds are highlighted in red. Hover labels show information on gene names, logFC and adj.P.Val"
                 , placement = "right", trigger = "hover"),
    # static/interactive UIs
    uiOutput("v_static"),
    uiOutput("v_interactive"),
    
    br(),
    splitLayout(
      bsButton(
        "volcano_confirm",
        tags$b("Visualize!"),
        style = "primary"
      ),
      uiOutput("ui_v_download")
    )
    
  )
  
})

# UI, line type for |logFC| and adj.P.Val
output$ui_v_line_type <- renderUI({
  req(!is.null(input$show_padj_logfc))
  
  radioGroupButtons(
    inputId = "v_threshold_line",
    label = "Line type",
    choices = c("Dot"="dotted"
                ,"Dash"="dashed")
    ,selected = rv$v_threshold_line
    ,size="sm"
    # ,checkIcon = list(
    #   yes = tags$i(class = "fa fa-check-square", 
    #                style = "color: steelblue"),
    #   no = tags$i(class = "fa fa-square-o", 
    #               style = "color: steelblue"))
  )
})

# UI, static volcano
output$v_static <- renderUI({
  req(input$volcano_mode == "static")
  
  fluidRow(
    column(
      width = 12,
      # if static, options to label genes
      radioGroupButtons(
        "v_label_opt",
        "Options to label genes",
        choices = label_options,
        selected = rv$plot_label
      ),
      # tooltips for radioGroupbuttons
      radioTooltip(id = "v_label_opt", choice = "threshold", 
                   title = "Genes are labeled in blue (downregulation) and red (upregulation) according to the defined logFC and adj.P.Val thresholds"
                   , placement = "top", trigger = "hover"),
      radioTooltip(id = "v_label_opt", choice = "top",
                   title = "Genes within the top # of |logFC|"
                   , placement = "top", trigger = "hover"),
      radioTooltip(id = "v_label_opt", choice = "manual",
                   title = "Manually label your genes of interest"
                   , placement = "top", trigger = "hover"),
      
      uiOutput("v_top"),
      uiOutput("v_manual"),
      uiOutput("v_box")
    )
  )
})


# UI if to label top genes in volcano
output$v_top <- renderUI({
  req(input$v_label_opt == "top")
  
  splitLayout(
    numericInput("n_up_volcano",
                 "# of top up",
                 rv$volcano_up, min=1,
                 width = "90%"),
    numericInput("n_down_volcano",
                 "# of top down",
                 rv$volcano_down, min=1,
                 width = "90%")
  )
})

# UI if manual selection of genes
output$v_manual <- renderUI({
  req(input$v_label_opt == "manual")
  
  textAreaInput(
    inputId = "v_gene_list",
    label = "Input your genes",
    placeholder = "Paste your genes here ...",
    height = 80
  )
})

# UI feedbacks on manual selection of genes
output$v_box <- renderUI({
  req(input$v_label_opt == "manual")
  
  if(is.null(rv$gene_lists)){
    box_color = "teal"
    msg = "Please input your genes and click <b>Visualize!</b> to update your list."
  }else{
    input_genes = paste0(
      "Your input: ",
      abbreviate_vector(rv$gene_lists), " (n=<b>",
      length(rv$gene_lists),"</b>)</br></br>"
    )
    if(is.null(rv$gene_lists_v) || length(rv$gene_lists_v)<1){
      box_color = "red"
      msg = paste0(input_genes,
                   "No gene found in DE table. Please check your input."
      )
    }else{
      output_genes = paste0(
        "Genes found: ",
        abbreviate_vector(rv$gene_lists_v), " (n=<b>",
        length(rv$gene_lists_v),"</b>)"
      )
      
      box_color = "green"
      msg = paste0(
        input_genes,
        output_genes)
    }
  }
  
  fluidRow(
    box(
      title = NULL, background = box_color, solidHeader = TRUE, width=12,
      HTML(msg)
    )
  )
})

# -------------- volcano: update parameters ---------------
# update volcano parameters when "Visualize!" clicked
observeEvent(input$volcano_confirm,{
  rv$v_success = NULL
  # confirm that the numericInputs are not NAs
  rv$error_par <- 0
  rv$error_par <- check_numericInput_na("v_logfc_cutoff", rv$error_par, "Threshold of |logFC|")
  req(rv$error_par == 0)
  # update thresholds and volcano mode
  rv$plot_q = input$v_q_cutoff
  rv$plot_logfc = input$v_logfc_cutoff
  rv$v_mode = input$volcano_mode
  rv$show_padj_logfc <- input$show_padj_logfc
  if("padj" %in% rv$show_padj_logfc){rv$show_padj <- T}else{rv$show_padj <- F}
  if("fc" %in% rv$show_padj_logfc){rv$show_logfc <- T}else{rv$show_logfc <- F}
  
  rv$v_threshold_line <- input$v_threshold_line
  
  # if interactive
  if(input$volcano_mode == "interactive"){
    rv$plot_label = "top"
    # if static
  }else if(input$volcano_mode == "static"){
    rv$plot_label = input$v_label_opt
    
    # if top
    if(input$v_label_opt == "top"){
      # check for the NAs first
      rv$error_par <- check_numericInput_na("n_up_volcano", rv$error_par, "# of top up")
      rv$error_par <- check_numericInput_na("n_down_volcano", rv$error_par, "# of top down")
      req(rv$error_par == 0)
      rv$volcano_up = input$n_up_volcano
      rv$volcano_down = input$n_down_volcano
      
      # if manual
    }else if(input$v_label_opt == "manual"){
      if(input$v_gene_list != ""){
        # read in gene list
        genelist = as.character(input$v_gene_list)
        genelist = gsub("\"","",genelist)
        genelist = strsplit(genelist,"\n")
        genelist = unlist(lapply(genelist, function(x) strsplit(x,'\\s*,\\s*')))
        genelist = unlist(lapply(genelist, function(x) strsplit(x,'\\s*;\\s*')))
        genelist = unlist(strsplit(genelist," "))
        genelist = toupper(unique(genelist))
        
        if(is.null(genelist)==F){
          # save original gene lists into RV
          rv$gene_lists = genelist
          
          # save genes found in DE table into RV
          rv$gene_lists_v = genelist[genelist %in% rownames(rv$deg)]
        }
      }
    }
    
  }
})

# --------------volcano: plot---------------
output$ui_volcano <- renderUI({
  if(rv$v_mode=="static"){
    plotOutput("v_plot_s",width = "100%",height = "650px")
  }else if(rv$v_mode=="interactive"){
    plotlyOutput("v_plot_i",width = "100%",height = "650px")
  }
})

output$v_plot_s <- renderPlot({
  req(rv$deg)
  req(rv$v_mode == "static")
  
  withProgress(message = "Updating static volcano plot...", value = 1,{
    volcano_ggplot()
  })
})

output$v_plot_i <- renderPlotly({
  req(rv$deg)
  req(rv$v_mode == "interactive")
  
  withProgress(message = "Updating interactive volcano plot...", value = 1, {
    volcano_plotly()
  })
})

# --------------volcano: download---------------
output$ui_v_download <- renderUI({
  req(rv$v_success == "yes")
  
  downloadButton("v_download","Download plot")
})

output$v_download <- downloadHandler(
  filename = function() {
    if(rv$v_mode == "static"){
      paste0(rv$geo_accession,"_",rv$c_level,"_vs_",rv$t_level,"_volcano.pdf")
    }else if(rv$v_mode == "interactive"){
      paste0(rv$geo_accession,"_",rv$c_level,"_vs_",rv$t_level,"_volcano.html")
      
    }
  },
  content = function(file) {
    if(rv$v_mode == "static"){
      ggsave(file,volcano_ggplot(), device = "pdf", width = 9, height = 9, dpi = 300, units = "in")
    }else if(rv$v_mode == "interactive"){
      saveWidget(as_widget(volcano_plotly()), file, selfcontained = TRUE)
    }
  }
)

#---------------heatmap: parameters------------------
# volcano parameters UI
output$hplot_parameters <- renderUI({
  wellPanel(
    # adj.P.Val cutoff
    sliderTextInput(
      inputId = "h_q_cutoff",
      label = "Threshold of adj.P.Val",
      choices = cutoff_slider,
      selected = rv$plot_q, grid=T, force_edges=T,
      width = "90%"
    ),
    # |logFC| cutoff
    numericInput(
      "h_logfc_cutoff",
      "Threshold of |logFC|",
      rv$plot_logfc,min=0
    ),
    # transform count data
    splitLayout(
      radioGroupButtons(
        "h_log",
        "Log2 transformation",
        choices = list("Yes"="yes","No"="no"),
        selected = rv$h_log
      ),
      radioGroupButtons(
        "h_zscore",
        "Z-score transformation",
        choices = list("Yes"="yes","No"="no"),
        selected = rv$h_zscore
      )
    ),
    if(rv$run_mode == "auto"){
    tags$hr(style="border-color: grey;")},
    if(rv$run_mode == "auto"){
      radioGroupButtons(
        inputId = "h_y_name",
        label = "Label samples by:", 
        choices = list("GEO accession"="accession", "Sample name"="title"),
        selected = rv$h_y_name
      )
    },
    # tooltips for radioGroupbuttons
    radioTooltip(id = "h_y_name", choice = "accession", 
                 title = "Label samples with GEO GSM accession numbers", placement = "bottom", trigger = "hover"),
    radioTooltip(id = "h_y_name", choice = "title",
                 title = "Label samples with author-supplied descriptive words", placement = "bottom", trigger = "hover"),
    
    tags$hr(style="border-color: grey;"),
    
    # options to change colorscale
    fluidRow(
      column(6,
        selectInput(
          inputId = "h_cscale",
          label= HTML(paste0(
            "<b>Select colorscale:</b>",
            add_help("h_cscale_help", style="margin-left: 5px;"))
          ),
          choices = div_seq_ora_colorChoices,
          selected = rv$h_cscale
        )
      ),
      column(6,
        radioGroupButtons(
          inputId = "h_cscale_rev", 
          label = HTML(paste0(
            "<b>Reverse colorscale?</b>",
            add_help("h_cscale_rev_help", style="margin-left: 5px;"))
          ),
          choices = list("Yes"=T,"No"=F),
          selected = rv$h_cscale_rev
        )
      )
    ),
    bsTooltip("h_cscale_help", 
              "<b>Divergent</b> colorscales are automatically centered at 0; <br><b>Sequential</b> colorscales are adjusted to the max/min of the data.", 
              placement = "top"),
    bsTooltip("h_cscale_rev_help", 
              "Display a reversed version of the selected colorscale.", 
              placement = "top"),

    
    tags$hr(style="border-color: grey;margin-top: 0px;"),
    
    # options to extract matrix
    radioGroupButtons(
      "h_label_opt",
      "Options to extract genes",
      choices = label_options,
      selected = rv$plot_label_hm
    ),
    # tooltips for radioGroupbuttons
    radioTooltip(id = "h_label_opt", choice = "threshold", 
                 title = "Extract genes using adj.P.Val and |logFC| thresholds as defined above", placement = "top", trigger = "hover"),
    radioTooltip(id = "h_label_opt", choice = "top",
                 title = "Extract genes within the top # of |logFC|", placement = "top", trigger = "hover"),
    radioTooltip(id = "h_label_opt", choice = "manual",
                 title = "Manually enter and extract your genes of interest", placement = "top", trigger = "hover"),
    
    uiOutput("h_top"),
    uiOutput("h_manual"),
    uiOutput("h_box"),
    
    br(),
    splitLayout(
      bsButton(
        "h_confirm",
        tags$b("Visualize!"),
        style = "primary"
      ),
      uiOutput("ui_h_download")
    )
    
  )
  
})


# UI if to label top genes in heatmap
output$h_top <- renderUI({
  req(input$h_label_opt == "top")
  
  splitLayout(
    numericInput("n_up_h",
                 "# of top up",
                 rv$volcano_up, min=1,
                 width = "90%"),
    numericInput("n_down_h",
                 "# of top down",
                 rv$volcano_down, min=1,
                 width = "90%")
  )
})

# UI if manual selection of genes
output$h_manual <- renderUI({
  req(input$h_label_opt == "manual")
  
  textAreaInput(
    inputId = "h_gene_list",
    label = "Input your genes",
    placeholder = "Paste your genes here ...",
    height = 80
  )
})

# UI feedbacks on manual selection of genes
output$h_box <- renderUI({
  req(input$h_label_opt == "manual")
  
  if(is.null(rv$gene_lists)){
    box_color = "teal"
    msg = "Please input your genes and click <b>Visualize!</b> to update your list."
  }else{
    input_genes = paste0(
      "Your input: ",
      abbreviate_vector(rv$gene_lists), " (n=<b>",
      length(rv$gene_lists),"</b>)</br></br>"
    )
    if(is.null(rv$gene_lists_v) || length(rv$gene_lists_v)<1){
      box_color = "red"
      msg = paste0(input_genes,
                   "No gene found in DE table. Please check your input."
      )
    }else{
      output_genes = paste0(
        "Genes found: ",
        abbreviate_vector(rv$gene_lists_v), " (n=<b>",
        length(rv$gene_lists_v),"</b>)"
      )
      
      box_color = "green"
      msg = paste0(
        input_genes,
        output_genes)
    }
  }
  
  fluidRow(
    box(
      title = NULL, background = box_color, solidHeader = TRUE, width=12,
      HTML(msg)
    )
  )
})

# -------------- heatmap: update parameters ---------------
# update heatmap parameters when "Visualize!" clicked
observeEvent(input$h_confirm,{
  rv$h_success = NULL
  # confirm that the numericInputs are not NAs
  rv$error_par <- 0
  rv$error_par <- check_numericInput_na("h_logfc_cutoff", rv$error_par, "Threshold of |logFC|")
  req(rv$error_par == 0)
  # update thresholds and volcano mode
  rv$plot_q = input$h_q_cutoff
  rv$plot_logfc = input$h_logfc_cutoff
  
  # count data transformation
  rv$h_log = input$h_log
  rv$h_zscore = input$h_zscore
  
  # colorscale
  rv$h_cscale = input$h_cscale
  rv$h_cscale_rev = input$h_cscale_rev
  
  # genes label by
  rv$h_y_name = input$h_y_name
  
  # options: threshold top manual
  rv$plot_label_hm = input$h_label_opt
  
  # if top
  if(input$h_label_opt == "top"){
    # check for the NAs first
    rv$error_par <- check_numericInput_na("n_up_h", rv$error_par, "# of top up")
    rv$error_par <- check_numericInput_na("n_down_h", rv$error_par, "# of top down")
    req(rv$error_par == 0)
    
    rv$volcano_up = input$n_up_h
    rv$volcano_down = input$n_down_h
    
    # if manual
  }else if(input$h_label_opt == "manual"){
    if(input$h_gene_list != ""){
      # read in gene list
      genelist = as.character(input$h_gene_list)
      genelist = gsub("\"","",genelist)
      genelist = strsplit(genelist,"\n")
      genelist = unlist(lapply(genelist, function(x) strsplit(x,'\\s*,\\s*')))
      genelist = unlist(lapply(genelist, function(x) strsplit(x,'\\s*;\\s*')))
      genelist = unlist(strsplit(genelist," "))
      genelist = toupper(unique(genelist))
      
      if(is.null(genelist)==F){
        # save original gene lists into RV
        rv$gene_lists = genelist
        
        # save genes found in DE table into RV
        rv$gene_lists_v = genelist[genelist %in% rownames(rv$deg)]
      }
    }
  }
})

# -------------- heatmap: plot ---------------
output$hm_area <- renderUI({
  req(rv$deg)
  
  df <- hm_count()
  dfm <- dim(df)
  dfmi <- dfm[1] * dfm[2]
  
  if(dfmi > 180000){
    HTML(
      "<br>We support a maximum of <i>180,000</i> data points in heatmaps",
      ". Please reduce the number of data points by adjusting <b>thresholds</b> and/or <b>Options to extract genes</b> in the right panel."
    )
  }else if(dfmi == 0){
    HTML(
      "<br>No gene found to be differentially expressed at thresholds of adj.P.Val < ",rv$plot_q
      ," and |logFC| &GreaterEqual; ",rv$plot_logfc
      ,". Adjust adj.P.Val and |logFC| on the right panel."
    )
  }else{
    plotlyOutput("heatmap_plot",width = "100%", height = "650px")
  }
})
output$heatmap_plot <- renderPlotly({
  req(rv$deg)

  withProgress(message = "Updating heatmap ...", value = 1,{
    hm_plot()
  })
})

# --------------heatmap: download---------------
output$ui_h_download <- renderUI({
  req(rv$h_success == "yes")
  
  downloadButton("h_download","Download plot")
})

output$h_download <- downloadHandler(
  filename = function() {paste0(rv$geo_accession,"_",rv$c_level,"_vs_",rv$t_level,"_hm.html")},
  content = function(file) {
    saveWidget(as_widget(hm_plot()), file, selfcontained = TRUE)
  }
)

#---------------one gene: parameters------------------
output$aplot_parameters <- renderUI({
  withProgress(message = "Loading genes for visualization...",value = 1,{
    wellPanel(
      selectizeInput(
        "aplot_genes",
        "Select your gene of interest:",
        choices = rownames(rv$deg),
        options = list(
          placeholder = 'Type to search ...',
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
      br(),
      tableOutput("a_stats"),
      br(),
      splitLayout(
        # transform count data
        radioGroupButtons(
          "a_log",
          "Log2 transformation",
          choices = list("Yes"="yes","No"="no"),
          selected = rv$a_log
        ),
        # no of sd in violin gitter
        numericInput(
          "a_sd_n",
          "If violin, # of s.d.",
          value = rv$a_k, min = 0.1, step = 0.1
        )
      ),
      
      br(),
      splitLayout(
        bsButton(
          "agene_confirm",
          tags$b("Visualize!"),
          style = "primary"
        ),
        uiOutput("ui_a_download")
      )
      
    )
  })
  
})

#---------------one gene: update parameters------------------
observeEvent(input$agene_confirm,{
  rv$a_success = NULL
  # check for the NAs first
  rv$error_par <- 0
  rv$error_par <- check_numericInput_na("a_sd_n", rv$error_par, " # of s.d.")
  req(rv$error_par == 0)
  
  rv$a_gene = input$aplot_genes
  rv$a_log = input$a_log
  
  rv$a_k = input$a_sd_n
})

#---------------one genes: plot----------------
# gene stats in table
output$a_stats <- renderTable({
  if(rv$demo == "yes" && input$agene_confirm==0){
    init_choices4()
  }
  
  req(rv$a_gene)
  
  cols = c("logFC","P.Value","adj.P.Val")
  
  rv$deg %>% dplyr::filter(rownames(.) == rv$a_gene) %>%
    dplyr::select(one_of(cols)) %>%
    dplyr::mutate_at(c("P.Value","adj.P.Val"), function(x) scientific(x, digits=3))
})

# violin/box plot
output$ui_aplot <- renderPlot({
  req(rv$deg_counts)
  req(rv$a_gene)
  
  y_label = "Expression level"
  
  if(rv$a_log == "yes"){
    y_label = "Log2(expression+1)"
  }
  
  rv$y_label = y_label
  
  if(input$a_type == "violin"){
    violin_plt(y_label)
  }else if(input$a_type == "box"){
    box_plt(y_label)
  }
})

# --------------one gene: download---------------
output$ui_a_download <- renderUI({
  req(rv$a_success == "yes")
  
  downloadButton("a_download","Download plot")
})

output$a_download <- downloadHandler(
  filename = function() {
    if(input$a_type == "violin"){
      paste0(rv$geo_accession,"_",rv$c_level,"_vs_",rv$t_level,"_violin.pdf")
    }else if(input$a_type == "box"){
      paste0(rv$geo_accession,"_",rv$c_level,"_vs_",rv$t_level,"_box.pdf")
    }
  },
  content = function(file) {
    if(input$a_type == "violin"){
      ggsave(file,violin_plt(rv$y_label), device = "pdf", width = 10, height = 10, dpi = 300, units = "in")
    }else if(input$a_type == "box"){
      ggsave(file,box_plt(rv$y_label), device = "pdf", width = 10, height = 10, dpi = 300, units = "in")
    }
  }
)

# adjust the numericInput if there are out of bound
observeEvent(input$n_up_volcano, {
  check_numericInput("n_up_volcano", 15)
})
observeEvent(input$n_down_volcano, {
  check_numericInput("n_down_volcano", 15)
})
observeEvent(input$n_up_h, {
  check_numericInput("n_up_h", 15)
})
observeEvent(input$n_down_h, {
  check_numericInput("n_down_h", 15)
})
# observeEvent(input$a_sd_n, {
#   check_numericInput("a_sd_n", 1.5, minimum = 0.1, integer_check = FALSE)
# })