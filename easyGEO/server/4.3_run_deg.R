# ------------ UI: DEG run & table -----------
output$confirm_run <- renderUI({
  req(length(input$sp_select_levels)==2 & rv$matrix_ready==T & input$sp_select_var != input$sp_batch_col)
  
  # RUN DEG button
  bsButton("run_deg", "Run DEG analysis",
           icon = icon("play-circle"), 
           size = "large",
           style = "primary")
})

output$run_deg_ui <- renderUI({
  req(is.null(rv$deg)==F)
  
  tabBox(
    width = 12, title = "DEG Analysis",
    
    
    tabPanel(
      "DEG Table",
      fluidRow(
        column(
          width = 12,
          h4(HTML("Download entire DEG table and proceed to <b>easyGSEA</b> for gene set enrichment analysis and/or <b>easyVizR</b> for multiple comparisons.")),
          
        )
      ),
      fluidRow(
        column(
          width = 8,
          
          br(),
          dataTableOutput("deg_table")
        ),
        column(
          width = 4,
          br(),
          wellPanel(
            downloadButton("deg_table_download",label = "Download entire DEG table (.csv)"),
            tags$hr(style="border-color: grey;"),
            
            h4("Filter DEG table"),
            
            # adj.P.Val cutoff
            sliderTextInput(
              inputId = "tl_q",
              label = "Threshold of adj.P.Val",
              choices = cutoff_slider,
              selected = rv$plot_q, grid=T, force_edges=T
            ),
            # |logFC| cutoff
            numericInput(
              "tl_logfc",
              "Threshold of |logFC|",
              rv$plot_logfc,min=0
            ),
            uiOutput("tl_summary"),
            # download table
            downloadButton("tl_table","Download filtered table"),
            br(),br(),
            # download list
            downloadButton("tl_list","Download filtered gene list")
            
          )
        )
      )
    )
  )
})

# -------------- observe run_deg, perform DEG analysis ---------
observeEvent(input$run_deg,{
  rv$gene_lists = NULL
  rv$deg = NULL
  
  # selected samples
  samples_c = input$samples_c_deg
  samples_t = input$samples_t_deg
  min_n = min(length(samples_c),length(samples_t))
  
  # counts in at least 5 samples if too many samples as in scRNAseq
  if(min_n > 5){min_n = 5}
  
  msg = paste0("Running DEG analysis on ",length(samples_c)," vs. ",length(samples_t)," samples. Please wait a minute...")
  
  if(is.null(samples_c) && is.null(samples_t)){
    showNotification("Select at least 1 control and 1 experimental samples.", type = "error", duration=4)
    next
  }else if(is.null(samples_c)){
    showNotification("Select at least 1 control sample.", type = "error", duration=4)
    next
  }else if(is.null(samples_t)){
    showNotification("Select at least 1 experimental sample.", type = "error", duration=4)
    next
  }
  
  withProgress(message = msg, value = 1, {
    ## 1) create design matrix
    # 1.1) batch effects
    batch_var = input$sp_batch_col
    batch = NULL
    
    # original design matrix
    p_df = rv$fddf
    
    if(batch_var!="na"){
      batch = factor(p_df[[batch_var]])
    }
    
    # 1.2) filter design matrix according to the selected two levels in selected variable
    # selected variable
    c_var = input$sp_select_var
    
    # selected two levels
    c_var_levels = input$sp_select_levels
    
    # selected variable - control level
    c_level = input$sp_select_levels_base
    
    # selected variable - experimental level
    t_level = c_var_levels[!c_var_levels %in% c_level]
    
    # filter design matrix according to selections
    p_df1 = p_df %>% dplyr::filter(rownames(p_df) %in% samples_c)
    p_df2 = p_df %>% dplyr::filter(rownames(p_df) %in% samples_t)
    p_df = rbind(p_df1,p_df2)
    
    
    # 1.3) create treatment factor
    # treatment effects
    treatment = factor(p_df[[c_var]])
    treatment = relevel(treatment, ref = c_level)
    
    # 1.4) design matrix
    if(is.null(batch)){
      design1 <- model.matrix(~treatment)
    }else{
      design1 <- model.matrix(~batch+treatment)
    }
    
    ## 2) filter count matrix according to variable selection
    # filtered samples
    samples = rownames(p_df)
    
    # titles of filtered samples
    samples_title = translate_sample_names(samples,rv$pdata[c("title", "geo_accession")],  "title")
    
    # original count matrix
    m_df = filtered_data_df()
    
    # genes
    genes = m_df$Name %>% toupper(.)
    
    # as numeric matrix
    m_df = m_df %>% dplyr::select(one_of(samples))
    setcolorder(m_df, as.character(samples))
    m_df = m_df %>% 
      apply(., 2, as.numeric) %>%
      as.matrix(.)
    
    # rename rownames
    rownames(m_df) = genes
    
    ## 3) run edgeR and/or limma
    # 3.1) determine if raw or normalized counts
    raw_or_norm = input$data_type
    
    # 3.2) create dgelist
    y <- DGEList(counts=m_df)
    
    # 3.3) filter and normalize if raw read counts, and run limma
    if(raw_or_norm == "raw"){
      # filter genes expressed in at least 3 of the samples
      keep <- rowSums(cpm(y)>1) >= min_n
      y <- y[keep,,keep.lib.sizes=FALSE]
      
      # normalize count data
      y=calcNormFactors(y, method = "TMM")
      
      # voom on normalized data
      v <- voom(y, design1, plot=F)
    }else{
      keep <- rowSums(y$counts>1) >= min_n
      y <- y[keep,,keep.lib.sizes=TRUE]
      
      # # voom directly on counts, if data are very noisy, as would be used for microarray
      v <- voom(y, design1, plot=F, normalize="quantile")
    }
    
    # 3.4) DEG analysis
    fit <- lmFit(v, design1)
    fit <- eBayes(fit,trend=TRUE, robust=TRUE)
    
    # results
    results <- decideTests(fit)
    summary(results)
    
    # export DEG table
    degs = topTable(fit, coef=ncol(fit),sort.by="P",number=Inf)
    rv$deg = degs
    
    # export count table
    if(raw_or_norm == "raw"){
      rv$deg_counts = cpm(y)
    }else{
      rv$deg_counts = y$counts
    }
    
    # export other data
    rv$c_var = c_var
    rv$c_level = c_level
    rv$t_level = t_level
    rv$samples_c = samples_c
    rv$samples_t = samples_t
    
    rv$deg_pdata = p_df
  })
  
})

# -------------render DEG Table, download------------
output$deg_table <- DT::renderDataTable({
  req(is.null(rv$deg)==F)
  
  df = filter_df()
  
  genes = rownames(df)
  
  df = df %>% 
    dplyr::mutate_at(c("logFC","AveExpr","t","B"),function(x) round(x, digits = 1)) %>%
    dplyr::mutate_at(c("P.Value","adj.P.Val"),function(x) scientific(x, digits = 2))
  
  rownames(df) = genes
  
  df
})

# download DEG table
output$deg_table_download <- downloadHandler(
  filename = function() {paste0(rv$geo_accession,"_",rv$c_level,"_vs_",rv$t_level,".csv")},
  content = function(file) {
    write.csv(rv$deg, file)
  }
)

# -------------filter DEG Table, download------------
output$tl_summary <- renderUI({
  df = filter_df()
  n_after = nrow(df)
  n_total = nrow(rv$deg)
  
  fluidRow(
    box(
      background = "teal", width = 12,
      HTML(
        "No. of genes before filtering = <b>",n_total,"</b></br>",
        "No. of genes after filtering = <b>",n_after,"</b>",
      )
    )
  )
  
})

# download DEG table
output$tl_table <- downloadHandler(
  filename = function() {paste0(rv$geo_accession,"_",rv$c_level,"_vs_",rv$t_level,"_logFC",input$tl_logfc,"_q",input$tl_q,".csv")},
  content = function(file) {
    write.csv(filter_df(), file)
  }
)

# download DEG list
output$tl_list <- downloadHandler(
  filename = function() {paste0(rv$geo_accession,"_",rv$c_level,"_vs_",rv$t_level,"_logFC",input$tl_logfc,"_q",input$tl_q,".txt")},
  content = function(file) {
    fwrite(list(rownames(filter_df())), file)
  }
)