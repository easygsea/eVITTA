# ------------ UI: DEG run & table -----------
# RUN DEG button

output$confirm_run <- renderUI({
  if(input$ui_select == "sp"){
    req(length(input$sp_select_levels)==2 & rv$matrix_ready==T & input$sp_select_var != input$sp_batch_col)
    # req(length(input$samples_c_deg)>0 && length(input$samples_t_deg)>0)
    actionBttn("run_deg", "4.3. Run DEG Analysis!",
             icon = icon("play-circle"), 
             style=rv$run_btn_style, color=rv$run_btn_color, size = "lg",
             block = TRUE)
  }else if(input$ui_select == "coerce"){
    req(rv$matrix_ready==T)
    req(is.null(input$samples_c_deg2)==F & is.null(input$samples_t_deg2)==F)
    actionBttn("run_deg2", "4.3. Run DEG Analysis!",
             icon = icon("play-circle"), 
             style=rv$run_btn_style, color=rv$run_btn_color, size = "lg",
             block = TRUE)
  }
})

output$run_deg_ui <- renderUI({
  req(is.null(rv$deg)==F)
  
  box(
    width = 12, title = span(HTML("<b>4.4.</b>"),icon("book-open"),HTML("Review & "),icon("download"),HTML("download DEG analysis results")), status = "primary",
    
    
      fluidRow(
        column(
          width = 12,
          wellPanel(
            style = paste0("background:",rv$bcol1),
            downloadBttn("deg_table_download",label = "Download entire DEG table (.csv)"
                         , style = rv$dbtn_style
                         , color = rv$dbtn_color
                         ,size="sm"),
            HTML("<br><br><b>Note:</b> Download entire DEG table and proceed to <a href='http://tau.cmmt.ubc.ca/eVITTA/easyGSEA/' target='_blank'><u><b>easyGSEA</b></u></a> for gene set enrichment analysis 
                  and/or <a href='http://tau.cmmt.ubc.ca/eVITTA/easyVizR/' target='_blank'><u><b>easyVizR</b></u></a> for multiple comparisons."),
            
            tags$hr(style="border-color:grey;"),
            
            fluidRow(
              column(5,
                h4("Filtered DEG table")
              ),
              column(7,
                uiOutput("tl_summary")
              )
            ),
            
            fluidRow(
              column(5,
                # adj.P.Val cutoff
                sliderTextInput(
                  inputId = "tl_q",
                  label = "Threshold of adj.P.Val",
                  choices = cutoff_slider,
                  selected = rv$plot_q, grid=T, force_edges=T
                )
              ),
              column(3,
                # |logFC| cutoff
                numericInput(
                  "tl_logfc",
                  "Threshold of |logFC|",
                  rv$plot_logfc,min=0
                )
              ),
              column(4, 
                     # download table
                     downloadButton("tl_table","Download filtered table"),
                     # download list
                     downloadButton("tl_list","Download filtered genes")
              )
            )
            ))
      ),
      fluidRow(
        column(
          width = 12,
          br(),
          uiOutput("ui_deg_table")
        )
      )
  )
})

# -------------- 4.3.1 observe run_deg (fine-tune mode), perform DEG analysis ---------
observeEvent(input$run_deg,{
  rv$gene_lists = NULL
  rv$deg = NULL

    
  # selected samples
  samples_c = input$samples_c_deg
  samples_t = input$samples_t_deg
  min_n = min(length(samples_c),length(samples_t))
  
  # counts in at least rv$min_n samples if too many samples as in scRNAseq
  if(min_n > rv$min_n){min_n = rv$min_n}
  
  msg = paste0("Running DEG analysis on ",length(samples_c)," vs. ",length(samples_t)," samples. Please wait a minute...")
  
  if(is.null(samples_c) && is.null(samples_t)){
    showNotification("Select at least 1 control and 1 experimental samples.", type = "error", duration=4)
  }else if(is.null(samples_c)){
    showNotification("Select at least 1 control sample.", type = "error", duration=4)
  }else if(is.null(samples_t)){
    showNotification("Select at least 1 experimental sample.", type = "error", duration=4)
  }else{
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
      m_df = filtered_data_df() %>% as.data.frame(.)
      
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
      
      # remove duplicates
      duplicates = duplicated(genes)
      m_df = m_df[!duplicates, ]

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
      
      rv$runs = rv$runs + 1
    })
  }
})

# -------------- 4.3.2 observe run_deg (coerce mode), perform DEG analysis ---------
observeEvent(input$run_deg2,{
  rv$gene_lists = NULL
  rv$deg = NULL

  # selected samples
  samples_c = input$samples_c_deg2
  samples_t = input$samples_t_deg2
  min_n = min(length(samples_c),length(samples_t))
  
  # counts in at least rv$min_n samples if too many samples as in scRNAseq
  if(min_n > rv$min_n){min_n = rv$min_n}
  
  msg = paste0("Running DEG analysis on ",length(samples_c)," vs. ",length(samples_t)," samples. Please wait a minute...")
  
  overlap = samples_c[samples_c %in% samples_t]
  
  if(length(overlap)>0){
    overlap_names = translate_sample_names(overlap,rv$pdata[c("title", "geo_accession")],  "title")
    print(overlap_names)
    
    w_msg = paste0("You have selected ",abbreviate_vector(overlap)
                   , " (", abbreviate_vector(overlap_names),")"
                   , "(n = ",length(overlap),")"
                   ," in both control and experimental group. Please re-select.")

    showNotification(w_msg, type = "error", duration=5)
  }else{
    withProgress(message = msg, value = 1, {
      ## 1) create design matrix
      # original design matrix
      p_df = rv$fddf
      
      # 1.1) filter design matrix according to selections
      p_df1 = p_df %>% dplyr::filter(rownames(p_df) %in% samples_c)
      p_df2 = p_df %>% dplyr::filter(rownames(p_df) %in% samples_t)
      p_df = rbind(p_df1,p_df2)
      
      # 1.2) create treatment factor
      # treatment effects
      treatment = factor(c(rep("c",nrow(p_df1)),rep("t",nrow(p_df2))))
      treatment = relevel(treatment, ref = "c")
      
      # 1.3) design matrix
      design1 <- model.matrix(~treatment)
      
      ## 2) filter count matrix according to variable selection
      # filtered samples
      samples = rownames(p_df)
      
      # titles of filtered samples
      samples_title = translate_sample_names(samples,rv$pdata[c("title", "geo_accession")],  "title")
      
      # original count matrix
      m_df = filtered_data_df() %>% as.data.frame(.)

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
      
      # remove duplicates
      duplicates = duplicated(genes)
      m_df = m_df[!duplicates, ]

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
      rv$c_level = "Control"
      rv$t_level = "Experimental"
      rv$samples_c = samples_c
      rv$samples_t = samples_t
      
      rv$deg_pdata = p_df
      
      rv$runs = rv$runs + 1
    })
  }
})

# -------------render DEG Table, download------------
output$ui_deg_table <- renderUI({
  req(is.null(rv$deg)==F)
  
  df = filter_df()
  
  if(nrow(df)<1){
    msg = paste0("No significant results found at <b>adj.P.Val < ",input$tl_q,"</b> and <b>logFC >= ", input$tl_logfc
                 ,"</b>. <br><br> Please adjust the filtering criteria above.")
    fluidRow(
      box(
        title = NULL, background = "red", solidHeader = TRUE, width=12,
        HTML(msg)
      )
    )
  }else{
    dataTableOutput("deg_table")
  }
})

# render DEG table if filtered row >= 1
output$deg_table <- DT::renderDataTable({
  req(is.null(rv$deg)==F)
  
  mutate_df()
}, options = list(pageLength = 4))

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
      background = "teal", width = 12, align = "center",
      HTML(
        "Before filtering n = <b>",n_total,"</b>; ",
        "After filtering n = <b>",n_after,"</b>",
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

# ---------------- 4.3.3 render modal upon successful DEG run --------------
observeEvent(rv$runs,{
  showModal(modalDialog(
    id = "run_modal",
    title = NULL,
    div(style="font-size:150%",
      fluidRow(
        column(12,
           h3("DEG analysis complete!"),
           br(),p("Download DEG table and explore the results.")
           ,br(),p("If you'd like to run DEG analysis for another comparison, re-select samples and re-click \"4.3. Run DEG Analysis!\".")
        )
      )
    ),
    easyClose = T,size="m"
    , footer = modalButton('Got it!')
  ))
},ignoreInit = T)
