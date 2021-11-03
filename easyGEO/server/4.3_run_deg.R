# ------------ UI: DE run -----------
# RUN DE button

output$confirm_run <- renderUI({
  if(input$ui_select == "sp"){
    req(length(input$sp_select_levels)==2 & rv$matrix_ready==T & input$sp_select_var != input$sp_batch_col)
    req(length(input$samples_c_deg)>0 && length(input$samples_t_deg)>0)
    btn_ui <- actionBttn("run_deg", "4.3. Run DE Analysis!",
                         icon = icon("play-circle"), 
                         style=rv$run_btn_style, color=rv$run_btn_color, size = "lg",
                         block = TRUE)
  }else if(input$ui_select == "coerce"){
    req(rv$matrix_ready==T)
    req(is.null(input$samples_c_deg2)==F & is.null(input$samples_t_deg2)==F)
    btn_ui <- actionBttn("run_deg2", "4.3. Run DE Analysis!",
                         icon = icon("play-circle"),
                         style=rv$run_btn_style, color=rv$run_btn_color, size = "lg",
                         block = TRUE)
  }

  div(
    btn_ui
    ,fluidRow(box(
      id="de_par_box",
      collapsible = T, collapsed = T, width = 12, status = "warning"
      ,title = span(style="color:gray;text-align: center;",HTML('Advanced DE run parameters - click the "<b>+</b>" button <i class="fas fa-hand-point-right"></i>'))
      ,wellPanel(
        style = paste0("background:",rv$bcol2),
        uiOutput("de_par")
      )
    ))
  )
})

output$de_par <- renderUI({
  # total data points
  if(input$ui_select == "sp"){
    n_samples <- length(input$samples_c_deg) + length(input$samples_t_deg)
  }else if(input$ui_select == "coerce"){
    n_samples <- length(input$samples_c_deg2) + length(input$samples_t_deg2)
  }
  n_total <- nrow(filtered_data_df()) * n_samples

  # method options
  if(input$data_type == "raw" & (rv$DE_sizeFilter & n_total <= rv$maxSize)){
    de_methods <- c(
      "limma (default)" = "default",
      "edgeR" = "edger",
      "DESeq2" = "deseq2"
    )
  }else{
    de_methods <- c("limma (default)" = "default")
  }

  fluidRow(
    conditionalPanel(
      'input.data_type == "normalized"',
      column(
        12,align="center",
        p(style = "color:gray", "Note: Only limma-based analysis pipeline is available for normalized count/array data.")
      )
    ),
    conditionalPanel(
      'input.data_type == "raw"',
      if(rv$DE_sizeFilter & n_total > rv$maxSize){
        column(
          12,align="center",
          HTML(paste0(
            "<p style='color:red;'>",
            "Note: Your selection contains more than ",formatC(rv$maxSize,format="d", big.mark=",")," data points."
            ," It is beyond our webserver capacity with edgeR/DESeq2."
            ," Thus, only limma-based analysis pipeline is provided."
            ," ",link_icon("sizeLimit_q","https://github.com/easygsea/eVITTA")
            ,"</p>"
            )
          )
          ,bsTooltip("sizeLimit_q",HTML(paste0(
            "To run edgeR/DESeq2 with such large dataset, please download our source code on GitHub, and set <b>DE_sizeFilter</b> as <b>FALSE</b> in server/rv.R"
            ,"<br><br>Click to access eVITTA source code on GitHub"
          )),placement = "bottom")
        )
      }
    ),
    # -------- UI: customizable gene filter -------
    fluidRow(
      column(
        12,
        column(
          4,
          selectInput(
            "count_filter",
            HTML(paste0("Filter genes:",add_help("count_filter_q"))),
            choices = c(
              "Default" = "default",
              "filterByExpr" = "filterByExpr",
              "Custom" = "custom"
            )
          )
          ,bsTooltip("count_filter_q",HTML(paste0(
            "Method to filter lowly expressed genes"
            ,". <b>Default</b> keeps genes expressed at a threshold of 1 in at least 5 of the samples, or the minimum number of biological repeats in each condition."
            ," <b>filterByExpr</b> by edgeR implements the filtering strategy that by Chen et al (2016)."
            ," <b>Custom</b> allows adjustments on both the thresholds and the minimum number of samples."
          )))
        )
        ,conditionalPanel(
          'input.count_filter == "filterByExpr"',
          column(
            2,
            numericInput(
              "fbe_min_count",HTML(paste0("min.count",add_help("fbe_min_count_q")))
              ,value = rv$fbe_min_count,min = 1,step = 1)
          )
          ,column(
            2,
            numericInput(
              "fbe_min_total_count",HTML(paste0("min.total.count",add_help("fbe_min_total_count_q")))
              ,value = rv$fbe_min_total_count,min = 1,step = 1)
          )
          ,column(
            2,
            numericInput(
              "fbe_large_n",HTML(paste0("large.n<br>",add_help("fbe_large_n_q")))
              ,value = rv$fbe_large_n,min = 1,step = 1)
          )
          ,column(
            2,
            numericInput(
              "fbe_min_prop",HTML(paste0("min.prop",add_help("fbe_min_prop_q")))
              ,value = rv$fbe_min_prop,min = 0,max=1,step = 0.05)
          )
          ,bsTooltip("fbe_min_count_q",HTML(paste0(
            "Minimum count required for at least some samples."
          )),placement = "bottom")
          ,bsTooltip("fbe_min_total_count_q",HTML(paste0(
            "Minimum total count required."
          )),placement = "bottom")
          ,bsTooltip("fbe_large_n_q",HTML(paste0(
            "Number of samples per group that is considered to be “large”."
          )),placement = "bottom")
          ,bsTooltip("fbe_min_prop_q",HTML(paste0(
            "Minimum proportion of samples in the smallest group that express the gene."
          )),placement = "bottom")
        )
        ,conditionalPanel(
          'input.count_filter == "custom"',
          column(
            3,
            selectInput(
              "exp_unit",
              HTML(paste0("Expr. unit:",add_help("exp_unit_q"))),
              choices = c(
                "CPM" = "cpm",
                "Counts" = "counts"
              )
              ,selected = rv$exp_unit
            )
            ,bsTooltip("exp_unit_q",HTML(paste0(
              "The expression unit to filter lowly expressed genes"
              ," .<b>CPM</b>: counts per million."
              ," <b>Counts</b>: raw counts."
            )),placement = "bottom")
          ),
          column(
            2,
            numericInput(
              "exp_threshold",
              HTML(paste0("Unit #:",add_help("exp_threshold_q"))),
              value = rv$exp_threshold, min = 0, step = 1
            )
            ,bsTooltip("exp_threshold_q",HTML(paste0(
              "The number of expression unit(s) of a gene considered to be expressed."
            )),placement = "bottom")
          )
          ,column(
            3,
            numericInput(
              "exp_n",
              HTML(paste0("Min sample #:",add_help("exp_n_q"))),
              value = rv$exp_n, min = 1, step = 1
            )
            ,bsTooltip("exp_n_q",HTML(paste0(
              "The minimum number of samples that have expression values of the defined threshold."
            )),placement = "bottom")
          )
        )
      )
    ),
    column(
      4,
      selectInput(
        "de_method",
        HTML(paste0("DE algorithm:",add_help("de_method_q"))),
        choices = de_methods,
        selected = rv$de_method
      )
      ,bsTooltip("de_method_q",HTML(paste0(
        "Method to perform differential expression (DE) analysis. By default, limma is applied, with edgeR to normalize raw counts (if applicable)."
      )),placement = "bottom")
    )
    # -------- UI: customizable DE parameters (limma) -------
    ,conditionalPanel(
      'input.de_method == "default" | input.de_method == "edger"',
      conditionalPanel(
        'input.data_type == "raw"',
        column(
          4,
          selectInput(
            "edger_norm_method",
            HTML(paste0("Count normalization:",add_help("edger_norm_method_q"))),
            choices = c(
              "TMM", "TMMwsp", "RLE", "upperquartile", "none"
            ),
            selected = rv$edger_norm_method
          )
          ,bsTooltip("edger_norm_method_q",HTML(paste0(
            "Method to normalize raw counts (edgeR)"
            ,".<br><b>TMM</b> (default): trimmed mean of M-values."
            ,"<br><b>TMMwsp</b>: TMM with singleton pairing."
            ,"<br><b>RLE</b>: relative log expression."
            ,"<br><b>upperquartile</b>: upper-quartile normalization."
            ,"<br><b>none</b>: no normalization."
          )),placement = "bottom")
        )
      )
      ,conditionalPanel(
        'input.data_type == "normalized"',
        column(
          4,
          selectInput(
            "limma_norm_method",
            HTML(paste0("Array normalization:",add_help("limma_norm_method_q"))),
            choices = c("quantile", "scale", "cyclicloess", "none")
            ,selected = rv$limma_norm_method
          )
          ,bsTooltip("limma_norm_method_q",HTML(paste0(
            "The microarray-style normalization method to be applied to the array or logCPM values (limma)."
          )),placement = "bottom")
        )
      )
    )
    ,conditionalPanel(
      'input.de_method == "default"',
      column(
        4,
        selectInput(
          "limma_fit",
          HTML(paste0("Fitting method:",add_help("limma_fit_q"))),
          choices = c("ls", "robust"),
          selected = rv$limma_fit
        )
        ,bsTooltip("limma_fit_q",HTML(paste0(
          "Method to fit linear model for each gene"
          ,": <b>ls</b> for least squares or <b>robust</b> for robust regression."
        )),placement = "bottom")
      )
      ,column(
        6,
        materialSwitch(
          inputId = "limma_trend",
          label = HTML(paste0("<b>","Intensity-trend for the prior variance?","</b>",add_help("limma_trend_q"))),
          value = rv$limma_trend, inline = F, width = "100%",
          status = "danger"
        )
        ,bsTooltip("limma_trend_q",HTML(paste0(
          "If TRUE (default), an intensity-trend is allowed for the prior variance. If FALSE, the prior variance is constant."
        )),placement = "bottom")
      )
      ,column(
        6,
        materialSwitch(
          inputId = "limma_robust",
          label = HTML(paste0("<b>","Robustified against outlier sample variances?","</b>",add_help("limma_robust_q"))),
          value = rv$limma_robust, inline = F, width = "100%",
          status = "danger"
        )
        ,bsTooltip("limma_robust_q",HTML(paste0(
          "If TRUE (default), the estimation of df.prior and var.prior is robustified against outlier sample variances."
        )),placement = "bottom")
      )
    )
    # -------- UI: customizable DE parameters (edgeR) -------
    ,conditionalPanel(
      'input.de_method == "edger"',
      column(
        4,
        selectInput(
          "edger_trend_method",
          HTML(paste0("Disp. trend:",add_help("edger_trend_method_q"))),
          choices = c(
            "locfit", "movingave", "loess", "locfit.mixed", "none"
          )
          ,selected = rv$edger_trend_method
        )
        ,bsTooltip("edger_trend_method_q",HTML(paste0(
          "Method for estimating dispersion trend"
        )),placement = "bottom")
      ),
      column(
        4,
        selectInput(
          "edger_test",
          HTML(paste0("DE test:",add_help("edger_test_q"))),
          choices = c("glmQLFit", "exactTest"),
          selected = rv$edger_test
        )
        ,bsTooltip("edger_test_q",HTML(paste0(
          "Method to test differentially expressed (DE) genes."
        )),placement = "bottom")
      )
      ,conditionalPanel(
        'input.edger_test == "glmQLFit"',
        column(
          4,
          materialSwitch(
            inputId = "glmQLFit_robust",
            label = HTML(paste0("<b>","Robust estimation?","</b>",add_help("glmQLFit_robust_q"))),
            value = rv$glmQLFit_robust, inline = F, width = "100%",
            status = "danger"
          )
          ,bsTooltip("glmQLFit_robust_q",HTML(paste0(
            "Whether to estimate the prior QL dispersion distribution robustly. Default: False."
          )),placement = "bottom")
        )
      )
    )
    # -------- UI: customizable DE parameters (DESeq2) -------
    ,conditionalPanel(
      'input.de_method == "deseq2"',
      column(
        4,
        selectInput(
          "deseq2_test",
          HTML(paste0("DE test:",add_help("deseq2_test_q"))),
          choices = c("LRT","Wald"),
          selected = rv$deseq2_test
        )
        ,bsTooltip("deseq2_test_q",HTML(paste0(
          "Method to test differentially expressed (DE) genes."
          ," <b>Wald</b> uses Wald significance tests (defined by nbinomWaldTest)"
          ," , and <b>LRT</b> uses the likelihood ratio test on the difference in deviance between a full and reduced model formula (defined by nbinomLRT)."
        )),placement = "bottom")
      )
      ,column(
        4,
        selectInput(
          "deseq2_fit",
          HTML(paste0("Fit type:",add_help("deseq2_fit_q"))),
          choices = c("parametric", "local", "mean", "glmGamPoi"),
          selected = rv$deseq2_fit
        )
        ,bsTooltip("deseq2_fit_q",HTML(paste0(
          "Method to obtain dispersion estimates for Negative Binomial distributed data. Check estimateDispersions in DESeq2 for details."
        )),placement = "bottom")
      )
      ,column(
        4,
        selectInput(
          "deseq2_sf",
          HTML(paste0("Size factor:",add_help("deseq2_sf_q"))),
          choices = c("ratio", "poscounts", "iterate"),
          selected = rv$deseq2_sf
        )
        ,bsTooltip("deseq2_sf_q",HTML(paste0(
          "Method to estimates the size factors. Check estimateSizeFactors in DESeq2 for details."
        )),placement = "bottom")
      )
      ,conditionalPanel(
        'input.deseq2_test != "LRT"',
        column(
          4,
          materialSwitch(
            inputId = "deseq2_bp",
            label = HTML(paste0("<b>","betaPrior?","</b>",add_help("deseq2_bp_q"))),
            value = rv$deseq2_bp, inline = F, width = "100%",
            status = "danger"
          )
          ,bsTooltip("deseq2_bp_q",HTML(paste0(
            "Whether or not to put a zero-mean normal prior on the non-intercept coefficients. Check nbinomWaldTest in DESeq2 for description of the calculation of the beta prior."
          )),placement = "bottom")
        )
      )
      ,column(
        4,
        selectInput(
          "deseq2_shrink",
          HTML(paste0("Shrinkage:",add_help("deseq2_shrink_q"))),
          choices = c("none","apeglm", "ashr", "normal"),
          selected = rv$deseq2_shrink
        )
        ,bsTooltip("deseq2_shrink_q",HTML(paste0(
          "Adds shrunken log2 fold changes (LFC) and SE to a results table from DESeq run without LFC shrinkage."
          ," <b>apeglm</b> is the adaptive Students\\\'t prior shrinkage estimator from the \\\'apeglm\\\' package"
          ,"; <b>ashr</b> is the adaptive shrinkage estimator from the \\\'ashr\\\' package, using a fitted mixture of normals prior - see the Stephens (2016) reference below for citation"
          ,"; <b>normal</b> is the 2014 DESeq2 shrinkage estimator using a Normal prior."
        )),placement = "bottom")
      )
    )
  )
})

# -------- update DE parameters -------
observeEvent(input$exp_unit,{input2rv("exp_unit")})
observeEvent(input$de_method,{input2rv("de_method")})
observeEvent(input$edger_norm_method,{input2rv("edger_norm_method")})
observeEvent(input$limma_norm_method,{input2rv("limma_norm_method")})
observeEvent(input$limma_fit,{input2rv("limma_fit")})
observeEvent(input$limma_norm_method,{input2rv("limma_norm_method")})
observeEvent(input$limma_trend,{input2rv("limma_trend")})
observeEvent(input$limma_robust,{input2rv("limma_robust")})
observeEvent(input$edger_trend_method,{input2rv("edger_trend_method")})
observeEvent(input$edger_test,{input2rv("edger_test")})
observeEvent(input$glmQLFit_robust,{input2rv("glmQLFit_robust")})
observeEvent(input$deseq2_test,{input2rv("deseq2_test")})
observeEvent(input$deseq2_fit,{input2rv("deseq2_fit")})
observeEvent(input$deseq2_sf,{input2rv("deseq2_sf")})
observeEvent(input$deseq2_bp,{input2rv("deseq2_bp")})
observeEvent(input$deseq2_shrink,{input2rv("deseq2_shrink")})

# ------------ UI: DE table -----------
output$run_deg_ui <- renderUI({
  req(is.null(rv$deg)==F)

  box(id="degs",
    width = 12, title = span(HTML("<b>4.4.</b>"),icon("book-open"),HTML("Review & download DE analysis results")), status = "primary",


      fluidRow(
        column(
          width = 12,
          wellPanel(
            style = paste0("background:",rv$bcol1),
            column(12, align = "center",
              downloadBttn("deg_table_download",label = "Download entire DE table (.csv)"
                           , style = rv$dbtn_style
                           , color = rv$dbtn_color
                           ,size="md")
            )
            ,br(),
            HTML("<br><br>Download entire DE table and proceed to <a href='http://tau.cmmt.ubc.ca/eVITTA/easyGSEA/' target='_blank'><u><b>easyGSEA</b></u></a> for gene set enrichment analysis
                  and/or <a href='http://tau.cmmt.ubc.ca/eVITTA/easyVizR/' target='_blank'><u><b>easyVizR</b></u></a> for multiple comparisons."
                 ),


            # tags$hr(style="border-color:grey;"),
            #
            # fluidRow(
            #   column(5,
            #     h4("Filtered DEG table")
            #   ),
            #   column(7,
            #     uiOutput("tl_summary")
            #   )
            # ),
            #
            # fluidRow(
            #   column(5,
            #     # adj.P.Val cutoff
            #     sliderTextInput(
            #       inputId = "tl_q",
            #       label = "Threshold of adj.P.Val",
            #       choices = cutoff_slider,
            #       selected = rv$plot_q, grid=T, force_edges=T
            #     )
            #   ),
            #   column(3,
            #     # |logFC| cutoff
            #     numericInput(
            #       "tl_logfc",
            #       "Threshold of |logFC|",
            #       rv$plot_logfc,min=0
            #     )
            #   ),
            #   column(4,
            #          # download table
            #          downloadButton("tl_table","Download filtered table"),
            #          # download list
            #          downloadButton("tl_list","Download filtered genes")
            #   )
            # )
            ))
      ),
      fluidRow(
        column(
          width = 12,
          br(),
          uiOutput("ui_deg_table"),
          br(),
          guide_box("guide4", "Navigate to <b>5. Visualize results</b> for visualizations"),
          br()
        )
      )
  )
})

# -------------- 4.3.1 observe run_deg (fine-tune mode), perform DEG analysis ---------
check_neg_counts <- function(){
  # check if any negative value in data matrix
  error <- 0
  if(any(filtered_data_df() < 0)){
    shinyalert("Error: Negative counts are not allowed.")
    error <- 1
  }
  # proceed only if right data matrix type is selected
  req(error == 0)
}

observeEvent(input$run_deg,{
  check_neg_counts()
  rv$gene_lists = NULL
  rv$deg = NULL


  # selected samples
  samples_c = input$samples_c_deg
  samples_t = input$samples_t_deg
  min_n <- find_min_n(samples_c,samples_t)

  msg = wait_msg(paste0("Running DE analysis on ",length(samples_c)," vs. ",length(samples_t)," samples..."))

  if((is.null(samples_c)||length(samples_c)<2) && (is.null(samples_t)||length(samples_t)<2)){
    shinyalert("Select at least 2 control and 2 experimental samples.")
  }else if(is.null(samples_c)||length(samples_c)<2){
    shinyalert("Select at least 2 control samples.")
  }else if(is.null(samples_t)||length(samples_t)<2){
    shinyalert("Select at least 2 experimental samples.")
  }else{
    withProgress(message = msg, value = 1, {
      ## 1) create design matrix
      # original design matrix
      p_df = rv$fddf

      # 1.1) filter design matrix according to the selected two levels in selected variable
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

      # 1.2) batch effects
      batch_var = input$sp_batch_col
      batch = NULL

      if(batch_var!="na"){
        batch = factor(p_df[[batch_var]])
      }

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
      if(rv$run_mode == "auto"){
        samples_title = translate_sample_names(samples,rv$pdata[c("title", "geo_accession")],  "title")
      } else {
        samples_title = samples
      }
      # original count matrix
      m_df = filtered_data_df() %>% as.data.frame(.)

      # genes
      genes = m_df$Name %>% toupper(.)

      # as numeric matrix
      m_df = m_df %>% dplyr::select(one_of(samples))
      m_df <- setcolorder(m_df, as.character(samples))
      m_df <- suppressWarnings(apply(m_df, 2, as.numeric))
      complete_cases <- complete.cases(m_df)
      rv$incomplete_cases_y <- !all(complete_cases)
      rv$incomplete_cases <- genes[which(!complete_cases)]
      complete_cases <- which(complete_cases)
      m_df <- m_df[complete_cases,]
      genes <- genes[complete_cases]
      m_df <- as.matrix(m_df)

      # rename rownames
      rownames(m_df) = genes

      # remove duplicates
      duplicates = duplicated(genes)
      m_df = m_df[!duplicates, ]

      ## 3) run edgeR and/or limma
      # 3.1) determine if raw or normalized counts
      raw_or_norm = input$data_type

      y <- run_DE(m_df,design1,min_n,raw_or_norm,p_df, batch_var, c_var, c_level)

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
  check_neg_counts()

  rv$gene_lists = NULL
  rv$deg = NULL

  # selected samples
  samples_c = input$samples_c_deg2
  samples_t = input$samples_t_deg2
  min_n <- find_min_n(samples_c,samples_t)

  msg = wait_msg(paste0("Running DE analysis on ",length(samples_c)," vs. ",length(samples_t)," samples..."))

  overlap = samples_c[samples_c %in% samples_t]

  if(length(overlap)>0){
    if(rv$run_mode == "auto"){
      overlap_names = translate_sample_names(overlap,rv$pdata[c("title", "geo_accession")],  "title")
    } else {
      overlap_names <- overlap
    }
    w_msg = paste0("You have selected ",abbreviate_vector(overlap)
                   , " (", abbreviate_vector(overlap_names),")"
                   , "(n = ",length(overlap),")"
                   ," in both control and experimental group. Please re-select.")

    shinyalert(w_msg)
  }else if((is.null(samples_c)||length(samples_c)<2) && (is.null(samples_t)||length(samples_t)<2)){
    shinyalert("Select at least 2 control and 2 experimental samples.")
  }else if(is.null(samples_c)||length(samples_c)<2){
    shinyalert("Select at least 2 control samples.")
  }else if(is.null(samples_t)||length(samples_t)<2){
    shinyalert("Select at least 2 experimental samples.")
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
      if(rv$run_mode == "auto"){
        samples_title = translate_sample_names(samples,rv$pdata[c("title", "geo_accession")],  "title")
      } else {
        samples_title = samples
      }

      # original count matrix
      m_df = filtered_data_df() %>% as.data.frame(.)

      # genes
      genes = m_df$Name %>% toupper(.)

      # as numeric matrix
      m_df = m_df %>% dplyr::select(one_of(samples))

      m_df <- setcolorder(m_df, as.character(samples))

      m_df <- suppressWarnings(apply(m_df, 2, as.numeric))
      complete_cases <- complete.cases(m_df)
      rv$incomplete_cases_y <- !all(complete_cases)
      rv$incomplete_cases <- genes[which(!complete_cases)]
      complete_cases <- which(complete_cases)
      m_df <- m_df[complete_cases,]
      genes <- genes[complete_cases]
      m_df <- as.matrix(m_df)

      # rename rownames
      rownames(m_df) = genes

      # remove duplicates
      duplicates = duplicated(genes)
      m_df = m_df[!duplicates, ]

      ## 3) run edgeR and/or limma
      # 3.1) determine if raw or normalized counts
      raw_or_norm = input$data_type

      p_dff <- data.frame(treatment = treatment, row.names = rownames(p_df))
      batch_var <- "na"; c_var <- "treatment"; c_level <- "c"
      y <- run_DE(m_df,design1,min_n,raw_or_norm,p_dff, batch_var, c_var, c_level)

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

  # df = filter_df()
  #
  # if(nrow(df)<1){
  #   msg = paste0("No significant results found at <b>adj.P.Val < ",input$tl_q,"</b> and <b>logFC >= ", input$tl_logfc
  #                ,"</b>. <br><br> Please adjust the filtering criteria above.")
  #   fluidRow(
  #     box(
  #       title = NULL, background = "red", solidHeader = TRUE, width=12,
  #       HTML(msg)
  #     )
  #   )
  # }else{
    dataTableOutput("deg_table")
  # }
})

# render DEG table if filtered row >= 1
output$deg_table <- DT::renderDataTable({
  req(is.null(rv$deg)==F)

  df <- mutate_df(df=rv$deg)
  df_no(df,scrollY = "185px")
})
#}, options = list(pageLength = 5))

# download DEG table
output$deg_table_download <- downloadHandler(
  filename = function() {paste0(rv$geo_accession,"_",rv$c_level,"_vs_",rv$t_level,".csv")},
  content = function(file) {
    write.csv(rv$deg, file)
  }
)

# # download DEG table
# output$deg_table_download2 <- downloadHandler(
#   filename = function() {paste0(rv$geo_accession,"_",rv$c_level,"_vs_",rv$t_level,".csv")},
#   content = function(file) {
#     write.csv(rv$deg, file)
#   }
# )

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
  js$collapse("de_par_box")
  showModal(modalDialog(
    id = "run_modal",
    title = NULL,
    div(style="font-size:150%",
      fluidRow(
        column(
          12, h3("DE analysis complete!")
        ),
        if(rv$incomplete_cases_y){
          box(
            title = NULL, background = "red", solidHeader = TRUE, width=12,
            HTML(paste0("Non-numeric data were found for: ",paste0(rv$incomplete_cases, collapse = ", ")
                        ,". Please double check your input data matrix file, revise it, re-upload and re-run the analysis for complete accuracy."
                        ," Or proceed by omitting genes with non-numeric data."))
          )
        },
        column(12,
           HTML("<br>Download entire DE table and proceed to <a href='http://tau.cmmt.ubc.ca/eVITTA/easyGSEA/' target='_blank'><u><b>easyGSEA</b></u></a> for gene set enrichment analysis
                  and/or <a href='http://tau.cmmt.ubc.ca/eVITTA/easyVizR/' target='_blank'><u><b>easyVizR</b></u></a> for multiple comparisons.
                <br>"
           )
           # ,downloadBttn("deg_table_download2",label = "Download entire DE table (.csv)"
           #              , style = rv$dbtn_style
           #              , color = rv$dbtn_color
           #              ,size="md")
           ,br(),br(),p("If you'd like to run DE analysis for another comparison, re-select samples and re-click \"4.3. Run DE Analysis!\".")
        )
      )
    ),
    easyClose = T,size="m"
    , footer = modalButton('OK')
  ))
},ignoreInit = T)

# ------------ guide to 5. visualize results page ------------
observeEvent(input$guide4,{
  updateTabItems(session, "menu1", "tab5")
})
