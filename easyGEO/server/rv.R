rv <- reactiveValues(
  # by default, if the count matrix contains more than 500,000 data points, edgeR/DESeq2 pipeline is disabled
  # if running easyGEO locally,
  # set DE_sizeFilter as FALSE, or increase the maxSize to enable analysis with larger data sets
  DE_sizeFilter = TRUE, maxSize = 200000,
  
  # ========= general RVs ======== #
  getgeo_mode = T, # switch to F if can't parse using GSEMatrix = TRUE
  gpl_type = list(), gpl_count = list(), gsmlist = list(),expr_nrow=0,
  run_mode = "auto", # auto for retrieval by GSE; manual for manual uploads
  demo_save = "no", # yes for saving the variables, no for regular run
  
  demo = "", # "yes" for a demo session, NULL for regular runs
  demo_n = 1, # odd for load, even for unload
  matrix_ready = NULL, # introjs use this variable as well
  
  # the error that occurs when there are 
  # samples in design matrix but not in data matrix
  column_match_error = FALSE, 
  
  gse_all = NULL,

  demo_acc = NULL, # default accession no

  fpta_ids = list(),

  bcol1 = "#e6f4fc", bcol2 = "#ffffe6", runs = 0,

  run_btn_color="warning", run_btn_style="simple",
  dbtn_color="warning", dbtn_style="unite",
  
  path_size=1e10, # maximum allowed GSE file size, default 10GB
  
  # ========= DE customization ======== #
  count_filter = "default", exp_unit = "cpm", exp_threshold = 1, exp_n = 3,
  fbe_min_count = 10, fbe_min_total_count = 15, fbe_large_n = 10, fbe_min_prop = 0.7,
  de_method = "default",
  edger_norm_method = "TMM",
  limma_norm_method = "quantile", limma_fit = "ls", limma_norm_method = "none",
  limma_trend = TRUE, limma_robust = TRUE,
  edger_trend_method = "locfit", edger_test = "glmQLFit", glmQLFit_robust = F,
  deseq2_test = "Wald", deseq2_fit = "parametric", deseq2_sf = "ratio", deseq2_bp = F,
  deseq2_shrink = "none",

  # ========= filtered data from DE run ======== #
  min_n = 5, # maximum no of min samples for filtering
  samples_null = NULL, # missed samples

  deg = NULL, # DEG table
  deg_counts = NULL, # normalized count table
  deg_pdata = NULL, # pData for DEG run
  c_var = NULL, c_level = NULL, t_level = NULL, samples_c = NULL, samples_t = NULL,

  # ========= parameters for DE visualizations ======== #
  plot_q=0.05, # adj.P.Val threshold for visualizations
  plot_logfc=1, # logfc threshold for visualization
  
  show_padj=T, show_logfc=T,show_padj_logfc=c("padj","fc"),
  v_threshold_line="dotted",

  v_mode = "static", # or "interactive"

  plot_label="top", # volcano's options to label/extract genes, other options: threshold manual
  plot_label_hm = "threshold", # heatmap's options to label/extract genes, other options: threshold manual

  volcano_up=15, volcano_down=15, # top # of genes to label in volcano & heatmap
  a_k=1.5,# no of sd to show in violin jitter

  h_log="yes",h_zscore="yes",a_log="yes", # transformation of count data, yes or no
  h_y_name = "title", # heatmap's samples label by accession or title
  h_cscale = "redBlueDiv", # default color scale
  h_cscale_rev = F, # reverse colorscale

  gene_lists=NULL, # user input gene list
  gene_lists_v=NULL, # gene list for volcano and heatmap

  v_success=NULL,h_success=NULL,a_success=NULL # show download plot button if success

)
