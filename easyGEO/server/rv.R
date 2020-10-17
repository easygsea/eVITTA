rv <- reactiveValues(
  demo = "", # "yes" for a demo session, NULL for regular runs
  demo_n = 1, # odd for load, even for unload
  
  gse_all = NULL,
  
  demo_acc = NULL, # default accession no
  
  fpta_ids = list(),
  
  bcol1 = "#e6f4fc", runs = 0,
  
  run_btn_color="warning", run_btn_style="simple",
  dbtn_color="warning", dbtn_style="unite",

  # ========= filtered data from DEG run ======== #
  min_n = 5, # maximum no of min samples for filtering
  samples_null = NULL, # missed samples
  
  deg = NULL, # DEG table
  deg_counts = NULL, # normalized count table
  deg_pdata = NULL, # pData for DEG run
  c_var = NULL, c_level = NULL, t_level = NULL, samples_c = NULL, samples_t = NULL,
  
  # ========= parameters for DEG visualizations ======== #
  plot_q=0.05, # adj.P.Val threshold for visualizations
  plot_logfc=1, # logfc threshold for visualization
  
  v_mode = "static", # or "interactive"
  
  plot_label="top", # options to label/extract genes, other options: threshold manual
  
  volcano_up=15, volcano_down=15, # top # of genes to label in volcano & heatmap
  a_k=1.5,# no of sd to show in violin jitter
  
  h_log="yes",h_zscore="yes",a_log="yes", # transformation of count data, yes or no
  h_y_name = "title", # heatmap's samples label by accession or title
  
  gene_lists=NULL, # user input gene list
  gene_lists_v=NULL, # gene list for volcano and heatmap
  
  v_success=NULL,h_success=NULL,a_success=NULL # show download plot button if success
  
)