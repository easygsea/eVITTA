#======================================================================#
####                    INITIALIZE DEMO RVS                         ####
#======================================================================#

# the function to initialize the RVs for the demo session
init_demo <- function(){
  rv$nx_n <- readRDS(paste0(getwd(), "/rvs/nx_n.rds"))
  
  rv$n_to_plot <- "Stat"
  rv$heatmap_sortby <- rv$nx_n[[1]]
  rv$n_hm_ylabs <- F
  rv$n_hm_ylabs_len <- 15
  rv$n_hm_cscale <- "redBlueDiv"
  rv$n_hm_cscale_rev <- F
  rv$n_hm_cscale_center <- T
  rv$n_ins_view == "Full"
  rv$n_venn_label <- "counts"
  rv$n_venn_type <- "Basic"
  rv$n_venn_show_ins <- T
  rv$ins_venn_c1 <- "gold"
  rv$n_upset_sortby <- "freq"
  rv$n_upset_showempty <- FALSE
  rv$n_upset_show_ins <- T
  rv$n_upset_c1 <- "gold"
  rv$nxy_selected_x <- rv$nx_n[[1]]
  rv$nxy_selected_y <- rv$nx_n[[2]]
  rv$nxy_selected_z <- "None"
  rv$nxy_colormode <- "None"
  rv$nxy_sig <- "PValue"
  rv$nxy_thresh <- 0.01
  rv$nxy_sc_size <- 3
  rv$nxy_sc_opacity <- 0.7
  rv$nxy_sc_outlinewidth <- 1
  rv$nxy_sc_outlinecolor <- "white"
  rv$nxy_p <- 0.05
  rv$nxy_q <- 1
  rv$nxy_stat <- 0.5
  rv$n_sc_logic <- "Both"
  rv$nxy_sc_plotmode <- "Focus"
  rv$nxy_sc_dflogic <- "Ins"
  rv$nxyz_sc_logic <- "Both"
  rv$nxyz_colormode <- "None"
  rv$n_3ds_p <- 0.05
  rv$n_3ds_q <- 1
  rv$n_3ds_Stat <- 0
  rv$nxyz_sc_size <- 3
  rv$nxyz_sc_opacity <- 0.7
  rv$nxyz_sc_outlinewidth <- 0
  rv$nxyz_sc_outlinecolor <- "white"
  rv$nxyz_sc_plotmode <- "Focus"
  rv$nxyz_sc_dflogic <- "Ins"
  rv$rrho_x <- rv$nx_n[[1]]
  rv$rrho_y <- rv$nx_n[[2]]
  rv$rrho_level_palette <- "default"
  rv$rrho_level_palette_reverse <- F
  rv$rrho_level_setting <- F
  rv$nx_vol_plotmode <- "Focus"
  rv$nx_selected <- rv$nx_n[[1]]
  rv$nx_p <- 0.05
  rv$nx_Stat <- 0
  rv$nx_vol_c1 <- "red"
  rv$nx_vol_c2 <- "black"
  rv$nx_vol_c3 <- "gray"
  rv$nx_bar_sig <- "PValue"
  rv$nx_bar_to_plot <- "Stat"
  rv$nw_selected_n <- rv$nx_n[[1]]
  rv$nw_le_sep <- ";"
  rv$nw_selected_col <- firstmatch(le_alias,rv$nw_char_stats)
  rv$p_or_q_vis <- "PValue"
  rv$vis_percent <- "jaccard"
  rv$vis_percent_cutoff <- 0.25
  rv$combined_k <- 0.5
  
  #Correlogram
  # Parsa's stuff
  rv$corrExcelStyleLetters <- c(LETTERS, unlist(sapply(seq_along(LETTERS), function(i){paste0(LETTERS[i], LETTERS)})))
  rv$corrDataSetN <- paste("DataSet", rv$corrExcelStyleLetters[1:length(rv$nx_n)], sep="")
  rv$corrDatasetRepresentation <- data.frame(datasetName = rv$nx_n, abbreviation = rv$corrDataSetN, displayName = paste(rv$corrDataSetN, rv$nx_n, sep=" = "))
  # rownames(rv$corrDatasetRepresentation) <- rv$nx_n

  rv$corrVarSelected <- rv$corrDatasetRepresentation$datasetName[1:3]
  rv$corrDataOptions <- "All data"
  rv$corrPlotType <- "Heatmap"
  rv$corrCorrelateBy <- "rValue"
  rv$corrShowCorrelationValue <- FALSE
  rv$corrUpper <- "cor"
  rv$corrDiag <- "densityDiag"
  rv$corrLower <- "points"
  rv$corrUpperV <- "cor"
  rv$corrDiagV <- "densityDiag"
  rv$corrLowerV <- "points"
  
  rv$folder_upload_state <- "uploaded"
  #readRDS(paste0(getwd(), "/rvs/folder_upload_state.rds"))
  rv$upload_batch_colscheme <- readRDS(paste0(getwd(), "/rvs/upload_batch_colscheme.rds"))
  rv$upload_batch_sharedcols <- readRDS(paste0(getwd(), "/rvs/upload_batch_sharedcols.rds"))
  rv$batch_failed <- readRDS(paste0(getwd(), "/rvs/batch_failed.rds"))
  #rv$batch_files <- readRDS(paste0(getwd(), "/rvs/batch_files.rds"))
  # rv$batch_files$datapath[[1]] <- paste0(getwd(), "/inc/example_data_folder_for_VizR/A549-ACE2_SARS-CoV-2_HiMOI_KEGG-WkPt-RctP-BlgP.csv")
  # rv$batch_files$datapath[[2]] <- paste0(getwd(), "/inc/example_data_folder_for_VizR/A549-ACE2_SARS-CoV-2_LowMOI_KEGG-WkPt-RctP-BlgP.csv")
  # rv$batch_files$datapath[[3]] <- paste0(getwd(), "/inc/example_data_folder_for_VizR/COVID19patients_KEGG-WkPt-RctP-BlgP.csv")
  rv$columnCount <- readRDS(paste0(getwd(), "/rvs/columnCount.rds"))
  rv$upload_batch_columns <- readRDS(paste0(getwd(), "/rvs/upload_batch_columns.rds"))
  #rv$FileDF <- readRDS(paste0(getwd(), "/rvs/FileDf.rds"))
  rv$ll <- readRDS(paste0(getwd(), "/rvs/ll.rds"))
  
  #updateCheckboxGroupInput(session, inputId = "heatmap_dfs", selected = list("A549-ACE2_SARS-CoV-2_HiMOI_KEGG-WkPt-RctP-BlgP"))
  
  rv$gg <- readRDS(paste0(getwd(), "/rvs/gg.rds"))
  rv$tt <- readRDS(paste0(getwd(), "/rvs/tt.rds"))
  rv$upload_columns <- readRDS(paste0(getwd(), "/rvs/upload_columns.rds"))
  rv$n_sharedcols <- readRDS(paste0(getwd(), "/rvs/n_sharedcols.rds"))
  rv$n_sharedrows <- readRDS(paste0(getwd(), "/rvs/n_sharedrows.rds"))
  rv$heatmap_i <- readRDS(paste0(getwd(), "/rvs/heatmap_i.rds"))
  
  rv$df_n <- readRDS(paste0(getwd(), "/rvs/df_n.rds"))
  rv$nic <- readRDS(paste0(getwd(), "/rvs/nic.rds"))
  rv$gls_ui <- readRDS(paste0(getwd(), "/rvs/gls_ui.rds"))
  rv$gls_text <- readRDS(paste0(getwd(), "/rvs/gls_text.rds"))
  rv$n_css_highlights <- readRDS(paste0(getwd(), "/rvs/n_css_highlights.rds"))
  rv$nx_i <- readRDS(paste0(getwd(), "/rvs/nx_i.rds"))
  rv$hm_numeric_stats <- readRDS(paste0(getwd(), "/rvs/hm_numeric_stats.rds"))
  rv$all_char_stats <- readRDS(paste0(getwd(), "/rvs/all_char_stats.rds"))
  rv$nw_char_stats <- readRDS(paste0(getwd(), "/rvs/nw_char_stats.rds"))
  rv$n_ui_showpanel <- readRDS(paste0(getwd(), "/rvs/n_ui_showpanel.rds"))
  rv$n_igl <- readRDS(paste0(getwd(), "/rvs/n_igl.rds"))
  rv$ins_criteria <- readRDS(paste0(getwd(), "/rvs/ins_criteria.rds"))
  rv$n_venn_status <- readRDS(paste0(getwd(), "/rvs/n_venn_status.rds"))
  rv$n_3ds_status <- readRDS(paste0(getwd(), "/rvs/n_3ds_status.rds"))
  rv$s <- readRDS(paste0(getwd(), "/rvs/s.rds"))
  rv$v <- readRDS(paste0(getwd(), "/rvs/v.rds"))
  rv$n_to_plot <- readRDS(paste0(getwd(), "/rvs/n_to_plot.rds"))
  rv$heatmap_sortby <- readRDS(paste0(getwd(), "/rvs/heatmap_sortby.rds"))
  rv$ins_venn_palette <- readRDS(paste0(getwd(),"/rvs/ins_venn_palette.rds"))
  rv$detected_dbs <- readRDS(paste0(getwd(),"/rvs/detected_dbs.rds"))
  rv$opt_easygsea_filter_db <- readRDS(paste0(getwd(),"/rvs/opt_easygsea_filter_db.rds"))
  rv$n_ins_namelen <- readRDS(paste0(getwd(), "/rvs/n_ins_namelen.rds"))
  for (i in 1:3){
    rv[[paste0("nic_p_",i)]] <- 0.05
    rv[[paste0("nic_q_",i)]] <- 1.1
    rv[[paste0("nic_Stat_",i)]] <- -0.1
    rv[[paste0("nic_sign_",i)]] <- "All"
  }
  
  refresh_vis_ui()
  js$collapse("select_n_panel")
  if(is.null(input$f_global_iscollapsed)==T){ # uncollapse this box
    js$collapse("f_global")
  } else if (input$f_global_iscollapsed==T){
    js$collapse("f_global")
  }
}

# the function to remove initialized RVs from the demo session
init_demo_d <- function(){
  
  rv$ll <- NULL
  rv$gg <- NULL
  rv$tt <- NULL
  
  variable_list <- c("all_char_stats", "batch_failed", "batch_files", "columnCount", "detected_dbs",
                     "df_n", "FileDf", "folder_upload_state", "gg", "gls_text",
                     "gls_ui", "heatmap_i", "heatmap_sortby", "hm_numeric_stats",
                     "ins_criteria", "ins_venn_palette", "ll", "n_3ds_status", "n_css_highlights",
                     "n_ins_view", "n_sharedcols", "n_sharedrows", "n_to_plot", "n_ui_showpanel",
                     "n_venn_status", "nic", "nw_char_stats", "nx_i", "nx_n", "opt_easygsea_filter_db",
                     "s", "tt", "upload_batch_colscheme", "upload_batch_columns", "upload_batch_sharedcols",
                     "upload_columns", "v")
  for(i in seq_along(variable_list)){
    rv[[variable_list[i]]] <- NULL
  }
}

# =============== demo toggle button ===============
btn_demo <- function(id){
  if(rv$demo_n %% 2 == 1){
    label = "Example Run"
    icon = "play"
    color = "success"
    style = "bordered"
    size = "xs"
  }else{
    label = "Unload Example"
    icon = "trash-alt"
    color = "default"
    style = "minimal"
    size = "sm"
    
  }
  
  fixedPanel(
    bottom = 50,
    actionBttn(id,label
               ,block = TRUE
               ,style = style
               ,color = color
               ,size = size
               ,icon = icon(icon)
    )
    
  )
}

btn_demo_e <- function(){
  withProgress(message = 'Updating session ...',
               value = 1,{
                 rv$demo_n = rv$demo_n + 1
                 if(rv$demo_n %% 2 == 1){
                   init_demo_d()
                 }else{
                   init_demo()
                 }
               })
}
