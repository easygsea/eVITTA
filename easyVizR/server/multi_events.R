#======================================================================#
####                      UPDATE VARIABLES                          ####
#======================================================================#
# update these into rv when selections change
observe({
  req(is.null(rv$nx_n)==F)
  
  # vis panel selection
  input2rv("n_ui_showpanel")
  
  # ins table settings
  input2rv(c("n_ins_view"))

  # heatmap
  input2rv(c(
    "n_to_plot",
    "heatmap_sortby", "n_hm_ylabs"
    ))
  
  # venn and upset
  input2rv(c(
    "n_venn_label","n_venn_type","n_venn_show_ins","ins_venn_c1",
    "n_upset_sortby","n_upset_showempty","ins_venn_palette",
    "n_upset_show_ins", "n_upset_c1"
    ))
  
  # 2D scatter
  input2rv(c(
    "nxy_selected_x","nxy_selected_y","nxy_selected_z",
    "nxy_colormode","nxy_sig","nxy_thresh","n_sc_logic",
    "nxy_sc_size","nxy_sc_opacity","nxy_sc_outlinewidth", "nxy_sc_outlinecolor",
    "nxy_p", "nxy_q", "nxy_stat",
    "nxy_sc_plotmode", "nxy_sc_dflogic"
  ))
  
  # 3D scatter
  input2rv(c(
    "nxyz_colormode","nxyz_sc_logic",
    "n_3ds_p","n_3ds_q","n_3ds_Stat",
    "nxyz_sc_size", "nxyz_sc_opacity", "nxyz_sc_outlinewidth", "nxyz_sc_outlinecolor",
    "nxyz_sc_plotmode","nxyz_sc_dflogic"
  ))
  
  # single volcano
  input2rv(c(
    "nx_vol_plotmode","nx_selected",
    "nx_p","nx_Stat",
    "nx_vol_c1","nx_vol_c2","nx_vol_c3"
  ))
  
  # single bar
  input2rv(c(
    "nx_bar_sig","nx_bar_to_plot"
  ))
  
  
  # network
  input2rv(c(
    "nw_selected_n","nw_le_sep","nw_selected_col",
    "p_or_q_vis","vis_percent","vis_percent_cutoff","combined_k"
  ))
  
})


#======================================================================#
####                     PRE-VIS PROCESSING                         ####
#======================================================================#
# rv$heatmap_i : vector, indices of selections in rv$ll
# rv$n_sharedcols : numeric
# rv$n_sharedrows : numeric


# observe and match selected df index into new list
observeEvent(input$heatmap_dfs,{
  rv$heatmap_i <- input$heatmap_dfs
  rv$heatmap_i <- lapply(rv$heatmap_i,function(x){match(x,rv$ll)})
})

# observe and pull out shared cols and rows among input dfs (as lists)
observe({
  if (length(rv$heatmap_i)>=2){
    try({
      observed <- detect_shared_dimensions(rv$heatmap_i, rv$gg, rv$ll, input_mode="indices")
      rv$n_sharedcols <- observed$shared_cols
      rv$n_sharedrows <- observed$shared_rows
    })
    
  }
  else{
    rv$n_sharedcols <- NULL
    rv$n_sharedrows <- NULL
  }
})


#======================================================================#
####                     POST-VIS PROCESSING                        ####
#======================================================================#


# observe selection and combine to data table
observeEvent(input$n_use_data,{
  shinyjs::disable("n_use_data")
  rv$df_n <- NULL # reset every time
  
  withProgress(message = 'Updating data...', value = 0, {
    
    df_n <- build_df_n(input$heatmap_dfs, rv$gg, rv$ll, input_mode = "names")
    
    incProgress(0.5)
    
    # ----------------- initialize params
    
    # currently selected datasets
    rv$nx_i <- isolate(rv$heatmap_i) # indices
    rv$nx_n <- isolate(input$heatmap_dfs) # names
    # rv$iso_sharedcols<- isolate(rv$n_sharedcols) # shared cols (used for hm)
    rv$n_sharedcols <- detect_shared_dimensions(rv$nx_i, rv$gg, rv$ll, input_mode="indices")$shared_cols
    rv$hm_numeric_stats <- get_cols_by_class(df_n, is.numeric, output_type="statnames")
    rv$all_char_stats <- get_cols_by_class(df_n, is.character, output_type="statnames")
    rv$nw_char_stats <- rv$all_char_stats[-which(rv$all_char_stats %in% c("Name"))] # without name col
    
    # ---------------  current panel
    rv$n_ui_showpanel <- "Heatmap"
    
    # ---------------  input genelist
    rv$n_igl <- ""

    # ---------------  initialize filters
    for (i in 1:length(rv$nx_n)){
      rv[[paste0("nic_p_",i)]] <- 0.05
      rv[[paste0("nic_q_",i)]] <- 1
      rv[[paste0("nic_Stat_",i)]] <- 0
      rv[[paste0("nic_sign_",i)]] <- "All"
    }
    
    
    #heatmap_sortby_initializer = rv$nx_n[[1]]
    #isolate({ heatmap_sortby_initializer<-rv$nx_n[[1]] })
    # ---------------  heatmap options
    rv$n_to_plot <- "Stat"
    rv$heatmap_sortby <- rv$nx_n[[1]]
    #rv$heatmap_sortby <- heatmap_sortby_initializer
    rv$n_hm_ylabs <- F

    
    # ---------------  intersection options
    rv$n_ins_view == "Full"
    # venn
    rv$n_venn_label <- "counts"
    rv$n_venn_type <- "Basic"
    rv$n_venn_show_ins <- T

    rv$ins_venn_c1 <- "gold"
    rv$ins_venn_palette <- "white"

    # upset
    rv$n_upset_sortby <- "freq"
    rv$n_upset_showempty <- FALSE
    rv$n_upset_show_ins <- T
    rv$n_upset_c1 <- "gold"
    
    
    
    
    # --------------- scatter options
    
    # 2d scatter
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
    
    # 3d scatter
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
    
    
    # --------------- single options
    rv$nx_vol_plotmode <- "Focus"
    rv$nx_selected <- rv$nx_n[[1]]
    rv$nx_p <- 0.05
    rv$nx_Stat <- 0
    rv$nx_vol_c1 <- "red"
    rv$nx_vol_c2 <- "black"
    rv$nx_vol_c3 <- "gray"
    
    rv$nx_bar_sig <- "PValue"
    rv$nx_bar_to_plot <- "Stat"
    
    
    # ---------------  network options
    rv$nw_selected_n <- rv$nx_n[[1]]
    rv$nw_le_sep <- ";"
    rv$nw_selected_col <- firstmatch(le_alias,rv$nw_char_stats)
    # rv$cutoff_vis_p <- 0.05
    # rv$cutoff_vis_q <- 1
    rv$p_or_q_vis <- "PValue"
    rv$vis_percent <- "jaccard"
    rv$vis_percent_cutoff <- 0.25
    rv$combined_k <- 0.5
    
    
    #  --------------- initialize intersection
    rv$ins_criteria <- rep(T,length(rv$nx_n))
    names(rv$ins_criteria) <- rv$nx_n
    
    
    #  --------------- initialize dynamic ui
    
    if (length(rv$nx_i) <= 5){rv$n_venn_status <- "ok"}
    else{ rv$n_venn_status <- "no" }
    if (length(rv$nx_i) == 3){rv$n_3ds_status <- "ok"}
    else{ rv$n_3ds_status <- "no" }
    
    rv$s <- vector(mode="list", length=length(rv$nx_i))
    rv$nic <- vector(mode="list", length=length(rv$nx_i))
    rv$v <- vector(mode="list", length=length(rv$nx_i))
    rv$gls_ui <- vector(mode="list", length=length(rv$nx_i))
    
    # saveRDS(rv$s, file = "rvs/s.rds")
    # saveRDS(rv$v, file = "rvs/v.rds")
    # 
    incProgress(0.2)
    # print(tt)
    
    
  })
  rv$df_n <- df_n
  
  # manually reload the UI
  remove_ui("venn_dropdowns")
  remove_ui("upset_dropdowns")
  # tab 4 elements
  remove_ui("select_graph_to_display")
  remove_ui("heatmap_dropdowns")
  remove_ui("scatter_selection")
  remove_ui("scatter_3d_dropdowns")
  remove_ui("scatter_2d_dropdowns")
  remove_ui("single_dropdowns")
  remove_ui("single_selections")
  remove_ui("nx_bar_panel_dropdowns")
  remove_ui("network_selection")
  remove_ui("dataset_selection")
  remove_ui("network_dropdowns ")
  
  
  
  
  
  
  
  # # find max stat and generate scale
  # statmax <- max(dplyr::select(df_n, contains("Stat_")), na.rm=TRUE)
  # rv$n_stat_scale <- round(generate_scale(statmax, 10),2)

  
  js$collapse("select_n_panel")
  if(is.null(input$f_global_iscollapsed)==T){ # uncollapse this box
    js$collapse("f_global")
  } else if (input$f_global_iscollapsed==T){
    js$collapse("f_global")
  }

  # saveRDS(rv$gls_ui, file = "rvs/gls_ui.rds")
  # saveRDS(rv$gls_text, file = "rvs/gls_text.rds")
  # saveRDS(rv$n_css_highlights, file = "rvs/n_css_highlights.rds")
  # saveRDS(rv$nx_i, file = "rvs/nx_i.rds")
  # saveRDS(rv$hm_numeric_stats, file = "rvs/hm_numeric_stats.rds")
  # saveRDS(rv$all_char_stats, file = "rvs/all_char_stats.rds")
  # saveRDS(rv$nw_char_stats, file = "rvs/nw_char_stats.rds")
  # saveRDS(rv$n_ui_showpanel, file = "rvs/n_ui_showpanel.rds")
  # saveRDS(rv$n_igl, file = "rvs/n_igl.rds")
  # saveRDS(rv$ins_criteria, file = "rvs/ins_criteria.rds")
  # saveRDS(rv$n_venn_status, file = "rvs/n_venn_status.rds")
  # saveRDS(rv$n_3ds_status, file = "rvs/n_3ds_status.rds")
  # saveRDS(rv$s, file = "rvs/s.rds")
  # saveRDS(rv$n_ins_view, file = "rvs/n_ins_view.rds")
  # 
  # saveRDS(rv$nx_n, file = "rvs/nx_n.rds")
  # saveRDS(rv$df_n, file = "rvs/df_n.rds")
  # saveRDS(rv$nic, file = "rvs/nic.rds")
  # 
  shinyjs::enable("n_use_data")
})



# update output variables (must be here!!)
output$n_venn_status <- reactive(rv$n_venn_status)
outputOptions(output, "n_venn_status", suspendWhenHidden = F)
output$n_3ds_status <- reactive(rv$n_3ds_status)
outputOptions(output, "n_3ds_status", suspendWhenHidden = F)




####-------------------- Process and filter data ------------------------####

# 0. cut first by input genelist (if any); 
# if no gene list is found, return the full df.

df_n_basic <- reactive({
  df <- rv$df_n
  if (nchar(rv$n_igl)>0){
    igl <- isolate(as.list(strsplit(toupper(rv$n_igl), '\\n+')))[[1]]
    print(igl)
    df <- df[df$Name %in% igl,]
    df <- df[order(match(df$Name, igl)), ]
  }
  return(df)
  print(df)
})





# 1. generate gene lists (gls) according to individual cutoffs
n_ins_gls <- reactive({
  req(length(rv$s)>0)
  req(length(rv$s)==length(rv$nx_i)) # make sure selections are fully rendered
  filter_to_gls("nic", rv, df_n_basic())
})


# 2. find intersection according to gene lists and criteria, and output as df
n_ins_full <- reactive({
  
  out <- extract_intersection(gls = n_ins_gls(), 
                              criteria = rv$ins_criteria, 
                              df = df_n_basic(), 
                              out_type = "Full",
                              include_background = T)
  
  out
})



# 5b. renders the df to show as datatable
n_ins_df <- reactive({
  req(length(rv$ins_criteria)>0)
  req(length(rv$ins_criteria)==length(rv$nx_i))

  df <- extract_intersection(gls = n_ins_gls(), 
                              criteria = rv$ins_criteria, 
                              df = df_n_basic(), 
                              out_type = rv$n_ins_view,
                              include_background = T)
  
  
  # tidy row names
  if (nrow(df)>0){rownames(df) <- seq(1,nrow(df),1)}
  
  # to replace colnames
  colnames(df) <- stat_replace2(colnames(df))
  
  
  df
})



# ####### -------------- Processing for all scatter related analysis. ---------------
# 
# # redo cutoffs using either or modes (this is substitute for n_basic_df())
# n_nxy_df <- reactive({
#   req(nrow(rv$df_n)>0)
#   req(length(rv$s)>0)
#   req(length(rv$s)==length(rv$nx_i))
#   req(is.null(rv$nxy_selected_x)==F)
#   req(is.null(rv$nxy_selected_y)==F)
#   req(is.null(rv$nxy_selected_z)==F)
#   req(is.null(rv$n_sc_logic)==F)
#   
#   df <- df_n_basic()
#   
#   if (rv$nxy_selected_z=="None"){
#     selected <- c(rv$nxy_selected_x, rv$nxy_selected_y)
#   } else {
#     selected <- c(rv$nxy_selected_x, rv$nxy_selected_y, rv$nxy_selected_z)
#   }
#   
#   
#   
#   # cut out only selected datasets
#   df <- dplyr::select(df, contains(c("Name", selected)))
#   
#   # get rid of rows with all NA
#   df <- df[complete.cases(df[ , -1]),]
#   
#   
#   
#   cuts <- vector(mode="list", length=length(selected))
#   for (n in 1:length(selected)){
#     xi <- match(selected[[n]], rv$nx_n) # get the index from name
#     
#     # try to apply cutoff
#     if (rv[[paste0("nic_apply_",xi)]]==T){
#       x_filtered <- apply_single_cutoff(df, selected[[n]], p=rv[[paste0("nic_p_",xi)]], q=rv[[paste0("nic_q_",xi)]], stat=rv[[paste0("nic_Stat_",xi)]], 
#                                         tolerate=rv[[paste0("nic_na_",xi)]])
#       x_filtered <- filter_by_sign(x_filtered, paste0("Stat_",selected[[n]]), 
#                                    rv[[paste0("nic_sign_",xi)]], 
#                                    tolerate=rv[[paste0("nic_na_",xi)]])
#     } else {
#       x_filtered <- df
#     }
#     
#     cuts[[n]] <- x_filtered$Name
#   }
#   
#   if (rv$n_sc_logic == "Both"){
#     df <- df[df$Name %in% Reduce(intersect, cuts), ]
#   } else if (rv$n_sc_logic == "Either"){
#     df <- df[df$Name %in% Reduce(union, cuts), ]
#   }
# 
#   
#   df
#   
# })