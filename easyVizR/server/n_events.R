#======================================================================#
####                      UPDATE VARIABLES                          ####
#======================================================================#
# update these into rv when selections change

observeEvent(input$n_to_plot,{rv$n_to_plot<-input$n_to_plot})
observeEvent(input$heatmap_sortby,{rv$heatmap_sortby<-input$heatmap_sortby})

observe({
  req(is.null(rv$nx_n)==F)
  
  if(is.null(input$n_ui_showpanel)==F){ rv$n_ui_showpanel <- input$n_ui_showpanel }
  
  for (i in 1:length(rv$nx_n)){
    if(is.null(input[[paste0("nic_p_",i)]])==F){ rv[[paste0("nic_p_",i)]] <- input[[paste0("nic_p_",i)]] }
    if(is.null(input[[paste0("nic_q_",i)]])==F){ rv[[paste0("nic_q_",i)]] <- input[[paste0("nic_q_",i)]] }
    if(is.null(input[[paste0("nic_Stat_",i)]])==F){ rv[[paste0("nic_Stat_",i)]] <- input[[paste0("nic_Stat_",i)]] }
    if(is.null(input[[paste0("nic_sign_",i)]])==F){ rv[[paste0("nic_sign_",i)]] <- input[[paste0("nic_sign_",i)]] }
    if(is.null(input[[paste0("nic_apply_",i)]])==F){ rv[[paste0("nic_apply_",i)]] <- input[[paste0("nic_apply_",i)]] }
    if(is.null(input[[paste0("nic_na_",i)]])==F){ rv[[paste0("nic_na_",i)]] <- input[[paste0("nic_na_",i)]] }
  }
  
  
  if(is.null(input$n_ins_view)==F){ rv$n_ins_view <- input$n_ins_view }
  if(is.null(input$n_venn_label)==F){ rv$n_venn_label <- input$n_venn_label }
  if(is.null(input$n_upset_sortby)==F){ rv$n_upset_sortby <- input$n_upset_sortby }
  if(is.null(input$n_upset_showempty)==F){ rv$n_upset_showempty <- input$n_upset_showempty }
  
  
  
  if(is.null(input$nxy_selected_x)==F){ rv$nxy_selected_x <- input$nxy_selected_x }
  if(is.null(input$nxy_selected_y)==F){ rv$nxy_selected_y <- input$nxy_selected_y }
  if(is.null(input$nxy_selected_z)==F){ rv$nxy_selected_z <- input$nxy_selected_z }
  if(is.null(input$nxy_colormode)==F){rv$nxy_colormode <- input$nxy_colormode}
  if(is.null(input$nxy_sig)==F){rv$nxy_sig <- input$nxy_sig}
  if(is.null(input$nxy_thresh)==F){rv$nxy_thresh <- input$nxy_thresh}
  if(is.null(input$nxy_sc_size)==F){rv$nxy_sc_size <- input$nxy_sc_size}
  if(is.null(input$n_sc_logic)==F){rv$n_sc_logic <- input$n_sc_logic}
  
  if(is.null(input$nxyz_sc_logic)==F){rv$n_sc_logic <- input$nxyz_sc_logic} # put xyz logic into xy
  if(is.null(input$n_3ds_p)==F){ rv$n_3ds_p <- input$n_3ds_p }
  if(is.null(input$n_3ds_q)==F){ rv$n_3ds_q <- input$n_3ds_q }
  if(is.null(input$n_3ds_Stat)==F){ rv$n_3ds_Stat <- input$n_3ds_Stat }
  
  # if(is.null(input$n_igl)==F){ rv$n_igl <- input$n_igl }
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
    rv$iso_sharedcols<- isolate(rv$n_sharedcols) # shared cols
    
    # current panel
    rv$n_ui_showpanel <- "Heatmap"
    
    # input genelist
    rv$n_igl <- ""

    # initialize filters
    for (i in 1:length(rv$nx_n)){
      rv[[paste0("nic_p_",i)]] <- 0.05
      rv[[paste0("nic_q_",i)]] <- 1
      rv[[paste0("nic_Stat_",i)]] <- 0
      rv[[paste0("nic_sign_",i)]] <- "All"
      rv[[paste0("nic_apply_",i)]] <- T
      rv[[paste0("nic_na_",i)]] <- T
    }
    
    
    
    # heatmap options
    rv$n_to_plot <- "Stat"
    rv$heatmap_sortby <- isolate(input$heatmap_dfs[[1]])
    

    
    # intersection options
    rv$n_ins_view == "Full"
    # venn
    rv$n_venn_label <- "counts"
    
    # upset
    rv$n_upset_sortby <- "freq"
    rv$n_upset_showempty <- FALSE
    

    
    # scatter options
    
    # 2d scatter
    rv$nxy_selected_x <- rv$nx_n[[1]]
    rv$nxy_selected_y <- rv$nx_n[[2]]
    rv$nxy_selected_z <- "None"
    rv$nxy_colormode <- "None"
    rv$nxy_sig <- "PValue"
    rv$nxy_thresh <- 0.01
    rv$nxy_sc_size <- 3
    rv$n_sc_logic <- "Both"
    
    # 3d scatter
    rv$nxyz_sc_logic <- "Both"
    rv$n_3ds_p <- 0.05
    rv$n_3ds_q <- 1
    rv$n_3ds_Stat <- 0
    
    
    if (length(rv$nx_i) <= 5){rv$n_venn_status <- "ok"}
    else{ rv$n_venn_status <- "no" }
    if (length(rv$nx_i) == 3){rv$n_3ds_status <- "ok"}
    else{ rv$n_3ds_status <- "no" }
    
    # initialize dynamic ui
    rv$s <- vector(mode="list", length=length(rv$nx_i))
    rv$nic <- vector(mode="list", length=length(rv$nx_i))
    rv$v <- vector(mode="list", length=length(rv$nx_i))
    
    incProgress(0.2)
    # print(tt)
    
    
  })
  rv$df_n <- df_n
  
  # # find max stat and generate scale
  # statmax <- max(dplyr::select(df_n, contains("Stat_")), na.rm=TRUE)
  # rv$n_stat_scale <- round(generate_scale(statmax, 10),2)
  
  shinyjs::enable("n_use_data")
})



# update output variables (must be here!!)
output$n_venn_status <- reactive(rv$n_venn_status)
outputOptions(output, "n_venn_status", suspendWhenHidden = F)
output$n_3ds_status <- reactive(rv$n_3ds_status)
outputOptions(output, "n_3ds_status", suspendWhenHidden = F)




####-------------------- Process and filter data ------------------------####

# 1. cut first by input genelist (if any)
n_basic_igl <- reactive({
  df <- rv$df_n
  if (nchar(rv$n_igl)>0){
    igl <- isolate(as.list(strsplit(rv$n_igl, '\\n+')))
    # print(igl)
    df <- df[df$Name %in% igl[[1]],]
    df <- df[order(match(df$Name, igl[[1]])), ]
  }
  return(df)
})

# report genes that are not found
n_basic_igl_nm <- reactive({
  if (nchar(rv$n_igl)>0){
    igl <- isolate(as.list(strsplit(rv$n_igl, '\\n+')))
    notfound <- setdiff(igl[[1]], n_basic_igl()$Name)
  }
  else{notfound=vector()}
  return(notfound)
})
output$n_igl_nm <- renderUI({
  req(length(n_basic_igl_nm())>0)
  nl <- paste(n_basic_igl_nm(), collapse=", ")
  box(width=12,
      shiny::HTML(paste0("<strong>Not found</strong> (",length(n_basic_igl_nm()),"): ", nl)),
  )
})
observeEvent(input$n_igl_update,{
  rv$n_igl <- input$n_igl
})
observeEvent(input$n_igl_reset,{
  updateTextAreaInput(session, "n_igl", value="")
  rv$n_igl <- ""
})

# 2. apply cutoffs to the master df
n_basic_df <- reactive({
  req(nrow(rv$df_n)>0)
  req(length(rv$s)>0)
  req(length(rv$s)==length(rv$nx_i))
  
  df <- n_basic_igl()
  
  # loop filters over every dataset
  for (i in 1:length(rv$nx_n)){
    req(rv[[paste0("nic_p_",i)]])
    req(rv[[paste0("nic_q_",i)]])
    req(rv[[paste0("nic_Stat_",i)]])
    req(rv[[paste0("nic_sign_",i)]])
    req(is.null(rv[[paste0("nic_na_",i)]])==F)
    req(is.null(rv[[paste0("nic_apply_",i)]])==F)
    
    
    tol = rv[[paste0("nic_na_",i)]]
    apply = rv[[paste0("nic_apply_",i)]]
    
    if (apply==T){
      # get the col names
      n <- rv$nx_n[[i]]
      statn <- paste0("Stat_", n)
      pn <- paste0("PValue","_", n)
      qn <- paste0("FDR","_", n)
      
      # filter by sign
      df <- filter_by_sign(df, statn, 
                           rv[[paste0("nic_sign_",i)]], 
                           tolerate=tol)
      # filter by cutoffs
      df <- apply_single_cutoff(df, n, 
                                rv[[paste0("nic_p_",i)]],
                                rv[[paste0("nic_q_",i)]], 
                                rv[[paste0("nic_Stat_",i)]], 
                                tolerate=tol)
      
    }
  }
  return(df)
  
})





####### -------------- Processing for all intersection related analysis. ---------------

# 1. generate gene lists (gls) according to individual cutoffs
n_ins_gls <- reactive({
  # req(length(rv$nx_n)<=5) # too many datasets may eat up memory
  req(nrow(rv$df_n)>0)
  req(length(rv$s)>0)
  req(length(rv$s)==length(rv$nx_i))
  
  df <- rv$df_n
  
  gls <- vector(mode="list") # initialize gls as empty list
  
  
  # using advanced settings
  for (i in 1:length(rv$nx_n)){
    req(rv[[paste0("nic_p_",i)]])
    req(rv[[paste0("nic_q_",i)]])
    req(rv[[paste0("nic_Stat_",i)]])
    req(rv[[paste0("nic_sign_",i)]])
    
    n <- rv$nx_n[[i]]
    ss <- df
    ss <- ss[ss[[paste0("PValue","_", n)]]<=rv[[paste0("nic_p_",i)]], ] # filter by p
    ss <- ss[ss[[paste0("FDR","_", n)]]<=rv[[paste0("nic_q_",i)]], ] # filter by q
    ss <- ss[abs(ss[[paste0("Stat","_", n)]])>=rv[[paste0("nic_Stat_",i)]], ] # filter by stat
    ss <- filter_by_sign(ss, paste0("Stat","_", n), rv[[paste0("nic_sign_",i)]], tolerate=T) # filter by stat sign
    
    gl <- as.character(na.omit(ss$Name)) # format
    gls[[n]] <- gl # write into list as named vector
  }
  
  return(gls)
})


# 2. turns gls into matrix
n_ins_glm <- reactive({
  req(nrow(rv$df_n)>0)
  req(length(n_ins_gls())>0)
  
  df <- rv$df_n
  
  gls <- n_ins_gls()
  xx <- lapply(seq_along(names(gls)),function(x){
    unique(unlist(gls)) %in% gls[[names(gls)[[x]]]]
  })
  names(xx) <- names(gls)
  
  # turn into df
  glm <- data.frame(xx, row.names = unique(unlist(gls)))
  
  glm
})

# 3. filters gls matrix by criteria
n_ins_ss <- reactive({ 
  req(nrow(rv$df_n)>0)
  req(nrow(n_ins_glm())>0)
  
  df <- rv$df_n
  
  
  glm <- n_ins_glm()
  
  # get subset of genes based on t/f table
  subset <- glm[apply(glm,1,function(x) {
    match_skipna(x,rv$ins_criteria)
  }),] 
  subset
  
})

# 4. turns ss into filtered gl
n_ins_fgl <- reactive({
  subset <- n_ins_ss()
  genelist <- rownames(subset) # these are gene list
  genelist
  
})

# 5. subsets full df based on ss and filtered gl
n_ins_df <- reactive({
  req(length(rv$ins_criteria)>0)
  req(length(rv$ins_criteria)==length(rv$nx_i))
  
  df <- rv$df_n # full df to subset
  genelist <- n_ins_fgl() # list of genes to show in table
  
  if(rv$n_ins_view == "Full"){
    df <- df[df$Name %in% genelist,] # extract the rows from full df
  }
  else if (rv$n_ins_view == "Minimized"){
    xx <- dplyr::select(df, contains(c("Name","Stat", rv$n_ins_plot)))
    print(head(xx))
    df <- xx[xx$Name %in% genelist,]
  }
  else if (rv$n_ins_view == "T/F Matrix"){
    df <- as.data.frame(n_ins_ss())
    
    # turn row names into identifying column
    df <- cbind(rownames(df),df)
    colnames(df)[[1]] <- "Name"
  }
  
  # tidy row names
  if (nrow(df)>0){rownames(df) <- seq(1,nrow(df),1)}
  
  # to replace the stat col names
  colnames(df) <- gsub("Stat", rv$tt[[rv$nx_i[[1]]]], colnames(df))
  
  df
})



####### -------------- Processing for all scatter related analysis. ---------------

# redo cutoffs using either or modes (this is substitute for n_basic_df())
n_nxy_df <- reactive({
  req(nrow(rv$df_n)>0)
  req(length(rv$s)>0)
  req(length(rv$s)==length(rv$nx_i))
  req(is.null(rv$nxy_selected_x)==F)
  req(is.null(rv$nxy_selected_y)==F)
  req(is.null(rv$nxy_selected_z)==F)
  req(is.null(rv$n_sc_logic)==F)
  
  df <- n_basic_igl()
  
  if (rv$nxy_selected_z=="None"){
    selected <- c(rv$nxy_selected_x, rv$nxy_selected_y)
  } else {
    selected <- c(rv$nxy_selected_x, rv$nxy_selected_y, rv$nxy_selected_z)
  }
  
  
  
  # cut out only selected datasets
  df <- dplyr::select(df, contains(c("Name", selected)))
  
  # get rid of rows with all NA
  df <- df[complete.cases(df[ , -1]),]
  
  
  
  cuts <- vector(mode="list", length=length(selected))
  for (n in 1:length(selected)){
    xi <- match(selected[[n]], rv$nx_n) # get the index from name
    
    # try to apply cutoff
    if (rv[[paste0("nic_apply_",xi)]]==T){
      x_filtered <- apply_single_cutoff(df, selected[[n]], p=rv[[paste0("nic_p_",xi)]], q=rv[[paste0("nic_q_",xi)]], stat=rv[[paste0("nic_Stat_",xi)]], 
                                        tolerate=rv[[paste0("nic_na_",xi)]])
      x_filtered <- filter_by_sign(x_filtered, paste0("Stat_",selected[[n]]), 
                                   rv[[paste0("nic_sign_",xi)]], 
                                   tolerate=rv[[paste0("nic_na_",xi)]])
    } else {
      x_filtered <- df
    }
    
    cuts[[n]] <- x_filtered$Name
  }
  
  if (rv$n_sc_logic == "Both"){
    df <- df[df$Name %in% Reduce(intersect, cuts), ]
  } else if (rv$n_sc_logic == "Either"){
    df <- df[df$Name %in% Reduce(union, cuts), ]
  }

  
  df
  
})