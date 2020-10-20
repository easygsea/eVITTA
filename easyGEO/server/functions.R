# ------- Function to draw an info box to guide the user along the pipeline ---------
# You can pass html string into msg, e.g. : guide_box("<strong>This is a bold message</strong>")
# default color is blue
# default width is 12 (maximum), must be is an integer value
# To make it appear on condition, call it in a conditional renderUI({})
# Then, observeEvent to next tab:
# observeEvent(input$guide1,{updateTabItems(session, "menu1", "tab3")})

guide_box <- function(id,msg, color="warning", size="sm"){
  actionBttn(
    id,
    HTML(msg),
    icon=icon("angle-double-right"),
    style = "simple", color=color, size = size
    , block = T
  )
}


# ------------- enhanced page progress infobox ------------------
# call in renderUI on the server side. you need to wrap this in a box
# 
# id: id of the list display (<ul>)
# prompt: shown in bold 
# msg: a vector of strings
# conditions: a vector of conditionals (T = condition fulfilled). should be SAME length as msg. 
# button renders only when all conditions are fulfilled.
# bttn_id: id of the button (use in observeEvent to jump to next tab)
# bttn_text: display text of the button
#
# example: 
# progress_box(id="infobox_1", prompt="To-dos:",
# msg=c("1. Search a valid GSE number", "2. Select a platform", "3. Read the study information", "4. This is a sample message", "This is another sample message"), 
# condition=c(!is.null(rv$gse_all), !is.null(rv$plat_id), !is.null(rv$plat_id), !is.null(rv$plat_id),!is.null(rv$plat_id)),
# bttn_id="next_p1", bttn_text="Continue to next panel"
# )

progress_box <- function(id, prompt, msg, condition, bttn_id, bttn_text="Continue to next panel"){
  
  icon <- vector(mode="list", length=length(msg))
  for(i in 1:length(msg)){
    # get icon
    if (condition[[i]]==T){
      icon[[i]] <- "<i class='fa fa-check' style='color:green;'></i>"
      msg[[i]] <- paste0("<span style='color:gray;'><strike>",msg[[i]],"</strike></span>")
    } else {
      icon[[i]] <- "<i class='fa fa-check' style='color:white;'></i>"
    }
  }
  display <- paste0("<li>", msg, icon, "</li>")
  display <- paste0(display, collapse="")
  if (all(condition)){
    bttn <- actionBttn(bttn_id, bttn_text, icon=icon("angle-double-right"), style="simple", color="primary", size="xs")
  } else {bttn <- ""}
  div(
    tags$head(tags$style(
      HTML(paste0("
        #",id," {margin-top:5px; margin-bottom:5px;padding-inline-start: 15px;display:inline;}
        ul#",id," li {display:inline; margin-right:30px;}
        ul#",id," i {margin-left:5px}"))
    )),
    HTML(paste0("<strong>",prompt,"</strong><ul id='",id,"'>", display,"
                  </ul>")),
    bttn
  )
}


# ------------- notification panel ------------------
panel_null <- function(text = "Data available upon selection of a platform."){
  box(
    title = span( icon("exclamation"), "Notification"), status = "warning", width=6,
    text
  )
}

# ------------- basic function to filter DEG table -------------------
filter_df <- function(df = rv$deg,q_cutoff=input$tl_q,logfc_cutoff=input$tl_logfc){
  # filter table according to q & logFC
  df %>%
    dplyr::filter(adj.P.Val < q_cutoff, abs(logFC)>=logfc_cutoff)
}

# mutate digits to 2 decimals
mutate_df <- function(df = filter_df()){
  genes = rownames(df)
  
  df = df %>% 
    dplyr::mutate_at(c("logFC","AveExpr","t","B"),function(x) round(x, digits = 1)) %>%
    dplyr::mutate_at(c("P.Value","adj.P.Val"),function(x) scientific(x, digits = 2))
  
  rownames(df) = genes
  
  return(df)
}

# input table for volcano plots
volcano_df <- function(df = rv$deg,q_cutoff=rv$plot_q,logfc_cutoff=rv$plot_logfc){
  # genes
  genes = rownames(df)
  
  # mutate 0 to a small value
  df = df %>%
    mutate_if(is.numeric,  ~replace(., . == 0, 0.0000000001))
  
  # threshold by q & logfc cutoffs
  threshold_OE <- df[["adj.P.Val"]] < q_cutoff & abs(df$logFC)>=logfc_cutoff
  df$threshold <- threshold_OE
  
  # add rownames
  rownames(df) = genes
  
  return(df)
}

# basic function to plot static volcano
volcano_basic <- function(df,q_cutoff,logfc_cutoff,text="no"){
  fig <- ggplot(df) +
    geom_point(aes(x=logFC,y=-log(.data[["adj.P.Val"]]),colour=threshold)) +
    scale_colour_manual(values = c("grey","red")) +
    xlab("logFC") + ylab(paste0("-log10(adj.P.Val)")) +
    geom_vline(xintercept=c(-logfc_cutoff,logfc_cutoff), linetype="dotted") +
    geom_hline(yintercept=-log(q_cutoff), linetype="dotted") +
    theme_minimal() +
    theme(legend.position="none",
          plot.title = element_text(size = rel(1.5), hjust = 0.5),
          axis.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.25))
    )
  
  if(text=="yes"){
    fig <- fig +
      geom_text_repel(data = df[which(df$threshold==TRUE),],size=5,
                      aes(x=logFC,y=-log(df[which(df$threshold==TRUE),][["adj.P.Val"]]),label=genelabels)
      )
  }
  
  return(fig)
}

# basic function to plot different modes of volcano
volcano_ggplot <- function(df=volcano_df(),q_cutoff=rv$plot_q,logfc_cutoff=rv$plot_logfc){
  # plot by threshold
  if(rv$plot_label == "threshold"){
    fig <- volcano_basic(df,q_cutoff,logfc_cutoff,text="no")
    
    rv$v_success = "yes"
    return(fig)
    
    # plot by top genes
  }else if(rv$plot_label == "top"){
    no_down = rv$volcano_down
    no_up = rv$volcano_up
    
    # order df by top down regulations
    df_ordered = df[order(df[["logFC"]],df[["adj.P.Val"]]),]
    y_genes = rownames(df_ordered)
    labels_down = rev(y_genes[1:no_down])
    
    # order df by top up regulations
    df_ordered = df[order(-df[["logFC"]],df[["adj.P.Val"]]),]
    y_genes = rownames(df_ordered)
    labels_up = y_genes[1:no_up]
    
    # calculate # not labeled
    no_unlabel = nrow(df_ordered) - no_down - no_up
    
    # create genelabels
    df_ordered$genelabels = c(labels_up,rep("",no_unlabel),labels_down)
    
    df_ordered$threshold = df_ordered$genelabels != ""
    
    fig <- volcano_basic(df_ordered,q_cutoff,logfc_cutoff,text="yes")
    
    rv$v_success = "yes"
    return(fig)
    
    # plot by manual selection of genes    
  }else if(rv$plot_label == "manual"){
    in_genes = rv$gene_lists_v
    
    if(is.null(in_genes)==F && length(in_genes)>0){
      # re-threshold
      threshold_OE = rownames(df) %in% in_genes
      df$threshold <- threshold_OE
      
      # add gene labels
      df$genelabels = ""
      df$genelabels[which(df$threshold==TRUE)] = rownames(df)[which(df$threshold==TRUE)]
      
      fig <- volcano_basic(df,q_cutoff,logfc_cutoff,text="yes")
      
      rv$v_success = "yes"
      return(fig)
    }
  }
  
}

volcano_plotly <- function(df=volcano_df(),q_cutoff=rv$plot_q,logfc_cutoff=rv$plot_logfc){
  
  fig <- ggplot(df) +
    geom_point(aes(x=logFC,y=-log(.data[["adj.P.Val"]]),colour=threshold,
                   text=paste0(
                     "<b>",rownames(df),"</b>\n",
                     "logFC=",signif(.data[["logFC"]],digits=3),"\n",
                     "adj.P.Val=",signif(.data[["adj.P.Val"]],digits=3)
                   ))) +
    scale_colour_manual(values = c("grey","red")) +
    xlab("logFC") + ylab(paste0("-log10(adj.P.Val)")) +
    geom_vline(xintercept=c(-logfc_cutoff,logfc_cutoff), linetype="dotted") +
    geom_hline(yintercept=-log(q_cutoff), linetype="dotted") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size = rel(1.5), hjust = 0.5),
          axis.title = element_text(size = rel(1.25)))
  
  fig <- ggplotly(fig,tooltip = "text")
  
  rv$v_success = "yes"
  return(fig)
}

# filtered DEG table for heatmaps
hm_df <- function(df = rv$deg,q_cutoff=rv$plot_q,logfc_cutoff=rv$plot_logfc){
  # filter table according to q & logFC
  df = df %>%
    dplyr::filter(adj.P.Val < q_cutoff, abs(logFC)>=logfc_cutoff)
  
  # genes
  genes = rownames(df)
  
  #  mutate 0 to a small value
  df = df %>%
    mutate_if(is.numeric,  ~replace(., . == 0, 0.0000000001))
  
  # add rownames
  rownames(df) = genes
  
  # order df according to logFC & FDR
  # df = df[order(-df[["logFC"]],df[["adj.P.Val"]]),] 
  df = df %>% dplyr::arrange(logFC)
  
  # original table if manual selection of genes
  if(rv$plot_label == "manual"){
    df = rv$deg %>% dplyr::arrange(logFC)
  }
  
  return(df)
}

# filtered count table for heatmaps
hm_count <- function(df = hm_df(),counts = rv$deg_counts){
  genes = rownames(df)
  counts = data.frame(counts[match(genes,rownames(counts)),],stringsAsFactors = F)
  
  if(rv$h_y_name == "title"){
    samples = colnames(counts) %>%
      translate_sample_names(.,  rv$pdata[c("title", "geo_accession")],  "title")
    
    colnames(counts) = samples
  }
  
  if(rv$plot_label_hm == "top"){
    # top up regulated genes
    genes_up = df %>% dplyr::arrange(desc(logFC)) %>% #df[order(-df[["logFC"]],df[["adj.P.Val"]]),]
      head(.,n=rv$volcano_up) %>%
      rownames(.)
    
    # top down regulated genes
    genes_down = df %>% dplyr::arrange(logFC) %>% #df[order(df[["logFC"]],df[["adj.P.Val"]]),]
      head(.,n=rv$volcano_down) %>%
      rownames(.)
    
    # combine genes
    genes = c(genes_up,genes_down)
    
    # filter counts
    counts = counts[match(genes,rownames(counts)),]
    
  }else if(rv$plot_label == "manual"){
    # filter counts
    counts = counts[match(rv$gene_lists_v,rownames(counts)),]
  }
  
  return(counts)
}

# function to plot heatmaps
hm_plot <- function(counts=hm_count(),df = hm_df()){
  if(is.null(counts) | nrow(counts)<1){
    return(NULL)
  }else{
    samples = colnames(counts)
    titlex = "Expression"
    
    # if applicable, log2 transform count matrix
    if(rv$h_log == "yes"){
      counts = log2(counts+1)
      
      titlex = "Log2(expression+1)"
    }
    
    # if applicable, z-score transform count matrix
    if(rv$h_zscore == "yes"){
      counts = t(apply(counts,1,scale))
      colnames(counts) = samples
      
      titlex = "Z-score-transformed log2(expression+1)"
      
    }
    
    # make matrix for plot
    dat <- expand.grid(y = rownames(counts), x = colnames(counts))
    dat$z <- unlist(as.data.frame(counts),recursive = T)
    
    # genes and their logFC & FDR info
    genes = rownames(counts)
    df = df[match(genes,rownames(df)),]
    logFCs = rep(signif(df[["logFC"]],digits = 3),ncol(counts))
    FDRs = rep(signif(df[["adj.P.Val"]],digits = 3),ncol(counts))
    
    # combine into text
    textx = paste0("logFC: ",logFCs,"<br>adj.P.Val: ",FDRs)
    
    
    fig <- plot_ly() %>%
      add_trace(data = dat, x = ~x, y = ~y, z = ~z, type = "heatmap",
                colorscale  = cscale_zscore,zauto = T, zmid= 0, colorbar = list(title = list(text=titlex, side = "right")),
                text = textx,
                hovertemplate = paste('Gene: <b>%{y}</b><br><br>',
                                      'Sample: %{x}<br>',
                                      'Value: %{z:.3f}<br><br>',
                                      '%{text}'
                )
      )
    
    fig <- fig %>% layout(
      xaxis = list(title = "", showticklabels = T),
      yaxis = list(title = "", showticklabels = F)
      # ,margin = list(l=200)
    )
    
    rv$h_success = "yes"
    return(fig)
    
  }
}

# data for violin/box plot
vb_data <- function(gene=rv$a_gene,counts = rv$deg_counts){
  counts = as.data.frame(counts) %>% dplyr::filter(rownames(counts)==rv$a_gene)
  
  # if applicable, log2 transform counts
  if(rv$a_log == "yes"){
    counts = log2(counts+1)
  }
  
  counts_c = counts %>% dplyr::select(one_of(rv$samples_c)) %>% unlist(.)
  counts_t = counts %>% dplyr::select(one_of(rv$samples_t)) %>% unlist(.)
  
  r1 <- data.frame(x=c(rep(rv$c_level,length(rv$samples_c))),y=counts_c);row.names(r1) <- NULL
  r2 <- data.frame(x=c(rep(rv$t_level,length(rv$samples_t))),y=counts_t);row.names(r2) <- NULL
  
  rr <- rbind(r1,r2)
  
  rr$x = factor(rr$x,levels=c(rv$c_level,rv$t_level))
  
  return(rr)
}

# violin plot
data_summary <- function(x,k=rv$a_k) {
  m <- mean(x)
  ymin <- m - k * sd(x)
  ymax <- m + k * sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

violin_plt <- function(y_label){
  rr=vb_data()
  
  p <- ggplot(rr,aes(x=x,y=y,color=x)) +
    geom_violin(trim=FALSE) +
    scale_color_manual(values=c("blue","orange")) +
    stat_summary(fun.data=data_summary,geom="pointrange", color="grey") +
    geom_jitter(height = 0, width = 0.1) +
    labs(title=rv$a_gene,y=y_label,x="") +
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(size = rel(1.5), hjust = 0.5),
          axis.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.5))
    )
  
  rv$a_success = "yes"
  return(p)
}

# box plot
box_plt <- function(y_label){
  rr=vb_data()
  
  p<-ggplot(rr,aes(x=x,y=y)) + 
    geom_boxplot(color=c("blue","orange")) +
    labs(title=rv$a_gene,y=y_label,x="") +
    theme_classic() +
    theme(legend.position="none",
          plot.title = element_text(size = rel(1.5), hjust = 0.5),
          axis.title = element_text(size = rel(1.5)),
          axis.text = element_text(size = rel(1.5))
    )
  
  rv$a_success = "yes"
  return(p)
  
  
}

# ================ initialize demo RVs =================
init_demo <- function(){
  # initialize all required rv for a demo run
  rv$demo_acc = "GSE147507"
  rv$gse_all = readRDS(paste0(getwd(),"/rvs/gse_all.rds"))
  rv$geo_accession <- "GSE147507"
  rv$platforms = readRDS(paste0(getwd(),"/rvs/platforms.rds"))
  rv$plat_id <- 1
  rv$gpl_summary <- readRDS(paste0(getwd(),"/rvs/gpl_summary.rds"))
  rv$gpl_choices <- readRDS(paste0(getwd(),"/rvs/gpl_choices.rds"))
  rv$dmdf <- readRDS(paste0(getwd(),"/rvs/dmdf.rds"))
  rv$all_samples <- readRDS(paste0(getwd(),"/rvs/all_samples.rds"))
  rv$samples <- readRDS(paste0(getwd(),"/rvs/samples.rds"))
  rv$pdata <- readRDS(paste0(getwd(),"/rvs/pdata.rds"))
  rv$fddf <- readRDS(paste0(getwd(),"/rvs/fddf.rds"))
  rv$sup_source <- readRDS(paste0(getwd(),"/rvs/sup_source.rds"))
  rv$suplist <- readRDS(paste0(getwd(),"/rvs/suplist.rds"))
  rv$deg <- readRDS(paste0(getwd(),"/rvs/deg.rds"))
  rv$deg_counts <- readRDS(paste0(getwd(),"/rvs/deg_counts.rds"))
  rv$c_var <- readRDS(paste0(getwd(),"/rvs/c_var.rds"))
  rv$c_level <- readRDS(paste0(getwd(),"/rvs/c_level.rds"))
  rv$t_level <- readRDS(paste0(getwd(),"/rvs/t_level.rds"))
  rv$samples_c <- readRDS(paste0(getwd(),"/rvs/samples_c.rds"))
  rv$samples_t <- readRDS(paste0(getwd(),"/rvs/samples_t.rds"))
  rv$deg_pdata <- readRDS(paste0(getwd(),"/rvs/deg_pdata.rds"))
  rv$gpl_tooltips <- readRDS(paste0(getwd(),"/rvs/gpl_tooltips.rds"))
  rv$text <- readRDS(paste0(getwd(),"/rvs/text.rds"))
  rv$matrix_ready <- readRDS(paste0(getwd(),"/rvs/matrix_ready.rds")) 
  rv$demo <- "yes"
}

# unload example
init_demo_d <- function(){
  # uninitialize all required rv for a demo run
  updateTextInput(session,"geo_accession",value="")
  rv$gse_all = NULL
  rv$geo_accession <- NULL
  rv$platforms = NULL
  rv$plat_id <- NULL
  rv$gpl_summary <- NULL
  rv$gpl_choices <- NULL
  rv$dmdf <- NULL
  rv$all_samples <- NULL
  rv$samples <- NULL
  rv$pdata <- NULL
  rv$fddf <- NULL
  rv$sup_source <- NULL
  rv$suplist <- NULL
  rv$deg <- NULL
  rv$deg_counts <- NULL
  rv$c_var <- NULL
  rv$c_level <- NULL
  rv$t_level <- NULL
  rv$samples_c <- NULL
  rv$samples_t <- NULL
  rv$deg_pdata <- NULL
  rv$gpl_tooltips <- NULL
  rv$text <- NULL
  rv$matrix_ready <- NULL
  rv$demo <- ""
}

init_choices <- function(){
  updateRadioButtons(session, inputId = "data_type", selected = "raw")
  updateSelectInput(session, inputId = "sp_select_var", selected = "strain")
}

init_choices2 <- function(){
  updateCheckboxGroupInput(session, inputId = "sp_select_levels", selected = c("N/A","USA-WA1/2020"))
}

init_choices3 <- function(){
  updatePickerInput(session, inputId = "samples_c_deg", selected = c("GSM4462342", "GSM4462343", "GSM4462344"))
  updatePickerInput(session, inputId = "samples_t_deg", selected = c("GSM4462345", "GSM4462346", "GSM4462347"))
}

init_choices4 <- function(){
  updateSelectizeInput(session, inputId = "aplot_genes", selected = "CXCL2")
  rv$a_gene = "CXCL2"
}

# =============== demo toggle button ===============
btn_demo <- function(id){
  if(rv$demo_n %% 2 == 1){
    label = "Example Run"
    icon = "play"
    color = "warning"
    style = "bordered"
    size = "sm"
  }else{
    label = "Unload Example"
    icon = "trash-alt"
    color = "default"
    style = "minimal"
    size = "md"
    
  }
  
  fixedPanel(
    bottom = 25,
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