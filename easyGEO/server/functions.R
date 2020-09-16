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
mutate_df <- function(){
  df = filter_df()
  
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
  
  if(rv$plot_label == "top"){
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