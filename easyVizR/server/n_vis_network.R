#=======================================================================#
####----------------------- Functions: VisNetwork -------------------####
#=======================================================================# 

output$nw_set_le_ui <- renderUI({
  req(is.null(rv$nw_selected_n)==F)
  
  available_cols <- firstmatch(le_alias,rv$nw_char_stats)
  if (length(available_cols)>0){
    div(
      selectInput(
        inputId = "nw_selected_col",
        label = "Select leading edge column:",
        choices = rv$nw_char_stats,
        selected = available_cols
      ),
      radioButtons("nw_le_sep", "Separator:", 
                   choices=c(
                     "space" = "\\s+",
                     "comma" = ","
                   )
      ),
      uiOutput("nw_feedback"),
    )
  } else {
    box(
      title = NULL, background = "red", solidHeader = TRUE, width=12,
      paste0("No character column available for selection. ")
    )
  }
})

output$nw_feedback <- renderUI({
  if (is.null(rv$nw_edges_mat)==T){
    box(
      title = NULL, background = "yellow", solidHeader = TRUE, width=12,
      paste0("No shared edges detected - Please double check your data.")
    )
  }
})

# wrapper
output$vis_network_panel <- renderUI({
  
  if (is.null(rv$nw_selected_n)==F & is.null(rv$nw_selected_col)==F){
    
    box(
      title = span(icon("chart-area"), "Network"), status = "primary", solidHeader = F, width=12,
      
      div(id="n_nw_p",
      visNetworkOutput("vis_network", height = "660px"),
      ),
      
      div(style = "position: absolute; left: 1em; bottom: 1em;",
          dropdown(
            selectInput("vis_percent",
                        label = p("Edge parameters",
                                  tags$style(type = "text/css", "#q_vis_edge {display: inline-block;width: 20px;height: 20px;padding: 0;border-radius: 50%;vertical-align: baseline;margin-left: 10px;}"),
                                  bsButton("q_vis_edge", label = "", icon = icon("question"), style = "default", size = "extra-small")),
                        
                        choices = list(
                          "Jaccard Coefficient" = "jaccard",
                          "Overlap Coefficient" = "overlap",
                          "Combined Coefficient" = "combined"
                        ),
                        selected = "jaccard"
            ),
            bsTooltip("q_vis_edge", "Click to learn more", placement = "top"),
            
            numericInput("vis_percent_cutoff",
                         label = p("Edge threshold",
                                   tags$style(type = "text/css", "#q_vis_edge_threshold {display: inline-block;width: 20px;height: 20px;padding: 0;border-radius: 50%;vertical-align: baseline;margin-left: 10px;}"),
                                   bsButton("q_vis_edge_threshold", label = "", icon = icon("question"), style = "default", size = "extra-small")),
                         value=0.25, min = 0, max = 1, step = 0.01
            ),
            bsTooltip("q_vis_edge_threshold", "Click to learn more", placement = "top"),
            
            uiOutput("combined_k_ui"),
            
            size = "xs",width="280px",
            icon = icon("gear", class = "opt"),
            up = TRUE
          )
      ),
      div(style = "position: absolute; left: 4em; bottom: 1em; width:300px;",
          dropdown(
            radioGroupButtons("p_or_q_vis","Color by",
                              choiceNames = c("PValue", "FDR"),
                              choiceValues = c("PValue", "FDR"),
                              selected = "PValue",
                              direction = "horizontal",status="default"
            ),
            
            size = "xs",
            icon = icon("palette", class = "opt"),
            up = TRUE
          )
      ),
      div(style = "position: absolute; left: 7em; bottom: 1em; width:300px;",
          dropdown(
            downloadButton(outputId = "download_vis", label = "Download network"),
            
            size = "xs",
            icon = icon("download", class = "opt"),
            up = TRUE
          )
      ),
      
      
    )
    
  } else {
    
    box(
      title = span( icon("chart-area"), "Network"), status = "warning", solidHeader = F, width=12,
      div(id="n_nw_p",
          paste0("Network is not available for the current selection.")
          )
      
    )
  }
  
  
  
})

# plot
output$vis_network <- renderVisNetwork({
  withProgress(message = 'Generating plots ...',value = 1, {
    rv$vis = vis()
    return(rv$vis)
  })
})


# download
output$download_vis <- downloadHandler(
  filename = function() {paste0("network_",Sys.Date(),".html")},
  content = function(file) {saveWidget(as_widget(rv$vis), file, selfcontained = TRUE)}
  
  # content = function(file) {saveWidget(as_widget(vis()), file, selfcontained = TRUE)}
)



output$combined_k_ui <- renderUI({
  req(rv$vis_percent=="combined")
  numericInput("combined_k","Combined constant, K",
               0.5, min = 0, max = 1, step = 0.01
  )
})


#  ============ vis edges modal =============
observeEvent(input$q_vis_edge,{
  showModal(modalDialog(
    inputId = "vis_edge_md",
    title = "Edge parameters: Determine the degree of gene overlap between gene sets",
    includeMarkdown(paste0(getwd(),"/inc/edge_explaination.md")),
    easyClose = TRUE,size="l",
    footer = modalButton("Dismiss")
  ))
  
})

observeEvent(input$q_vis_edge_threshold,{
  showModal(modalDialog(
    inputId = "vis_edge_md",
    title = "Recommendations on choice of thresholds",
    includeMarkdown(paste0(getwd(),"/inc/edge_threshold_explaination.md")),
    easyClose = TRUE,size="l",
    footer = modalButton("Dismiss")
  ))
  
})


#  ============ functions =============
addlinebreaks_vis <- function(x, max=50, lbtype="<br>"){
  x = gsub(paste0('(.{1,',max,'})(\\s|$)'), paste0('\\1',lbtype), x)
  return(x)
}


# edge calculation methods

jaccard_coef <- function(x,y){
  return(length(intersect(x,y))/length(union(x,y)))
}

overlap_coef <- function(x,y){
  return(length(intersect(x,y))/min(length(x),length(y)))
}

combined_coef <- function(x,y,k=0.5){
  o_coef <- overlap_coef(x,y)
  j_coef <- jaccard_coef(x,y)
  return((k * o_coef) + ((1-k) * j_coef))
}


# get edge pre-matrix

# edges <- function(a,a_gmt,b,method=rv$vis_percent,cutoff=rv$vis_percent_cutoff,edges_k=rv$combined_k){
edges <- function(a,b,method=rv$vis_percent,cutoff=rv$vis_percent_cutoff,edges_k=rv$combined_k){
  cal_coeff <- function(x,y){
    if(method=="jaccard"){
      cal_coeff <- jaccard_coef(x,y)
    }else if(method=="overlap"){
      cal_coeff <- overlap_coef(x,y)
    }else if(method=="combined"){
      cal_coeff <- combined_coef(x,y,k=edges_k)
    }
    return(cal_coeff)
  }
  
  from = NULL;to=NULL;percent=NULL;sharedn=NULL;sharedlist=NULL
  
  for(i in seq_along(b)){
    c = b[[i]][[1]];d = b[[i]][[2]];coeff=0;
    # x_gmt = unname(unlist(a_gmt[c])); y_gmt = unname(unlist(a_gmt[d]))
    x = toupper(unname(unlist(a[c]))); y = toupper(unname(unlist(a[d])))
    # print(str(head(x)))
    
    coeff <- cal_coeff(x,y)
    # print(coeff)
    
    shared_genes = intersect(x,y)
    
    if(coeff >= cutoff){
      from = c(from, c)
      to = c(to, d)
      percent = c(percent, coeff)
      sharedn = c(sharedn, length(shared_genes))
      sharedlist = c(sharedlist,paste(shared_genes, collapse=", "))
    }
    c = NULL;d= NULL; x=NULL; y=NULL
  }
  
  if(length(from)==0){
    edges = NULL
  }else{
    edges <- data.frame(from = from,
                        to = to,
                        percent = percent,
                        sharedn = sharedn,
                        sharedlist = sharedlist,
                        stringsAsFactors=FALSE)
  }
  # print(str(head(edges)))
  return(edges)
}

# plot vis network

vis <- function(){
  # req(is.null(rv$vis_status) == T)
  rv$vis = NULL
  rv$vis_status = NULL
  # get df
  # df = dfNEL()
  nn <- rv$nw_selected_n
  le_coln <- paste0(rv$nw_selected_col, "_", nn)
  pcol <- paste0("PValue_", nn)
  qcol <- paste0("FDR_", nn)
  pqcol <- paste0(rv$p_or_q_vis, "_", nn)
  statcol <- paste0("Stat", "_", nn)
  df = n_ins_full()
  
  # print(df)
  
  # print(nrow(df))
  if(nrow(df)<1){
    rv$vis_status = "failed"
    return(NULL)
  }else{
    rv$vis_status = "success"
    # leading edge genes
    a = df[[le_coln]]
    a = sapply(a, function(x) strsplit(x,rv$nw_le_sep))
    names(a) <- df$Name
    # print(a)
    
    # GMT genes
    # a_gmt = rv$gmts[names(rv$gmts) %in% df$Name]
    
    edges_mat = NULL
    if(nrow(df)>1){
      # pathway combinations
      b_combn<-sapply(as.data.frame(combn(names(a),2)), function(x) as.character(x), simplify = FALSE)
      
      # edge pre-matrix
      # edges_mat = edges(a,a_gmt,b_combn)
      edges_mat = edges(a,b_combn)
      # rv$hc_edges = edges_mat[,c("from","to","percent")]
      # edges_mat = edges_mat[edges_mat$percent>rv$vis_percent_cutoff,]
    }
    rv$nw_edges_mat <- edges_mat
    print(edges_mat)
    
    # nodes matrix
    # colors
    colors = rep("lightgray",nrow(df))
    colors[df[[pqcol]]<0.25 & df[[statcol]]>0] = "rgba(254,224,144)" #lightyellow
    colors[df[[pqcol]]<0.1 & df[[statcol]]>0] = "rgba(253,174,97)" #yellow
    colors[df[[pqcol]]<0.05 & df[[statcol]]>0] = "rgba(244,109,67)" #orange
    colors[df[[pqcol]]<0.01 & df[[statcol]]>0] = "rgba(215,48,39)" #red
    colors[df[[pqcol]]<0.001 & df[[statcol]]>0] = "rgba(165,0,38)" #dark red

    colors[df[[pqcol]]<0.25 & df[[statcol]]<0] = "rgba(198,219,239)" #pale blue
    colors[df[[pqcol]]<0.1 & df[[statcol]]<0] = "rgba(158,202,225)" #light blue
    colors[df[[pqcol]]<0.05 & df[[statcol]]<0] = "rgba(107,174,214)" #blue
    colors[df[[pqcol]]<0.01 & df[[statcol]]<0] = "rgba(49,130,189)" #darker blue
    colors[df[[pqcol]]<0.001 & df[[statcol]]<0] = "rgba(8,81,156)" #cornflower



    
    # shapes
    shapes = rep("dot",nrow(df))
    
    # sizes
    sizes = sapply(a, function(x) length(x))
    
    # # group for selection
    # if(rv$run_mode == "gsea"){
    #     group <- ifelse(df$ES >= 0, "Up", "Down")
    # }
    
    # node hover
    hovertexts <- df$Name
      hovertexts <- stat_replace1(paste0("<b>", df$Name,"</b><br>
                     PValue = ",round(as.numeric(df[[pcol]]),3),
                           "; FDR = ",round(as.numeric(df[[qcol]]),3),
                           "; Stat = ",round(as.numeric(df[[statcol]]),3),
                           "<br>","Leading edge (",sizes,")",":<br>", addlinebreaks_vis(df[[le_coln]])), input$nw_selected_n)
      
    
    
    # generate nodes
    nodes <- data.frame(
      id = df$Name,
      label = df$Name,
      value = sizes * 2,  # sizes proportional to no of leading edge genes
      color = colors, # color represents ES and p
      shape = shapes, 
      # group = group, # group represents ES up/down,
      # font.size = 5+sizes*15,
      title = hovertexts
    )
    
    if(is.null(edges_mat)==T || nrow(edges_mat)<1){
      vis <- visNetwork(nodes, height = "1000px", width = "100%") %>%
        # visEdges(smooth = FALSE) %>% #disable smooth curve for edges
        # visIgraphLayout() %>% # decrease plotting time
        visNodes(borderWidth= 2) %>%
        visInteraction(navigationButtons = TRUE) %>% 
        visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T), 
                   nodesIdSelection = TRUE) %>% # , selectedBy = "group"once select a node, see relevant nodes and grey out the rest.
        # visPhysics(stabilization = FALSE) %>%
        visPhysics(solver = "barnesHut") %>% # node moving dynamics
        visLayout(randomSeed = 12) # to always have the same network
      return(vis)
    }else{
      # generate edges
      edgehovertexts <- paste0("<b>", edges_mat$from, "<br>", edges_mat$to,"</b><br>","<br>Shared ",tail(colnames(df),n=1),":<br>", addlinebreaks_vis(edges_mat$sharedlist))
      edges <- data.frame(
        from = edges_mat$from,
        to = edges_mat$to,
        # width = edges_mat$sharedn*2,
        # length = 1 - edgelist$percent, 
        title = edgehovertexts
      )
      
      vis <- visNetwork(nodes, edges, height = "1000px", width = "100%") %>%
        visEdges(smooth = FALSE) %>% #disable smooth curve for edges
        # visIgraphLayout() %>% # decrease plotting time
        visNodes(borderWidth= 2) %>%
        visInteraction(navigationButtons = TRUE) %>% 
        visOptions(highlightNearest = list(enabled = T, degree = 1, hover = T), 
                   nodesIdSelection = TRUE, selectedBy = "group") %>% # once select a node, see relevant nodes and grey out the rest.
        # visPhysics(stabilization = FALSE) %>%
        visPhysics(solver = "barnesHut") %>% # node moving dynamics
        visLayout(randomSeed = 12) # to always have the same network
      return(vis)
    }
  }
}