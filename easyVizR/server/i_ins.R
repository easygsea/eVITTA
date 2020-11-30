####================= MULTIPLE - INTERSECT =====================####
palette <- c("aquamarine","blue","darkgrey","darksalmon","red","lightgrey","lightblue","lightgreen","lightyellow","white","yellow") # this is the default base palette for venns

####-------------------- Intersection selection ------------------------####

# generates dynamic ui for selection
observe({
  # # save RVs in the rvs to restore later for the demo session
  # saveRDS(rv$folder_upload_state, file = "rvs/folder_upload_state.rds")
  # saveRDS(rv$upload_batch_colscheme, file = "rvs/upload_batch_colscheme.rds")
  # saveRDS(rv$upload_batch_sharedcols, file = "rvs/upload_batch_sharedcols.rds")
  # saveRDS(rv$batch_failed, file = "rvs/batch_failed.rds")
  # saveRDS(rv$batch_files, file = "rvs/batch_files.rds")
  # saveRDS(rv$columnCount, file = "rvs/columnCount.rds")
  # saveRDS(rv$upload_batch_columns, file = "rvs/upload_batch_columns.rds")
  # saveRDS(rv$FileDF, file = "rvs/FileDf.rds")
  # saveRDS(rv$ll, file = "rvs/ll.rds")
  # saveRDS(rv$gg, file = "rvs/gg.rds")
  # saveRDS(rv$tt, file = "rvs/tt.rds")
  # saveRDS(rv$upload_columns, file = "rvs/upload_columns.rds")
  # saveRDS(rv$n_sharedcols, file = "rvs/n_sharedcols.rds")
  # saveRDS(rv$n_sharedrows, file = "rvs/n_sharedrows.rds")
  # saveRDS(rv$heatmap_i, file = "rvs/heatmap_i.rds")
  # saveRDS(rv$nx_n, file = "rvs/nx_n.rds")
  # saveRDS(rv$df_n, file = "rvs/df_n.rds")
  # saveRDS(rv$nic, file = "rvs/nic.rds")
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
  
  req(nrow(rv$df_n)>0)
  req_filter_ns("nic",rv)
  
  criteria <- rv$ins_criteria
  criteria <- as.character(criteria)
  criteria[is.na(criteria)] <- "Ignore"
  criteria <- gsub("FALSE","False", criteria)
  criteria <- gsub("TRUE", "True",criteria)
  print(criteria)
  for (i in 1:length(rv$nx_n)){
    rv$s[[i]] <- div(style="display: inline-block;vertical-align:top; width: 280px;",
                     radioGroupButtons(inputId = paste0("ins_",i),
                                       label = rv$nx_n[[i]], size="s",
                                       choices = c("True", "False", "Ignore"),
                                       selected =criteria[[i]],
                                       status = "info"),
                     
                     radioTooltip(id = paste0("ins_",i), choice = "True", 
                                  title = paste0("Included in the Venn circle.","<br>(",summarize_filter("nic",rv,rv$nx_n[[i]],T,include_name=F)," in this dataset)"), 
                                  placement = "bottom"),
                     radioTooltip(id = paste0("ins_",i), choice = "False", 
                                  title = paste0("Excluded from the Venn circle.","<br>(",summarize_filter("nic",rv,rv$nx_n[[i]],F,include_name=F)," in this dataset)"), 
                                  placement = "bottom"),
                     radioTooltip(id = paste0("ins_",i), choice = "Ignore", 
                                  title = "Filters ignored", 
                                  placement = "bottom"),

                     )
  }
  rv$s_button[[1]] <- div(style="display: inline-block;margin-top: 1.55em; width: 280px;",
                     actionButton("ins_applytorv", "Save selection", class = "btn-warning")
                     )
    
  
})

output$ui_intersections <- renderUI({
  req(nrow(rv$df_n)>0)
  append(x = rv$s, value=rv$s_button, length(rv$s))
    
})

# updates criteria
observeEvent(input$ins_applytorv, {
  req(rv$df_n)
  req(length(rv$s)==length(rv$nx_i))
  
  # turn those T/F inputs into a true/ false vector
  criteria <- vector(mode = "list", length = length(rv$v))
  for (i in 1:length(rv$s)){
    criteria[[i]] <- input[[paste0("ins_", i)]]
  }
  criteria <- unlist(criteria)
  criteria <- gsub("False",FALSE, criteria)
  criteria <- gsub("True",TRUE, criteria)
  criteria <- gsub("Ignore",NA, criteria)
  criteria <- as.logical(criteria)
  
  # name the criteria vector
  if (length(criteria) == length(rv$nx_n)){
    names(criteria) <- rv$nx_n
  }
  # print(criteria)
  rv$ins_criteria <- criteria
})


####-------------------- Active Filters summary ------------------------####

# summarizes verbally what is in the selected intersection
output$intersection_summary <- renderUI({
  req(is.null(rv$ins_criteria)==F)
  req(length(rv$ins_criteria)>0)
  
  desc <-list()
  criteria <- rv$ins_criteria
  req_filter_ns("nic", rv)
  
  # generate a description for every dataset and put into list
  for (i in 1:length(rv$nx_n)){
    # only append a description when it's not on ignore
    adddesc <- summarize_filter("nic", rv, rv$nx_n[[i]], criteria[[rv$nx_n[[i]]]])
    if (is.na(adddesc)==F){
      desc <- c(desc, adddesc)
    }
  }
  
  # make into html unordered list
  if (length(desc)>0){
    text <- paste0("<ul>", paste(paste0("<li>",desc,"</li>"), collapse=""), "</ul>")
    HTML(text)
  } else {
    HTML("No filters are active.")
  }
  
})

# summarizes verbally the filters used to trim gene lists
output$filters_summary <- renderUI({
  
  desc <-list()
  
  for (i in 1:length(rv$nx_n)){
    req(rv[[paste0("nic_p_",i)]])
    req(rv[[paste0("nic_q_",i)]])
    req(rv[[paste0("nic_Stat_",i)]])
    req(rv[[paste0("nic_sign_",i)]])
    
    name <- rv$nx_n[[i]]
    cur_p <- rv[[paste0("nic_p_",i)]]
    cur_q <- rv[[paste0("nic_q_",i)]]
    cur_Stat <- rv[[paste0("nic_Stat_",i)]]
    cur_sign <- rv[[paste0("nic_sign_",i)]]
    if (cur_sign=="All"){
      cur_sign = "Positive and Negative"
    } else {
      cur_sign <- cur_sign
    }
    
    nterms <- length(n_ins_gls()[[i]]) # number of genes in genelist
    
    adddesc <- paste(name, ": ",
                     nterms, " ",
                     cur_sign, " entries with ",
                     "p <= ",cur_p, ", ", 
                     "FDR <= ", cur_q, ", ", 
                     "|Stat| >= ", cur_Stat,
                     sep="")
    desc <- c(desc, adddesc)
  }
  text <- paste(desc, collapse="<br>")
  
  
  HTML(text)
})




####-------------------- Intersection table ------------------------####

# show intersection preview table
output$n_ins_tbl <- DT::renderDataTable({
  req(is.null(rv$df_n)==F)
  req(length(rv$ins_criteria)>0)

  df <- n_ins_df()
  # print(head(n_ins_df()))
  
  rv$df_ins_fullcols <- colnames(df)
  
  # to abbreviate the long column names...take first 5 letters
  char_limit <- 56 / length(colnames(df))
  # print(char_limit)
  colnames(df) <- sapply(names(df), function(x){
    if (nchar(x)>char_limit)
    {return (paste0(substr(x, start = 1, stop = char_limit),"..."))}
    else{return (x)}
  })
  
  # to round everything down to 3 decimals
  df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
  
  
  df
  
  
}
, plugins = "ellipsis",
options = list(scrollX=TRUE, 
               columnDefs = list(
                 list(
                   targets = 1,
                   render = JS("$.fn.dataTable.render.ellipsis( 17, true )")
                 ),
                 list(
                   targets = "_all",
                   render = JS("$.fn.dataTable.render.ellipsis( 6, true )")
                 )
               ),
               headerCallback= JS("function(thead, data, start, end, display){",
                                  sprintf("  var tooltips = [%s];", toString(paste0("'", rv$df_ins_fullcols, "'"))),
                                  "  for(var i = 1; i <= tooltips.length; i++){",
                                  "    $('th:eq('+i+')',thead).attr('title', tooltips[i-1]);",
                                  "  }",
                                  "}"))
)


# download intersection table as csv
output$download_ins_df <- downloadHandler( # needs to be exactly the same as table render
  filename = function() {
    paste("intersection", "-", "multiple", "-", Sys.Date(), ".csv", sep="")},
  content = function(file) {
    write.csv(n_ins_df(), file, 
              row.names = F, quote=TRUE)})

# download filtered gene list as txt
output$download_ins_gl <- downloadHandler(
  filename = function() {
    paste("names", "-", "multiple", "-", Sys.Date(), ".txt", sep="")},
  content = function(file) {
    ins <- n_ins_full()$Name
    fwrite(list(ins), file, sep=",", 
           row.names = F, quote=F)
  }
)




####-------------------- Upset plot ------------------------####

n_upset_plt <- reactive({
  gls <- n_ins_gls()
  
  names(gls) <- gsub(".csv","",names(gls))
  #c(intersection size title, intersection size tick labels, set size title, set size tick labels, set names, numbers above bars)
  textscale <- c(1.3, 2, 1.3, 1, 1.1, 2)
  if (rv$n_upset_showempty==F){
    upset <- upset(fromList(gls), order.by = rv$n_upset_sortby,
                   text.scale = textscale)
  }
  else if (rv$n_upset_showempty==T){
    upset <- upset(fromList(gls), empty.intersections = "on", order.by = rv$n_upset_sortby,
                   text.scale = textscale)
  }
  upset
})


# gl ver (uses the shared reactive)
output$df_n_upset <- renderPlot({
  req(rv$df_n)
  req(is.null(rv$n_upset_showempty)==F)
  req(is.null(rv$n_upset_sortby)==F)
  req(max(lengths(n_ins_gls()))>0)
  req(min(lengths(n_ins_gls()))>0)
  req(length(n_ins_gls())>1)
  
  
  n_upset_plt()
})

output$n_upset_dl <- downloadHandler(
  filename = function() { paste("upset-multiple-",Sys.Date(), '.pdf', sep='') },
  content = function(file) {
    pdf(file)
    print(n_upset_plt())
    dev.off()
  }
)




####-------------------- Venn plot ------------------------####

output$n_npvenn_dl <- downloadHandler(
  filename = function() { paste("venn1-multiple-",Sys.Date(), '.png', sep='') },
  content = function(file) {
    req(is.null(n_npvenn_plt())==F)
    ggsave(file, plot = n_npvenn_plt(), device = "png")
  }
)

output$n_venn_dl <- downloadHandler(
  filename = function() { paste("venn2-multiple-",Sys.Date(), '.png', sep='') },
  content = function(file) {
    req(is.null(n_venn_plt())==F)
    ggsave(file, plot = n_venn_plt(), device = "png")
  }
)


#------------------- eulerR area proportional venn
# draws an eulerr diagram with intersection highlighted
#------------------------------------------
# gls: named list of vectors
# ins: named vector
draw_eulerr_with_ins <- function(gls, ins, print_mode="counts", show_ins=T, ins_color="red", base_colors=palette,
                                 adjust_labels=T){
  fit2 <- euler(gls)
  # get t/f/na subsets
  t_sections <- names(ins[ins==T & is.na(ins)==F])
  f_sections <- names(ins[ins==F & is.na(ins)==F])
  na_sections <- names(ins[is.na(ins)==T])
  
  selector <- names(fit2[[2]])
  # first get the ins that has all the true sections, if any
  if (length(t_sections)>0){
    temp=vector(mode="list", length=length(t_sections)) # get the sections that contain ALL the T substrings
    for (i in 1:length(t_sections)){
      temp[[i]] <- selector[grep(t_sections[[i]], selector)]
    }
    selector <- Reduce(intersect, temp)
    
  }
  # second get rid of any ins that contains the false sections, if any
  if (length(f_sections)>0){
    for (f in f_sections){
      selector <- selector[-grep(f, selector)]
    }
  }
  # we don't care about the na sections
  
  # assign colors to venn
  colors <- fit2[[2]]
  
#Now we deal with color
    if(length(rv$ins_venn_palette) < length(gls)){
      temp = rv$ins_venn_palette
      while (length(temp) < length(gls)) {
        temp = append(temp,"white")
      }
    }
    else{
      temp = rv$ins_venn_palette[1:length(gls)]
      
    }
  #This part split the color column based on names, so we will know which color to mix later
  ColorSplitName <- names(colors)
  ColorSplitName <- strsplit(ColorSplitName, "&")
  
  while(length(temp) < length(colors)){
    colorsToMix = c()
    for (GLSIndex in 1:length(gls)) {
     
     #print(ColorSplitName[[GLSIndex]])
     #print(ColorSplitName[[length(temp)+1]])
     if(ColorSplitName[[GLSIndex]] %in% ColorSplitName[[length(temp)+1]]){
       colorsToMix = append(colorsToMix,temp[GLSIndex]) 
     }
    }
    colorsToMix<-col2rgb(colorsToMix)
    colorsToMix <-rowMeans(colorsToMix)
    temp = append(temp,rgb(colorsToMix[[1]],colorsToMix[[2]],colorsToMix[[3]],max = 255))
    colorsToMix <- NULL
    
  }
    #TODO: Add a mix color here!
  
  colors[] <- temp
  
  if (show_ins==T){ # if highlight the ins, will show all else as white
    colors[which(names(colors) %in% selector)] <- ins_color
  }
    
    #print(colors[1])
    #print(names(colors))
  #} 
  #else { # if don't highlight the ins, will show base colors
  #  colors <- palette[1:length(gls)] # assign base circle colors
  #}
  
  # draw the venn
  venn <- plot(fit2, quantities = list(type = print_mode), fills =colors, adjust_labels=adjust_labels)
  venn
}



# gl ver (uses the shared reactive)
n_venn_plt <- reactive({
  # gls <- n_ins_gls()
  # # names(gls) <- addlinebreaks2(names(gls), 20, "\n")
  # fit2 <- euler(gls)
  # venn <- plot(fit2, quantities = list(type = rv$n_venn_label))
  
  
  draw_eulerr_with_ins(n_ins_gls(), rv$ins_criteria, 
                       print_mode=rv$n_venn_label, 
                       show_ins=rv$n_venn_show_ins, ins_color=rv$ins_venn_c1, base_color=rv$ins_venn_palette,
                       adjust_labels=T)
  
})

output$df_n_venn <- renderPlot({
  req(length(n_ins_gls())>0)
  req(max(lengths(n_ins_gls()))>0)
  req(rv$n_venn_type=="Area-proportional")
  req_vars(rv$ins_criteria)
  
  n_venn_plt()
})


# find closest label to each circle, and use that to match the ins criteria.
#-------------------------------------------
# used within draw_venn_with_ins()
find_closest_label <- function(circle_coords, labs, nx_n, lb_limit){
  # req_vars(c(circle_coords, labs, nx_n, lb_limit))
  
  textlabs<-labs[which(labs$label %in% nx_n),] # find the text labels within labs and their xy
  spts<- SpatialPoints(textlabs[1:2]) # get text label xy
  grobpts <- SpatialPoints(lapply(circle_coords, function(x){x[seq(1, length(x), 200)]})) # extract every 200th point on the circle, get xy.
  dmat=gDistance(spts, grobpts, byid = TRUE) # calculate dist matrix of text labels vs circle points
  closest <- colnames(dmat)[which(dmat == min(dmat), arr.ind = TRUE)[[2]]] # find index of closest text label
  textlabs[closest, "label"]
}


# draw venn from gene lists and highlight intersection
#---------------------------------------------
# for gls: pass in a named list of vectors
# for ins: pass in a named vector with T/F/NA as true/false/ignore
draw_venn_with_ins <- function(gls, ins, nx_n=rv$nx_n, print_mode="raw", lb_limit=20, show_ins=T, ins_color="red", base_color=palette){
  # get needed parameters
  d <- gls
  ins <- ins
  nx_n <- addlinebreaks2(nx_n, lb_limit, "\n")
  names(d) <- addlinebreaks2(names(d), lb_limit, "\n")
  names(ins) <- addlinebreaks2(names(ins), lb_limit, "\n")
  
  if (show_ins==F){
    fill=base_color[1:length(gls)]
    #print(fill)
    alpha=0.5
  } else if (show_ins==T){
    #fill="white"

  if(length(rv$ins_venn_palette) <= length(gls)){
      temp = rv$ins_venn_palette
      while (length(temp) < length(gls)) {
        temp = append(temp,"white")
      }
      fill = temp
    }
    else{
      fill = rv$ins_venn_palette[1:length(gls)]
    }
    
    #print(fill)
    alpha=1
  }
  
  # draw venn diagram
  vp <- venn.diagram(d, filename = NULL,
                     lwd = 1, # border width
                     fontfamily = "sans",
                     cat.fontface = "bold",
                     cex = 1, # catname size
                     # cat.pos = rep(180,len), # catname position
                     cat.cex = 1, # areaname size
                     cat.fontfamily = "sans",
                     fill = fill, # area fill
                     alpha = alpha, # area alpha
                     ext.text=T, # draw label outside in case no space
                     ext.line.lty = "dotted", # pattern of extline
                     sigdigs = 2,
                     print.mode = print_mode,
                     category.names=names(d)
  )
  
  if (show_ins==F){
    grid.newpage()
    grid.draw(vp)
  } else if (show_ins==T){
    # get labels from venn
    ix <- sapply(vp, function(x) grepl("text", x$name, fixed = TRUE))
    labs <- do.call(rbind.data.frame, lapply(vp[ix], `[`, c("x", "y", "label")))
    
    # get circles and names from venn
    all_circ <- vector(mode="list", length=length(d))
    all_names <- vector()
    
    # transform the circles into accepted format
    for (i in 1:length(d)){
      
      # important:
      # if any expected circle is missing (i.e. due to overlaps), immediately break and don't proceed to highlight intersection
      if(length(vp[[2+i]][[1]])<2 & length(vp[[2+i]][[2]])<2){
        return(grid.draw(vp))
      }
      
      circ <- list(list(x = as.vector(vp[[2+i]][[1]]), y = as.vector(vp[[2+i]][[2]]))) # get x and y
      all_circ[[i]] <- circ # put circle x y into list
      nn <- find_closest_label(circ[[1]], labs, nx_n, lb_limit)
      all_names <- c(all_names, nn) # try to find closest label (because the labels are all jumbled.)
    }
    names(all_circ) <- all_names
    
    t_sections <- all_circ[which(ins[names(all_circ)]==T)]
    f_sections <- all_circ[which(ins[names(all_circ)]==F)]
    na_sections <- all_circ[which(is.na(ins[names(all_circ)]))]
    # find the intersection
    # op=c("intersection", "union", "minus", "xor")
    
    # calculate which intersection to color
    if (length(t_sections)>0){ # first intersect all the T circles
      sel_int <- Reduce(function(x, y) polyclip(x, y, op="intersection"), 
                        t_sections)
    } else {
      sel_int <- Reduce(function(x, y) polyclip(x, y, op="union"), 
                        na_sections)
    }
    if (length(f_sections)>0){ # next minus all the F circles
      sel_int <- Reduce(function(x, y) polyclip(x, y, op="minus"), 
                        c(sel_int, f_sections))
    }
    
    # replot the diagram
    grid.newpage()
    par(mar=c(0, 0, 0, 0), xaxs='i', yaxs='i')
    plot(c(-0.1, 1.1), c(-0.1, 1.1), type = "n", axes = FALSE, xlab = "", ylab = ""
    )
    for (i in 1:length(d)){ # draw the circles
      polygon(all_circ[[i]][[1]],col =alpha(rv$ins_venn_palette[i],0.8), border = "black")
    }
    if (length(sel_int)>0 & identical(unname(ins), rep(F,length(d)))==F){ # if intersection is valid
      polygon(sel_int[[1]], col = ins_color)
    }
    for (i in 1:length(d)){ # draw the circles
      polygon(all_circ[[i]][[1]],col = alpha(rv$ins_venn_palette[i],0), border = "black")
    }

    
    text(x = labs$x, y = labs$y, labels = labs$label)
    
  }
  
  

  
  # if (show_ins==T){
  # 
  # 
  # } else if (show_ins==F){
  # 
  #   
  #   # replot the diagram
  #   grid.newpage()
  #   par(mar=c(0, 0, 0, 0), xaxs='i', yaxs='i')
  #   plot(c(-0.1, 1.1), c(-0.1, 1.1), type = "n", axes = FALSE, xlab = "", ylab = ""
  #   )
  #   ordered_colors <- unlist(lapply(1:length(all_names),function(x){
  #     base_color[[match(all_names[[x]], nx_n)]]
  #   }))
  #   for (i in 1:length(d)){ # draw the circles
  #     polygon(all_circ[[i]][[1]], col=adjustcolor(ordered_colors[[i]],alpha.f=0.5)  )
  #   }
  #   
  #   text(x = labs$x, y = labs$y, labels = labs$label)
  # }
  
}



# #------------------- VennDiagram non area proportional venn
n_npvenn_plt <- reactive({
  # req(length(n_ins_gls())>0)
  # req(max(lengths(n_ins_gls()))>0)
  # req(n_ins_gls())
  # req(length(rv$ins_criteria)==length(rv$nx_n))
  
  grid.newpage()
  
  # venn1 <- venn.diagram(gls, filename = NULL,
  #                       lwd = 1, # border width
  #                       fontfamily = "sans",
  #                       cat.fontface = "bold",
  #                       cex = 1, # catname size
  #                       # cat.pos = rep(180,len), # catname position
  #                       cat.cex = 1, # areaname size
  #                       cat.fontfamily = "sans",
  #                       fill = palette, # area fill
  #                       alpha = 0.5, # area alpha
  #                       ext.text=T, # draw label outside in case no space
  #                       ext.line.lty = "dotted", # pattern of extline
  #                       sigdigs = 2,
  #                       print.mode = gsub("counts","raw", rv$n_venn_label)
  # )
  
  venn1 <- draw_venn_with_ins(n_ins_gls(), rv$ins_criteria, rv$nx_n,
                              gsub("counts","raw", rv$n_venn_label),
                              lb_limit=20,
                              show_ins=rv$n_venn_show_ins, ins_color=rv$ins_venn_c1, base_color=rv$ins_venn_palette
                              )
  venn1
})

output$df_n_npvenn <- renderPlot({
  # req(length(n_ins_gls())>0)
  # req(max(lengths(n_ins_gls()))>0)
  # req_vars(rv$n_venn_label)
  grid.draw(n_npvenn_plt())

  
})



# ----------------------- exclusion report

output$venn_exclusion_report <- renderUI({
  req_vars(c(n_ins_gls(),rv$ins_venn_c1,rv$ins_criteria))
  req_df(rv$df_n)
  
  excluded <- nrow(rv$df_n)-length(Reduce(union, n_ins_gls()))
  
  if (T %in% rv$ins_criteria ==F & rv$n_venn_show_ins==T){  # only highlight if user selected these excluded genes
    rgbhl <- paste(as.vector(col2rgb(rv$ins_venn_c1)), collapse = ",")
    options <- paste0("color:",rv$ins_venn_c1,";text-shadow: 0px 0px 5px rgba(",rgbhl,", 0.3)") # apply a color+dropshadow effect
  } else {options="color:black"}
  HTML(paste0("<span style='",options,";'>Not in any circle: ", excluded,"</span>"))
})


#------------------- Conditionally show either Venn -------------------

output$n_venn_ui <- renderUI({
  req(is.null(rv$n_venn_type)==F)
  if (rv$n_venn_type=="Basic"){
    plotOutput("df_n_npvenn", width="100%")
  } else if (rv$n_venn_type=="Area-proportional"){
    plotOutput("df_n_venn", width="100%")
  }
})




####--------------------wordcloud for pathways------------------------####

map2color<-function(x,pal,limits=NULL){
  if(is.null(limits)) limits=range(x)
  pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}

# compute the frequency table
n_ins_wc_df <- reactive({
  vc <- n_ins_full()$Name
  sep <- input$n_ins_wc_sep
  words <- unlist(lapply(vc, function(x){
    toupper(unlist(strsplit(x, sep)))
  }))
  words <- gsub("[[:punct:]]$", "", words) # get rid of punctuations at end
  words <- gsub("^[[:punct:]]", "", words) # get rid of punctuations at beginning
  
  df <- data.frame(table(words))
  
  # ignore certain words (supports regex pattern!!)
  ignore <- unlist(strsplit(input$n_ins_Wc_ignore, "\\s+"))
  # ignore <- c("GO", "KEGG", "of", "and", "pathway")
  # df <- df[df$words %in% ignore ==F,]
  pattern <- paste(ignore, collapse = "|")
  df <- df[-grep(pattern, df$words, ignore.case=TRUE),]
  
  df
})

n_ins_wc_plt <- reactive({
  df <- n_ins_wc_df()
  req(max(df$Freq)>1) # blocks further processing if no repeated words are found
  
  
  # only draw top x words
  df <- df[order(df$Freq, decreasing = TRUE), ]
  if (nrow(df)>150){
    df <- df[1:150, ] #or set 150 to whatever
  }
  
  # print(head(df))
  wordcloud(df$words, df$Freq, 
            min.freq = 1, max.words=200, scale=c(3,.5),
            random.order=FALSE, 
            rot.per=0.2, # freq of vertical words
            colors=brewer.pal(8, "Dark2"))
})

output$n_ins_wc <- renderPlot({
  req(length(n_ins_full()$Name)>0)
  
  n_ins_wc_plt()
})

output$n_ins_wc_df <- DT::renderDataTable({
  req(nrow(n_ins_wc_df())>0)
  n_ins_wc_df()[order(n_ins_wc_df()$Freq, decreasing=T),]
}
, plugins="ellipsis",
options=list(scrollX=T, scrollY=T, dom= 'tp',
             pageLength = 10
),
rownames= FALSE
)


#======================================================================#
####                     intersection UI                            ####
#======================================================================#

#----------------- ins table --------------------

output$ins_table_panel <- renderUI({
  
  div(
    
    div(id="n2_4",
        
        wellPanel(
          HTML(paste0(
            "Combine options below to select an intersection of interest:",
            add_help("ins_help", style="margin-left: 5px;margin-bottom:10px;"))
          ),
          bsTooltip("ins_help", 
                    "Hover over the options to see the corresponding filters.", 
                    placement = "top"),
          uiOutput("ui_intersections"),
          
          HTML(paste0(
            "Filtering conditions for current intersection:",
            add_help("active_filters_help", style="margin-left: 5px;"))
          ),
          bsTooltip("active_filters_help", 
                    "Filtering conditions for this intersection are displayed below", 
                    placement = "top"),
          uiOutput("intersection_summary")
        ),
    ),
    
    
    
    dataTableOutput("n_ins_tbl"),
    
    
    div(style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          radioGroupButtons(
            inputId = "n_ins_view",
            label = "Choose view:",
            choices = c("Full", "Minimized", "T/F Matrix"),
            selected= rv$n_ins_view, direction="vertical"
          ),
          
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE
        ),
        radioTooltip(id = "n_ins_view", choice = "Full", 
                     title = "Show all columns for datasets", 
                     placement = "right"),
        radioTooltip(id = "n_ins_view", choice = "Minimized", 
                     title = stat_replace1("Only show the essential columns (Name, Stat, PValue, FDR)", rv$nx_n), 
                     placement = "right"),
        radioTooltip(id = "n_ins_view", choice = "T/F Matrix", 
                     title = "Show true/false value depending on if the corresponding filters conditions are met", 
                     placement = "right"),
        
    ),
    bsTooltip("n_wc_dropdown", "Text enrichment wordcloud (for gene set-type terms)", placement = "top"),
    div(style = "position: absolute; left: 4em; bottom: 1em", id="n2_4b",
        dropdown(inputId="n_wc_dropdown",
                 column(8,
                        column(2, textInput("n_ins_wc_sep", "Separator:", value="_")),
                        column(10, textInput("n_ins_Wc_ignore", "Ignore strings: (separated by spaces)", value="and or of GO KEGG WP RA C2")),
                        
                        plotOutput("n_ins_wc"),
                 ),
                 column(4,
                        dataTableOutput("n_ins_wc_df"),
                 ),
                 
                 size = "xs",
                 icon = icon("cloud", class = "opt", lib="glyphicon"),
                 up = TRUE, width=800
        )
        
    ),
    
    div(style = "position: absolute; left: 7em; bottom: 1em",
        dropdown(
          downloadButton("download_ins_df", "Download current table"),
          downloadButton("download_ins_gl", "Download gene list"),
          
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE, width=300
        )
        
    )
    
    
  )
})


#----------------- ins vis  --------------------

#output$n_ui_intersect <- renderUI({
  # req(rv$n_ui_showpanel == "Intersection")

#})

#----------------- venn --------------------
output$n_venn_ins_hl_opt <- renderUI({
  req_vars(rv$n_venn_show_ins)
  req(rv$n_venn_show_ins==T)
  div(
    selectInput("ins_venn_c1", 
                HTML(paste0(
                  "<b>Intersection highlight color:</b>",
                  add_help("ins_venn_c1_help", style="margin-left: 5px;"))
                ),
                choices = default_colors,
                selected=rv$ins_venn_c1
    ),
    bsTooltip("ins_venn_c1_help", 
              "Highlight color for selected intersection (corresponds to table below)", 
              placement = "top")
  )
})
#Newly added graph color selector
output$n_venn_ins_palette <- renderUI({
  req_vars(rv$n_venn_show_ins)
  #req(rv$n_venn_show_ins==T)
  div(
    selectInput("ins_venn_palette", 
                HTML(paste0(
                  "<b>color(s) for parts not in the intersection:</b>",
                  add_help("ins_venn_palette_help", style="margin-left: 5px;"))
                ),
                choices = default_colors,
                selected=rv$ins_venn_palette,
                multiple = TRUE
    ),
    bsTooltip("ins_venn_palette_help", 
              "This is a multiple choices color picker for non-intersection parts of the graph", 
              placement = "top")
  )
})

output$venn_dropdowns <- renderUI({
  div(
    div(style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          checkboxGroupInput(
            inputId = "n_venn_label",
            label= "Show in label:",
            choices = c("Counts"="counts", "Percent"="percent"),
            selected=rv$n_venn_label,
            inline=T, width="250px"
          ),
          radioGroupButtons(
            inputId = "n_venn_type",
            label = "Venn type:",
            choices = c("Basic","Area-proportional"),
            selected = rv$n_venn_type, direction="horizontal"
          ),
          radioGroupButtons(
            inputId = "n_venn_show_ins",
            label = HTML(paste0(
              "<b>Highlight selected intersection?</b>",
              add_help("n_venn_show_ins_help", style="margin-left: 5px;"))
            ),
            choices = c("Yes"=T,"No"=F),
            selected = rv$n_venn_show_ins, direction="horizontal"
          ),
          bsTooltip("n_venn_show_ins_help", 
                    "Whether to highlight selected intersection (corresponds to table below)", 
                    placement = "top"),
          
          
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE
        )
    ),
    #div(id = "venn_gear_dropdowns_anchor"), #,style = "position: absolute; left: 1em; bottom: 1em"),
    div(style = "position: absolute; left: 4em; bottom: 1em",
        dropdown(
          uiOutput("n_venn_ins_hl_opt"),
          uiOutput("n_venn_ins_palette"),
          size = "xs",
          icon = icon("palette", class = "opt"),
          up = TRUE, width=300
        )
        
    ),
    #div(id = "venn_color_dropdowns_anchor"),#,style = "position: absolute; left: 4em; bottom: 1em"),
    div(style = "position: absolute; left: 7em; bottom: 1em",
        dropdown(
          downloadButton("n_npvenn_dl", "Download basic"),
          downloadButton("n_venn_dl", "Download area-proportional"),
          
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE, width=300
        )
    )
  )
  #div(id = "venn_download_dropdowns_anchor"), #,style = "position: absolute; left: 7em; bottom: 1em"),
})


ins_venn_panel <- reactive({
  box(title = span( strong("3.1a."),icon("chart-area"), "Venn Diagram"), status = "primary", solidHeader = F, width=6,
      
      uiOutput("n_venn_ui"),
      
      div(id = "venn_dropdowns_anchor"),
            
      div(style = "position: absolute; right: 1em; bottom: 1em", 
          uiOutput("venn_exclusion_report")
          
      )
      
      
  )
})



#----------------- upset --------------------
ins_upset_panel <- reactive({
  
  box(
    title = span( strong("3.1b."),icon("chart-area"), "UpSet Plot"), status = "primary", solidHeader = F, width=6,
    
    plotOutput("df_n_upset", width = "100%"),
    
    div(style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          selectInput(
            inputId = "n_upset_sortby",
            label = "Order by:",
            choices = c("Frequency"="freq", "Degree"="degree"),
            selected = "freq"),
          materialSwitch(
            inputId = "n_upset_showempty", label = "Show empty intersections?", status="primary",
            value = FALSE
          ),
          
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE, width=300
        )
        
    ),
    div(style = "position: absolute; left: 4em; bottom: 1em",
        dropdown(
          downloadButton("n_upset_dl", "Download plot"),
          
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE, width=300
        )
        
    )
    
  )
})

output$n_venn_placeholder <- renderUI({
  box(
    title = span( strong("3.1a."),icon("chart-area"), "Venn Diagram"), status = "warning", solidHeader = F, width=6,
    paste0("Venn diagram is only available for 5 or less datasets. You have selected ", length(rv$nx_i)," datasets.")
  )
})
output$n_upset_placeholder <- renderUI({
  box(
    title = span( strong("3.1b."),icon("chart-area"), "UpSet Plot"), status = "warning", solidHeader = F, width=6,
    paste0("UpSet plot is only available for 5 or less datasets. You have selected ", length(rv$nx_i)," datasets.")
  )
})




#======================== ASSEMBLE INTO A PAGE ========================#

output$ins_main_panels <- renderUI({
  if (is.null(rv$df_n)==T){
    box(
      title = span( icon("exclamation"), "Notification"), status = "warning", width=8,
      "No data selected."
    )
  } else {
    div(
      fluidRow(
        div(style="height: 3.5em;",
            column(6,
                   HTML("<span style='font-size: 160%;margin-left: 0.5em;'>Select Intersection</span>"),
                   
            ),
            column(6,align= "right",
                   div(id="ins_filters_here"),
                   
            )
        )
        
      ),
      fluidRow(
        div(
          column(
            width = 12,
            div(id="n0_4", style="height:400px",
                #uiOutput("n_ui_intersect"),
                req(is.null(rv$df_n)==F),
                div(id = "n_ui_intersect",
                    
                    #----------------- venn --------------------
                    
                    conditionalPanel("output.n_venn_status == 'ok'",
                                     ins_venn_panel(),
                                     #uiOutput("ins_venn_panel"),
                                     #Tried to add anchor for venn but doesn't work
                                     #div(id="venn_plot_here"),
                    ),
                    
                    conditionalPanel("output.n_venn_status == 'no'",
                                     uiOutput("n_venn_placeholder")
                    ),
                    
                    
                    
                    #----------------- upset --------------------
                    conditionalPanel("output.n_venn_status == 'ok'",
                                     ins_upset_panel(),
                    ),
                    conditionalPanel("output.n_venn_status == 'no'",
                                     uiOutput("n_upset_placeholder")
                    ),
                    
                    
                    
                )
                #div(id="ins_graph_here"),
                
            )
            
          )
        )
      ),
      
      fluidRow(
        column(12,
               box(
                 width = 12, status = "primary",solidHeader = F,
                 title = span(strong("3.2."),icon("table"),"Intersection of Interest"),
                 div(id= "ins_pg_bottom"),
               )
               
        )
      )
    )
  }
})