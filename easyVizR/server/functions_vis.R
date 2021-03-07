#======================================================================#
####                3.1A.1 NON AREA PROPORTIONAL VENN                          ####
#======================================================================#

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



#======================================================================#
####                3.1A.2 AREA PROPORTIONAL VENN                          ####
#======================================================================#


#------------------- eulerR area proportional venn
# draws an eulerr diagram with intersection highlighted
#------------------------------------------
# gls: named list of vectors
# ins: named vector
draw_eulerr_with_ins <- function(gls, ins, print_mode="counts", show_ins=T, ins_color="red", base_colors=palette,
                                 adjust_labels=T, lb_limit=20){
  # apply linebreaks
  names(gls) <- addlinebreaks2(names(gls), lb_limit, "\n")
  names(ins) <- addlinebreaks2(names(ins), lb_limit, "\n")
  
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


#======================================================================#
####                3.1B UPSET PLOT                          ####
#======================================================================#

####-------------------- Upset plot ------------------------####

# draws a upsetR plot, highlighting the selected intersection
#---------------------------------------------------
# df must be a T/F or 0/1 matrix (i.e. fromList(gls))
# names(criteria) should match the column names of df
# text_scale: c(intersection size title, intersection size tick labels, set size title, set size tick labels, set names, numbers above bars)


draw_upsetR_with_ins <- function(df, criteria, show_ins=T, color="red", 
                                 empty_intersections=F, 
                                 order_by="freq", 
                                 text_scale=c(1.3, 2, 1.3, 1, 1.1, 2),
                                 lb_limit=20
                                 
){
  req(is.null(show_ins)==F)
  if(show_ins==T){
    
    # ---------------- apply linebreaks
    names(criteria) <- addlinebreaks2(names(criteria), lb_limit, "\n")
    colnames(df) <- addlinebreaks2(colnames(df), lb_limit, "\n")
    
    #----------------- parse criteria
    ins_true <- names(criteria[criteria==T & is.na(criteria)==F])
    ins_false <- names(criteria[criteria==F & is.na(criteria)==F])
    ins_na <- names(criteria[is.na(criteria)==T])
    
    # ---------------- get all iterations
    
    # build truth table for NA sections
    # function to apply permutations to the selected # of n
    if (length(ins_na)>0){
      my_permute <- function(vect, n, repeats = TRUE) {
        gtools::permutations(length(vect), n, vect, repeats.allowed = repeats)
      }
      x <- my_permute(vect=c(T,F), n=length(ins_na))
      colnames(x) <- ins_na
      
      # build possible combinations
      ins_iterations <- list()
      for (i in 1:nrow(x)){
        add = x[i,]
        add <- add[add==T]
        addname <- names(add)
        toadd <- c(ins_true, addname)
        ins_iterations <- c(ins_iterations, list(toadd))
      }
    } else {
      ins_iterations <- list(ins_true)
    }
    
    
    sel_ins=vector(mode="list", length=length(ins_iterations))
    
    for (i in 1:length(ins_iterations)){
      
      # check if there are genes in this intersection 
      check_range <- df[,names(criteria)] # extract the relevant columns
      check_range <- as.data.frame(lapply(check_range, as.logical))
      # print(check_range)
      # make a ref criteria vector
      ref <- rep(F, length(criteria))
      names(ref) <- names(criteria)
      ref[ins_iterations[[i]]] <- T
      # get matching rows
      found <- match_skipna_rows(check_range, ref)
      
      # if the combination is not empty, write it to list. if empty, leave as null.
      if (is.null(found)==F){ 
        sel_ins[[i]] <- list(query = intersects,
                             params= as.list(ins_iterations[[i]]), color=color, active=T)
      }
    }
    # finally remove the combinations that are not found (shown in list as null element)
    sel_ins[sapply(sel_ins, is.null)] <- NULL
  } else {
    sel_ins=NULL
  }
  
  if (length(sel_ins)==0){ sel_ins <- NULL }
  
  if (empty_intersections==T){
    upset(df, 
          order.by = order_by,
          queries = sel_ins, 
          empty.intersections = "on",
          text.scale = text_scale
    )
  } else if (empty_intersections==F) {
    upset(df, 
          order.by = order_by,
          queries = sel_ins, 
          text.scale = text_scale
    )
  }
  
}




#======================================================================#
####                    3.2.1 WORDCLOUD                          ####
#======================================================================#
####--------------------wordcloud for pathways------------------------####

# Set up color palette
#-----------------------------------------
map2color<-function(x,pal,limits=NULL){
  if(is.null(limits)) limits=range(x)
  pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}


# compute frequency table for wordcloud
#-----------------------------------------
generate_wc_freq_table <- function(vec, # vector of names
                                   sep, # separator for subdividing names into words
                                   ignored, # string that specifies ignored words
                                   ignored_sep="\\s+" # separator for ignored
){
  
  words <- unlist(lapply(vec, function(x){
    toupper(unlist(strsplit(x, sep)))
  }))
  words <- gsub("[[:punct:]]$", "", words) # get rid of punctuations at end
  words <- gsub("^[[:punct:]]", "", words) # get rid of punctuations at beginning
  
  df <- data.frame(table(words))
  
  # ignore certain words (supports regex pattern!!)
  ignore <- unlist(strsplit(ignored, ignored_sep))
  # ignore <- c("GO", "KEGG", "of", "and", "pathway")
  # df <- df[df$words %in% ignore ==F,]
  pattern <- paste(ignore, collapse = "|")
  df <- df[-grep(pattern, df$words, ignore.case=TRUE),]
}

# generate wordcloud plot from table
#------------------------------------------
generate_wc_plot <- function(freq_table, # freq table with colnames=c("words", "Freq")
                             max_words=150,
                             min_freq=1,
                             scale=c(3,.5),
                             rot_per=0.2 # freq of vertical words
){
  df <- freq_table
  # only draw top x words
  df <- df[order(df$Freq, decreasing = TRUE), ]
  if (nrow(df)>max_words){
    df <- df[1:max_words, ] # default max is 150
  }
  
  # print(head(df))
  wordcloud(df$words, df$Freq, 
            min.freq = min_freq, max.words=max_words, scale=scale,
            random.order=FALSE, 
            rot.per=rot_per, 
            colors=brewer.pal(8, "Dark2"))
}

#======================================================================#
####                       4,1 HEATMAP                          ####
#======================================================================#

# draw heatmap
#----------------------------------------

draw_heatmap <- function(df, 
                         sortby_dataset, # sort by which dataset (e.g. dataset1)
                         sortby_statistic, # sort by which statistic (e.g. PValue)
                         show_ylabs=F, # whether or not to show Y labels
                         ylabs_len, # max length of y labels
                         hovertext_linebreak = 30, # max line length for hovertexts
                         sharedcols = rv$n_sharedcols,
                         nx_n=rv$nx_n
){

  
  rownames(df) <- df$Name # put genename as index
  
  # order by selected column
  sortby_coln <- paste0(sortby_dataset,"_", sortby_statistic)
  df <- df[order(-df[sortby_coln]),] 
  names <- df$Name # preserve formatting in vector
  
  # extract plotted values
  to_match <- paste0(sortby_dataset, "_")
  plotted <- data.frame(t(dplyr::select(df,contains(to_match))))
  req(nrow(plotted) > 0)
  
  rownames(plotted) <- stat_replace1(rownames(plotted), nx_n, mode="each")
  # print(head(plotted))
  
  # make matrix for plot
  ylabls <- addlinebreaks(names,hovertext_linebreak,"<br>",do_end=F)
  
  # # if ylabel is shown, abbreviate # disabled as it messes up y order
  # if (show_ylabs==T){
  #   ylabls <- abbreviate_vector(ylabls, ylabs_len)
  # }

  dat <- expand.grid(x = rownames(plotted), y = ylabls)
  dat$z <- unlist(plotted)

  validate(need(length(dat$z)>0, select_ins_empty_msg))
  
  # put all the shared columns in the hovertext (as many as you have).
  addlabel <- ""
  for (coln in sharedcols){
    le <- unlist(data.frame(t(dplyr::select(df,matches(paste0("^",coln,"_"))))))
    if (is.numeric(le)){
      le <- round(le,3)
    }
    else if (is.character(le)){
      le <- addlinebreaks(le,hovertext_linebreak,"<br>")
    }
    if (coln == "Stat"){
      replace_stat <- stat_replace1(rep("Stat", length(nx_n)), nx_n, mode="each")
      replace_coln <- rep(replace_stat, nrow(df))
      addlabel <- paste(addlabel, paste0(replace_coln, ": ", le), sep="<br>")
    } else {
      addlabel <- paste(addlabel, paste0(coln, ": ", le), sep="<br>")
    }
    
  }
  full_y= dat$y
  
  # define the hovertext
  textt <- paste(full_y, addlabel)
  

  
  fig <- plot_ly() %>%
    add_trace(data = dat, x = ~x, y = ~y, z = ~z, type = "heatmap",
              colorscale  = cscale_simple,zauto = T, zmid= 0, colorbar = list(title = stat_replace1(sortby_dataset, nx_n)),
              hoverinfo = 'text',
              text = textt)
  
  fig <- fig %>% layout(
    title= paste0("Heatmap (n=",ncol(plotted),")"),
    xaxis = list(title = "", showticklabels = T),
    yaxis = list(title = "", showticklabels = show_ylabs)
    # ,margin = list(l=200)
  )
  
  fig
}


#======================================================================#
####                       4.2.1 XY SCATTER                          ####
#======================================================================#

# plotly two way scatter
#--------------------------------------------

draw_xy_scatter <- function(to_plot_df,
                            selected,
                            plotmode = rv$nxy_sc_plotmode,
                            sig = rv$nxy_sig,
                            thresh = rv$nxy_thresh,
                            nxy_p = rv$nxy_p,
                            nxy_q = rv$nxy_q,
                            nxy_stat = rv$nxy_stat,
                            nxy_size = rv$nxy_sc_size,
                            logic = rv$n_sc_logic,
                            outline_width= rv$nxy_sc_outlinewidth,
                            outline_color = rv$nxy_sc_outlinecolor,
                            opacity = rv$nxy_sc_opacity,
                            colormode = rv$nxy_colormode,
                            df_n=rv$df_n
){
  # these are the plotted genes
  df_ins <- to_plot_df$Name 
  
  if (plotmode=="Focus"){ # plot intersection only
    df <- to_plot_df
  } else if (plotmode=="Context"){ # plot all genes
    df <- to_plot_df
    df <- rbind(df_n[-which(df_n$Name %in% df_ins),], df) # make sure the ins is plotted first
  }
  
  # initialize colnames
  sig= sig
  thresh <- thresh
  xsig <- paste(sig, selected[[1]], sep="_")
  ysig <- paste(sig, selected[[2]], sep="_")
  xstat <- paste("Stat", selected[[1]], sep="_")
  ystat <- paste("Stat", selected[[2]], sep="_")
  xp <- paste("PValue", selected[[1]], sep="_")
  yp <- paste("PValue", selected[[2]], sep="_")
  xq <- paste("FDR", selected[[1]], sep="_")
  yq <- paste("FDR", selected[[2]], sep="_")
  print(c(nxy_p, nxy_q, nxy_stat))
  pthresh <- nxy_p
  qthresh <- nxy_q
  statthresh <- nxy_stat
  
  req_cols(df, c(xsig, ysig, xstat, ystat, xp, yp, xq, yq))
  
  # initialize df
  df[df==0]<-0.00001 # replace 0 with 0.001
  
  # remove NAs tied to selected datasets
  df_before <- df
  df <- remove_nas_in_cols(df, selected)
  # report on diff
  diff_report <- report_df_diff(df_before, df, "Name", "XY Scatter",
                                reason=" because values are missing for either X or Y"
                                )
  # print(diff_report$report)
  rv$nxy_diff_report <- diff_report
  
  req(nrow(df)>0)
  
  # initialize custom variables
  discrete_c1 = "red"
  discrete_c2 = "black"
  discrete_c3 = "lightgray"
  size = nxy_size
  size1 = size+2 # default dot size, initialized to 5
  size2 = (nxy_size-1) + 3 # size multiplier for the color & size option; initialized to 2+3
  linewidth = outline_width
  linecolor=outline_color
  opacity=opacity # default 0.7
  
  
  # initialize marker settings as none
  df$color <- discrete_c3
  df$color <- as.factor(df$color)
  df$size <- size1
  
  if (colormode== "None"){
    df$color <- as.character(df$color)
    df$color[which(df$Name %in% df_ins)] <- discrete_c2
    df$color <- as.factor(df$color)
    
    marker_settings <- list(
      color= df$color, size= df$size, opacity=opacity,
      line = list(color = linecolor, width = linewidth))
    
  } else if (colormode== "Two colors"){ 
    
    # color by AND or OR logic
    df$color <- as.character(df$color)
    df$color[which(df$Name %in% df_ins)] <- discrete_c2 # dots in the genelist
    
    # highlighted dots
    if (logic == "Both"){
      df$color[which(
        df[[xp]] < pthresh & df[[xq]] < qthresh & abs(df[[xstat]]) > statthresh &
          df[[yp]] < pthresh & df[[yq]] < qthresh & abs(df[[ystat]]) > statthresh
      )] <- discrete_c1
    } else if (logic == "Either"){
      df$color[which(
        (df[[xp]] < pthresh & df[[xq]] < qthresh & abs(df[[xstat]]) > statthresh) |
          (df[[yp]] < pthresh & df[[yq]] < qthresh & abs(df[[ystat]]) > statthresh)
      )] <- discrete_c1
    }
    
    df$color <- as.factor(df$color)
    df$size <- size1
    
    marker_settings <- list(
      color= df$color, size= df$size, opacity=opacity,
      line = list(color = linecolor, width = linewidth))
  }
  else if (colormode== "Color and size"){
    
    df[df==0]<-0.00001 # replace 0 with 0.001
    df$color <- -log10(as.numeric(df[[xsig]]))
    df$color <- as.numeric(df$color)
    df$size <- -log10(as.numeric(df[[ysig]]))* size2
    #print(head(df))
    
    # background dots color
    df$color[-which(df$Name %in% df_ins)] <- discrete_c3
    df$size[-which(df$Name %in% df_ins)] <- size1
    
    marker_settings <- list(
      color= df$color, size= df$size,
      opacity=opacity, line = list(color = linecolor, width = linewidth),
      colorscale=cscale, cauto=F, cmin=0, cmax=3,
      colorbar=list(title=paste0('-log10(',sig,'(x))')))
  }
  
  # generate properties table only when two color mode is selected
  if (colormode!="Color and size"){
    summary_df <- summarize_factor_column(df, "color")
    rv$nxy_color <- summary_df
  }
  
  
  lm_fun <- paste0("`", xstat, "` ~ `", ystat, "`")
  rv$fit_nxy <- lm(lm_fun, data = df)
  
  # print(head(df))
  
  stat_replacements <- stat_replace1(rep("Stat",2), selected, mode="each")
  
  fig <- plot_ly(
    data = df, 
    x = df[[xstat]],
    y = df[[ystat]],
    type = 'scatter',
    mode = 'markers', 
    marker = marker_settings,
    hoverinfo="text",
    text=c(paste0(df$Name, 
                  "<br>",stat_replacements[[1]],"(x): ", round(df[[xstat]], 3),
                  "<br>p(x): ", round(df[[xp]], 3),
                  ", q(x): ", round(df[[xq]], 3),
                  "<br>",stat_replacements[[2]],"(y): ", round(df[[ystat]], 3),
                  "<br>p(y): ", round(df[[yp]], 3),
                  ", q(y): ", round(df[[yq]], 3)
    ))
  )
  fig <- fig %>% layout(title = paste0(selected[[1]], " vs ", selected[[2]], " (n=",nrow(df),")"),
                        yaxis = list(zeroline = T, title=stat_replace1(paste0("Stat_",selected[[2]]),selected[[2]])),
                        xaxis = list(zeroline = T, title=stat_replace1(paste0("Stat_",selected[[1]]),selected[[1]]))
  )
  
  return(fig)
}

#======================================================================#
####                       4.2.2 3D SCATTER                          ####
#======================================================================#


draw_3d_scatter <- function(to_plot_df,
                            selected,
                            plotmode = rv$nxyz_sc_plotmode,
                            pp = rv$n_3ds_p,
                            qq = rv$n_3ds_q,
                            ss = rv$n_3ds_Stat,
                            size = rv$nxyz_sc_size,
                            linewidth = rv$nxyz_sc_outlinewidth,
                            linecolor = rv$nxyz_sc_outlinecolor,
                            opacity=rv$nxyz_sc_opacity,
                            colormode = rv$nxyz_colormode,
                            logic = rv$nxyz_sc_logic,
                            df_n = rv$df_n
){
  
  # these are the plotted genes
  df_ins <- to_plot_df$Name 
  
  if (plotmode=="Focus"){ # plot intersection only
    df <- to_plot_df
  } else if (plotmode=="Context"){ # plot all genes
    df <- to_plot_df
    df <- rbind(df_n[-which(df_n$Name %in% df_ins),], df) # make sure the ins is plotted first
  }
  
  # remove NAs tied to selected datasets
  df_before <- df
  df <- remove_nas_in_cols(df, selected)
  # report on diff
  diff_report <- report_df_diff(df_before, df, "Name", "3D Scatter",
                                reason=" because values are missing for X, Y or Z"
  )
  # print(diff_report$report)
  rv$nxyz_diff_report <- diff_report
  
  
  # get col names
  statcols <- paste("Stat_", selected, sep="")
  pcols <- paste("PValue_", selected, sep="")
  qcols <- paste("FDR_", selected, sep="")
  pp <- pp
  qq <- qq
  ss <- ss
  
  discrete_c1a <- "red"
  discrete_c1b <- "blue"
  discrete_c2 <- "black"
  discrete_c3 <- "lightgray"
  size1 <- size-1 # default size
  linewidth = linewidth
  linecolor=linecolor
  opacity=opacity # default 0.7
  
  
  incProgress(0.2)
  
  # default color is here. conditionally color by threshold.
  df$color <- discrete_c3
  df$color[which(df$Name %in% df_ins)] <- discrete_c2
  # assign color by certain criteria
  if (colormode == "Two colors"){
    if (logic == "Both"){
      req(is.null(logic)==F)
      df$color <- ifelse(
        (df[[pcols[[1]]]] < pp & df[[qcols[[1]]]] < qq & df[[statcols[[1]]]] > ss) &
          (df[[pcols[[2]]]] < pp & df[[qcols[[2]]]] < qq & df[[statcols[[2]]]] > ss) &
          (df[[pcols[[3]]]] < pp & df[[qcols[[3]]]] < qq & df[[statcols[[3]]]] > ss), 
        discrete_c1a, df$color)
      df$color <- ifelse(
        (df[[pcols[[1]]]] < pp & df[[qcols[[1]]]] < qq & df[[statcols[[1]]]] < -ss) &
          (df[[pcols[[2]]]] < pp & df[[qcols[[2]]]] < qq & df[[statcols[[2]]]] < -ss) &
          (df[[pcols[[3]]]] < pp & df[[qcols[[3]]]] < qq & df[[statcols[[3]]]] < -ss), 
        discrete_c1b, df$color)
    } else if (logic == "Either"){
      df$color <- ifelse(
        (df[[pcols[[1]]]] < pp & df[[qcols[[1]]]] < qq & df[[statcols[[1]]]] > ss) |
          (df[[pcols[[2]]]] < pp & df[[qcols[[2]]]] < qq & df[[statcols[[2]]]] > ss) |
          (df[[pcols[[3]]]] < pp & df[[qcols[[3]]]] < qq & df[[statcols[[3]]]] > ss), 
        discrete_c1a, df$color)
      df$color <- ifelse(
        (df[[pcols[[1]]]] < pp & df[[qcols[[1]]]] < qq & df[[statcols[[1]]]] < -ss) |
          (df[[pcols[[2]]]] < pp & df[[qcols[[2]]]] < qq & df[[statcols[[2]]]] < -ss) |
          (df[[pcols[[3]]]] < pp & df[[qcols[[3]]]] < qq & df[[statcols[[3]]]] < -ss), 
        discrete_c1b, df$color)
    }
    df$color <- as.factor(df$color)
  }
  
  incProgress(0.2)
  
  stat_replacements <- stat_replace1(rep("Stat",3), selected, mode="each")
  
  # datapoint appearance
  marker_settings <- list(
    color = df$color, size=size1,
    opacity=opacity, line = list(color = linecolor, width = linewidth)
  )
  
  fig <- plot_ly(df, x = df[[statcols[[1]]]], y = df[[statcols[[2]]]], z = df[[statcols[[3]]]], marker = marker_settings,
                 hoverinfo="text",
                 text=c(paste0(
                   df$Name, 
                   "<br>",stat_replacements[[1]],"(x):", round(df[[statcols[[1]]]], 3),
                   "<br>p=", round(df[[pcols[[1]]]], 3),", q=", round(df[[qcols[[1]]]], 3),
                   "<br>",stat_replacements[[2]],"(y):", round(df[[statcols[[2]]]], 3),
                   "<br>p=", round(df[[pcols[[2]]]], 3),", q=", round(df[[qcols[[2]]]], 3),
                   "<br>",stat_replacements[[3]],"(z):", round(df[[statcols[[3]]]], 3),
                   "<br>p=", round(df[[pcols[[3]]]], 3),", q=", round(df[[qcols[[3]]]], 3)
                 )))
  
  incProgress(0.2)
  
  # generate properties table
  summary_df <- summarize_factor_column(df, "color")
  rv$n_3ds_prop <- summary_df
  
  fig <- fig %>% add_markers()
  fig <- fig %>% layout(title = paste0('3D Scatter, n=',nrow(df)),
                        scene = list(xaxis = list(title = paste0(statcols[[1]])),
                                     yaxis = list(title = paste0(statcols[[2]])),
                                     zaxis = list(title = paste0(statcols[[3]]))))
  
  # })
  fig
}
