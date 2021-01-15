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