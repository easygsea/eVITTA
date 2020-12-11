    #=======================================================================#
    ####----------------------- Functions: Calculation -------------------####
    #=======================================================================# 
    # change heximal colors to 90% transparency
    addalpha <- function(colors, alpha=0.25) {
        r <- col2rgb(colors, alpha=T)
        # Apply alpha
        r[4,] <- alpha*255
        r <- r/255.0
        return(rgb(r[1,], r[2,], r[3,], r[4,]))
    }
    
    addlinebreaks_vis <- function(x, max=50, lbtype="<br>"){
        x = gsub(paste0('(.{1,',max,'})(\\s|$)'), paste0('\\1',lbtype), x)
        return(x)
    }
    
    addlinebreaks <- function(x, max=6, lbtype="\n", no = 100){
        lapply(x, function(x){
            if(length(x)<=no){
                x = split(x, ceiling(seq_along(x)/max))
                x = lapply(x, function(x) paste(x, collapse = ", "))
                x = paste(x, collapse = lbtype)
                return(x)
            }else{
                x = x[1:no]
                x = split(x, ceiling(seq_along(x)/max))
                x = lapply(x, function(x) paste(x, collapse = ", "))
                x = paste(x, collapse = lbtype)
                return(paste0(x,"\n... ..."))
            }
           
            }) 
    }
    
    data_summary <- function(x,k=rv$k) {
        m <- mean(x)
        ymin <- m - k * sd(x)
        ymax <- m + k * sd(x)
        return(c(y=m,ymin=ymin,ymax=ymax))
    }
    
    combine_df <- function(df=rv$fgseagg,db_selected=unlist(gs_selected())){
        df <- df %>% 
            dplyr::filter(db %in% db_selected) %>% 
            dplyr::select(-db) %>% 
            dplyr::arrange(padj)
        
        df[[ncol(df)]] = lapply(df[[ncol(df)]], function(x) paste(x,collapse = ";"))
        
        return(df)
    }
    
    filter_df <- function(df = combine_df(), p=rv$tl_p, q=rv$tl_q, direction=rv$tl_ES){
        if(p<1){
            df = df %>% dplyr::filter(pval<p)
        }
        
        if(q<1){
            df = df %>% dplyr::filter(padj < q)
        }
        
        if(direction == "up"){
            df = df %>% dplyr::filter(ES > 0)
        }else if(direction == "down"){
            df = df %>% dplyr::filter(ES < 0)
        }
        
        df[[ncol(df)]] = lapply(df[[ncol(df)]], function(x) paste(x,collapse = ";"))
        # # get rid of db id
        # df$pathway = unlist(lapply(df$pathway,function(x){unlist(strsplit(x,"%(?=[^%]+$)",perl=TRUE))[[1]]}))
        return(df)
    }
    
    # combine_df <- function(df=rv$fgseagg){
    #     df <- df[[names(df) %in% unlist(gs_selected())]]
    #     df <- rbindlist(df)
    #     df <- df[order(padj)]
    #     df <- df %>%
    #         mutate_if(is.numeric, function(x) round(x, digits=3))
    #     return(df)
    # }
    
    #=======================================================================#
    ####------------------------ Functions: Plot ------------------------####
    #=======================================================================#
    plot_confirm_btn <- function(id,label,style="simple",size="md",color="warning",icon = NULL,block=F){
      actionBttn(
        id,HTML(sprintf("<b>%s</b>",label)),
        color=color,style=style,size=size,icon=icon,block=block
      )
      
    }
    enrichmentplot <- function() {
        ranks = rv$rnkgg
        names(ranks) = toupper(names(ranks))
        gmt = rv$gmts[[rv$es_term]] %>% toupper(.)
        plotEnrichment(gmt,ranks) + labs(title = rv$es_term)
    }
    
    filter_plot_df <- function(pathways, up, down, cutoff_p, cutoff_q){
      df = rv$fgseagg %>% dplyr::filter(!(is.na(pval)))
      
      df = df %>% 
        dplyr::filter(db %in% pathways) %>% 
        mutate_if(is.numeric,  ~replace(., . == 0, p_min)) %>%
        dplyr::arrange(padj)
      
      if(cutoff_p < 1){
        df = df %>% dplyr::filter(pval < cutoff_p)
      }
      if(cutoff_q < 1){
        df = df %>% dplyr::filter(padj < cutoff_q)
      }
      
      if(is.null(df)==T || nrow(df)<1){
        return(NULL)
      }else{
        if(rv$run_mode == "gsea"){
          df1 <- df %>% dplyr::filter(ES > 0)
          if(is.null(up)==F){
            df1 <- df1 %>%
              # dplyr::slice_max(ES,n=up)
              dplyr::arrange(-ES) %>%
              dplyr::slice_head(n=up)
          }
          
          df2 <- df  %>% dplyr::filter(ES < 0)
          if(is.null(down)==F){
            df2 <- df2 %>%
              # dplyr::slice_min(ES,n=down)
              dplyr::arrange(ES) %>%
              dplyr::slice_head(n=down)
          }
          
          df <- rbind(df1,df2)
          df <- df %>% arrange(desc(ES))
        }else if(rv$run_mode == "glist"){
          if(is.null(up)==F){
            df <- df %>%
              dplyr::slice_min(padj,n=up)
          }
        }
        return(df)
      }
    }
    
    bar_plot <- function(pathways=rv$bar_pathway,up=rv$bar_up,down=rv$bar_down,pq=rv$bar_pq,cutoff_p=rv$bar_p_cutoff,cutoff_q=rv$bar_q_cutoff,abby=rv$bar_abb,abbn=rv$bar_abb_n){
        if(is.null(pathways)==T){
            return(NULL)
        }else{
          if(rv$bar_mode == "cutoff"){
            df <- filter_plot_df(pathways,up,down,cutoff_p,cutoff_q)
          }else if(rv$bar_mode == "gs"){
            df <- rv$fgseagg %>%
              dplyr::filter(pathway %in% rv$gss_selected) %>%
              arrange(desc(ES))
          }
          
          if(is.null(df)==T || nrow(df)<1){
            rv$bar_error <- "0"
            return(NULL)
          }else if(nrow(df)>200){
            rv$bar_error <- "l"
            return(NULL)
          }else{
            size_g = unlist(lapply(df[[ncol(df)]], function(x) length(x)))
            
            rv$bar_pathway_list = df[["pathway"]]
            
            # get rid of db id
            y_pathway = unlist(lapply(df$pathway,function(x){unlist(strsplit(x,"%(?=[^%]+$)",perl=TRUE))[[1]]}))
            
            # abbreviate gene set names on y axis if too long
            if(abby == "y"){
              y_pathway = lapply(y_pathway, function(x){if(nchar(x)<abbn){return(x)}else{return(paste0(substr(x,0,abbn),"..."))}})
            }
            
            fig <- df %>% 
              ggplot(aes(x=ES, y=factor(pathway, levels=pathway), fill=-log10(df[[pq]])*sign(ES),
                         text=paste0(
                           "<b>",df[["pathway"]],"</b>\n",
                           "ES=",signif(df[["ES"]],digits=3),"; ",
                           "P=",signif(df[["pval"]],digits=3),"; ",
                           "P.adj=",signif(df[["padj"]],digits=3),"\n",
                           tail(colnames(df),n=1)," (",size_g,"/",df[["size"]],"): \n",addlinebreaks(df[[ncol(df)]])
                           
                         ))) +
              geom_bar(stat="identity", width = 0.8) +
              scale_fill_gradientn(limits = c(-3,3),colours=gcols, values=gvalues, name=paste0("-log10(",pq,")*sign(ES)"), oob=squish) +
              xlab("Enrichment Score (ES)") + ylab("") +
              geom_vline(xintercept=0, size=0.1) +
              theme_minimal() +
              theme(axis.text.y = element_text(size=10),
                    legend.title = element_text(size = 9)) +
              scale_y_discrete(labels = y_pathway)
            
            yh = nrow(df) * 18 + 50
            if(yh<=rv$box_hp){yh=rv$box_hp}
            
            fig <- ggplotly(fig,
                            height = yh,
                            tooltip = "text",
                            source = "bar_plot_click"
            ) %>%
              # layout(legend=list(colorbar=list(side="right"))) %>%
              event_register("plotly_click")
            
            return(fig)
          }
        }
    }
    
    bubble_plot <- function(pathways=rv$bar_pathway,up=rv$bar_up,down=rv$bar_down,pq=rv$bar_pq,cutoff_p=rv$bar_p_cutoff,cutoff_q=rv$bar_q_cutoff,abby=rv$bar_abb,abbn=rv$bar_abb_n,zmin=rv$bubble_zmin,zmax=rv$bubble_zmax){
        if(is.null(pathways)==T){
            return(NULL)
        }else{
          if(rv$bar_mode == "cutoff"){
            df <- filter_plot_df(pathways,up,down,cutoff_p,cutoff_q)
          }else if(rv$bar_mode == "gs"){
            df <- rv$fgseagg %>%
              dplyr::filter(pathway %in% rv$gss_selected) %>%
              arrange(desc(ES))
          }
          
          if(is.null(df)==T || nrow(df)<1){
            rv$bar_error <- "0"
            return(NULL)
          }else if(nrow(df)>200){
            rv$bar_error <- "l"
            return(NULL)
          }else{
              df <- df %>%
                dplyr::filter(pathway %in% rv$gss_selected)
              
                rv$bubble_pathway_list = df[["pathway"]]
                
                # get rid of db id
                y_pathway = unlist(lapply(df$pathway,function(x){unlist(strsplit(x,"%(?=[^%]+$)",perl=TRUE))[[1]]}))
                
                # abbreviate gene set names on y axis if too long
                if(abby == "y"){
                    y_pathway = lapply(y_pathway, function(x){if(nchar(x)<abbn){return(x)}else{return(paste0(substr(x,0,abbn),"..."))}})
                }
                size_g = unlist(lapply(df[[ncol(df)]], function(x) length(x)))

                # values = size_g
                # values<-values[!is.na(values)]
                # zmax = max(values) / 10
                # zmin = min(values) / 10

                fig <- df %>% 
                    ggplot(aes(x=ES, y=factor(pathway, levels=pathway), size=size_g, color=-log10(df[[pq]])*sign(ES),
                               text=paste0(
                                   "<b>",df[["pathway"]],"</b>\n",
                                   "ES=",signif(df[["ES"]],digits=3),"; ",
                                   "P=",signif(df[["pval"]],digits=3),"; ",
                                   "P.adj=",signif(df[["padj"]],digits=3),"\n",
                                   tail(colnames(df),n=1)," (",size_g,"/",df[["size"]],"): \n",addlinebreaks(df[[ncol(df)]])
                                   
                               ))) +
                    geom_point(alpha=0.5) +
                    scale_size(range = c(zmin, zmax)) +
                    scale_color_gradientn(limits = c(-3,3),colours=gcols, values=gvalues, name=paste0("-log10(",pq,")*sign(ES)"), oob=squish) +
                    xlab("Enrichment Score (ES)") + ylab("") +
                    geom_vline(xintercept=0, size=0.1) +
                    theme_minimal() +
                    theme(axis.text.y = element_text(size=10),
                          legend.title = element_text(size = 9)) +
                    scale_y_discrete(labels = y_pathway)
                
                    
                yh = nrow(df) * 18 + 50
                if(yh<=rv$box_hp){yh=rv$box_hp}
                
                fig <- ggplotly(fig,
                                height = yh,
                                tooltip = "text",
                                source = "bubble_plot_click"
                ) %>%
                    event_register("plotly_click")
                
                
                return(fig)
            }
            
        }
    }
    
    volcano_plot <- function(pathways=rv$volcano_pathway,pq=rv$volcano_pq,cutoff_p=rv$volcano_p_cutoff,cutoff_q=rv$volcano_q_cutoff) {
        if(is.null(pathways)){
            return(NULL)
        }else{
            df = rv$fgseagg %>% 
                dplyr::filter(db %in% pathways) %>% 
                mutate_if(is.numeric,  ~replace(., . == 0, p_min))
            
            size_g = unlist(lapply(df[[ncol(df)]], function(x) length(x)))
            
            # temporarily save pathway order into rv
            rv$volcano_pathway_list = df$pathway
            
            fig <- df %>% 
                ggplot(aes(x=ES, y=-log10(df[[pq]]), color=-log10(df[[pq]])*sign(ES),
                           text=paste0(
                               "<b>",df[["pathway"]],"</b>\n",
                               "ES=",signif(df[["ES"]],digits=3),"; ",
                               "P=",signif(df[["pval"]],digits=3),"; ",
                               "P.adj=",signif(df[["padj"]],digits=3),"\n",
                               tail(colnames(df),n=1)," (",size_g,"/",df[["size"]],"): \n",addlinebreaks(df[[ncol(df)]])
                               
                           ))) +
                geom_point(alpha=0.5) +
                # geom_text_repel(aes(x = ES, y =-log10(df[[pq]]), label = ifelse(pval<cutoff_p && padj<cutoff_q, pathway,""))) +
                scale_color_gradientn(limits = c(-3,3),colours=gcols, values=gvalues, name=paste0("-log10(",pq,")*sign(ES)"), oob=squish) +
                xlab("Enrichment Score (ES)") + ylab(paste0("-log10(",pq,")")) +
                geom_vline(xintercept=0, size=0.1) +
                theme_minimal() +
                theme(#axis.text.y = element_text(size=10),
                      legend.title = element_text(size = 9))
            
            
            
            fig <- ggplotly(fig,tooltip = "text",
                            source = "volcano_plot_click"
            ) %>%
                event_register("plotly_click")
            
            return(fig)
        }
    }
    
    # discrete plotly volcano plot
    volcano_plot2 <- function(pathways=rv$volcano_pathway,pq=rv$volcano_pq,cutoff=rv$volcano_cutoff) {
        if(is.null(pathways)){
            return(NULL)
        }else{
            df = rv$fgseagg %>% 
                dplyr::filter(db %in% pathways) %>% 
                mutate_if(is.numeric,  ~replace(., . == 0, p_min))
            
            df = df[order(df[[pq]]),]
            
            size_g = unlist(lapply(df[[ncol(df)]], function(x) length(x)))
            
            # temporarily save pathway order into rv
            rv$volcano_pathway_list = df$pathway
            
            colors = rep("grey",nrow(df))
            # if(cutoff < 1){
                colors[df[[pq]] < cutoff] = "red"
            # }else{
            #     colors[df[[pq]] <= cutoff] = "red"
            # }
            
            es_volcano <- plot_ly(
                x = df$ES,
                y = -log10(df[[pq]]),
                # color = df[[pq]] < cutoff, colors = c("grey","red"),
                type = "scatter",
                mode = 'markers', marker = list(color = colors),
                hoverinfo="text",
                text=c(paste0(
                            "<b>",df[["pathway"]],"</b>\n",
                            "ES=",signif(df[["ES"]],digits=3),"; ",
                            "P=",signif(df[["pval"]],digits=3),"; ",
                            "P.adj=",signif(df[["padj"]],digits=3),"\n",
                            tail(colnames(df),n=1)," (",size_g,"/",df[["size"]],"): \n",addlinebreaks(df[[ncol(df)]])
                )),
                source = "volcano_plot_click2"
            )
            
            es_volcano <- es_volcano %>% layout(#title = paste0("Volcano plot of enrichment scores", rv$show_df, " (n=",nrow(df),")"),
                                                yaxis = list(zeroline = T, title=paste0("-log10(",pq,")"),
                                                             range=c(0,max(-log10(df[[pq]]), na.rm = TRUE)+0.2)),
                                                xaxis = list(zeroline = T, title="Enrichment score (ES)", 
                                                             range=c(-1,1)))
            return(es_volcano)
        }
    }
    
    # discrete plotly volcano plot
    volcano_plot3 <- function(pathways=rv$volcano_pathway,pq=rv$volcano_pq,cutoff=rv$volcano_cutoff,no_down=rv$volcano_top_down,no_up=rv$volcano_top_up) {
        if(is.null(pathways)){
            return(NULL)
        }else{
            df = rv$fgseagg %>% 
                dplyr::filter(db %in% pathways) %>% 
                mutate_if(is.numeric,  ~replace(., . == 0, p_min))
            
            y_pathway = unlist(lapply(df$pathway,function(x){unlist(strsplit(x,"%(?=[^%]+$)",perl=TRUE))[[1]]}))
            
            # threshold by p & q cutoffs
            if(cutoff < 1){
                threshold_OE <- df[[pq]] < cutoff
            }else{
                threshold_OE <- df[[pq]] <= cutoff
            }
            df$threshold <- threshold_OE
            
            df$genelabels = ""
            
            df$genelabels[which(df$threshold==TRUE)] = y_pathway[which(df$threshold==TRUE)]
            
            # # order df by top down regulations
            # df_ordered = df[order(df[["ES"]],df[[pq]]),]
            # y_pathway = unlist(lapply(df_ordered$pathway,function(x){unlist(strsplit(x,"%(?=[^%]+$)",perl=TRUE))[[1]]}))
            # labels_down = rev(y_pathway[1:no_down])
            # 
            # # order df by top down regulations
            # df_ordered = df[order(-df[["ES"]],df[[pq]]),]
            # y_pathway = unlist(lapply(df_ordered$pathway,function(x){unlist(strsplit(x,"%(?=[^%]+$)",perl=TRUE))[[1]]}))
            # labels_up = y_pathway[1:no_up]
            # 
            # # calculate # not labeled
            # no_unlabel = nrow(df_ordered) - no_down - no_up
            # 
            # # create genelabels
            # df_ordered$genelabels = c(labels_up,rep("",no_unlabel),labels_down)

            # 
            # threshold_OE = df_ordered$genelabels != ""
            # length(which(threshold_OE))
            
            # fig <- ggplot(df_ordered) +
            #     geom_point(aes(x=ES,y=-log(df_ordered[[pq]]),colour=threshold_OE)) +
            #     scale_colour_manual(values = c("grey","red")) +
            #     geom_text_repel(data = df_ordered[which(threshold_OE),],aes(x=ES,y=-log(df_ordered[which(threshold_OE),][[pq]]),label=genelabels)) +
            #     xlab("Enrichment Score (ES)") + ylab(paste0("-log10(",pq,")")) +
            #     geom_vline(xintercept=0, size=0.1) +
            #     theme_minimal() +
            #     theme(legend.position = "none",
            #           plot.title = element_text(size = rel(1.5), hjust = 0.5),
            #           axis.title = element_text(size = rel(1.25)))
            
            fig <- ggplot(df) +
                geom_point(aes(x=ES,y=-log(df[[pq]]),colour=threshold)) +
                scale_colour_manual(values = c("grey","red")) +
                geom_text_repel(data = df[which(threshold),],aes(x=ES,y=-log(df[which(threshold),][[pq]]),label=genelabels)) +
                xlab("Enrichment Score (ES)") + ylab(paste0("-log10(",pq,")")) +
                geom_vline(xintercept=0, size=0.1) +
                theme_minimal() +
                theme(legend.position = "none",
                      plot.title = element_text(size = rel(1.5), hjust = 0.5),
                      axis.title = element_text(size = rel(1.25)))
            
            return(fig)
        }
    }
    
    word_plot <- function(pathways=rv$bar_pathway,cutoff_p=rv$bar_p_cutoff,cutoff_q=rv$bar_q_cutoff,ntop=rv$n_word){
      if(is.null(pathways)==T){
        return(NULL)
      }else{
        df = rv$fgseagg %>% dplyr::filter(!(is.na(pval)))
        
        df = df %>% 
          dplyr::filter(db %in% pathways) %>% 
          mutate_if(is.numeric,  ~replace(., . == 0, p_min))
        
        if(cutoff_p < 1){
          df = df %>% dplyr::filter(pval < cutoff_p)
        }
        if(cutoff_q < 1){
          df = df %>% dplyr::filter(padj < cutoff_q)
        }
        
        if(is.null(df)==T || nrow(df)<1){
          return(NULL)
        }else{
          # transform df to tibble
          data <- df %>% 
            as_tibble() %>%
            dplyr::select(-db) %>%
            dplyr::arrange(padj) %>%
            mutate_if(is.numeric, function(x) round(x, digits=3))
          
          if(rv$run_mode == "glist"){
            # create data for word freq count plots
            data <- data %>%
              dplyr::mutate(linenumber = row_number(),text = pathway) %>%
              dplyr::select(text,linenumber)
            
            data$text <- lapply(data$text,function(x) strsplit(x,"%")[[1]][1]) %>%
              lapply(.,function(x){
                if(grepl("_",x))
                  regmatches(x, regexpr("_", x), invert = TRUE)[[1]][2]
                }) %>%
              lapply(., function(x) gsub("_"," ",x)) %>%
              unlist(.)
            
            # tidy and count data
            data <- data %>%
              unnest_tokens(word, text) %>%
              dplyr::anti_join(stop_words) %>%
              dplyr::anti_join(useless_words) %>%
              dplyr::filter(is.na(as.numeric(word))) %>%
              dplyr::count(word,sort=TRUE)
            
            data <- data %>%
              dplyr::arrange(desc(n))

            tidy_data <- data %>%
              dplyr::mutate(word = factor(word, levels = rev(unique(word)))) %>%
              top_n(ntop)
            
            # hover text
            text = lapply(tidy_data$word, function(x){
              x = as.character(droplevels(x))
              a = df %>%
                dplyr::filter(str_detect(pathway,regex(x, ignore_case = TRUE))) %>%
                dplyr::select(pathway) %>%
                unlist(.) %>%
                unname(.)
              if(length(a)>14){a = c(a[1:14],"... ...")}
              a = paste(a,collapse = "\n")
              return(a)
            })
            
            text = unlist(text)
            
            p <- tidy_data %>%
              ggplot(aes(word, n, text=text)) +
              geom_col(show.legend = FALSE, fill = "#F8766D") +
              labs(x = NULL, y = NULL, title = NULL) +
              coord_flip() +
              scale_x_reordered() +
              theme(
                plot.title = element_text(size = 10,face = "bold",vjust=0) #hjust = 0.5
              )
            
            # adjust plot height
            lth = nrow(tidy_data) * 18 + 50
            if(lth<=rv$box_hp){lth=rv$box_hp}
            
            p <- ggplotly(p, height = lth,
                          # margin=dict(
                          #     l=250,
                          #     r=0,
                          #     b=0,
                          #     t=50,
                          #     pad=0
                          # ),
                          tooltip=c("word","n","text"))
            
            return(p)
          }else if(rv$run_mode == "gsea"){
            # create data for word freq count plots
            data <- data %>%
              dplyr::mutate(linenumber = row_number(),text = pathway) %>%
              dplyr::select(ES,text,linenumber)
            
            data1 <- data %>% dplyr::filter(ES>0)
            data2 <- data %>% dplyr::filter(ES<0)
            
            tidy_data = NULL

            for(ESsign in c(-1,1)){
              if(ESsign == 1){i0 = data1}else{i0 = data2}
              if(nrow(i0) < 1){next}
              i = i0

              i$text <- lapply(i$text,function(x) strsplit(x,"%")[[1]][1]) %>%
                lapply(.,function(x) regmatches(x, regexpr("_", x), invert = TRUE)[[1]][2]) %>%
                lapply(., function(x) gsub("_"," ",x)) %>%
                unlist(.)
              
              # tidy and count data
              i <- i %>%
                unnest_tokens(word, text) %>%
                dplyr::anti_join(stop_words) %>%
                dplyr::anti_join(useless_words) %>%
                dplyr::filter(is.na(as.numeric(word))) %>%
                dplyr::count(word,sort=TRUE)
              
              if(ESsign == 1){
                i = i %>%
                  dplyr::arrange(n)
              }else{
                i = i %>%
                  dplyr::arrange(desc(n))
              }
              
              tidy_data0 <- i %>%
                dplyr::mutate(word = factor(word, levels = rev(unique(word)))) %>%
                top_n(ntop) %>%
                dplyr::mutate(n = n * ESsign)
              
              if(ESsign == 1){
                tidy_data0$color = "Up"
              }else{
                tidy_data0$color = "Down"
              }

              # hover text
              text0 = lapply(tidy_data0$word, function(x){
                x = as.character(droplevels(x))
                a = i0 %>%
                  dplyr::filter(str_detect(text,regex(x, ignore_case = TRUE))) %>%
                  dplyr::select(text) %>%
                  unlist(.) %>%
                  unname(.)
                if(length(a)>14){a = c(a[1:14],"... ...")}
                a = paste(a,collapse = "\n")
                return(a)
              })
              
              text0 = unlist(text0)
              
              tidy_data0$text = text0
              tidy_data = rbind(tidy_data,tidy_data0)
            }
            
            p <- tidy_data %>%
              dplyr::arrange(n) %>%
              dplyr::mutate(word = factor(word, levels = rev(unique(word)))) %>%
              ggplot(aes(word, n, text=text, fill = color)) +
              geom_col(show.legend = FALSE) +
              labs(x = NULL, y = NULL, title = NULL) +
              coord_flip() +
              scale_x_reordered() +
              scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
              theme(
                plot.title = element_text(size = 10,face = "bold",vjust=0) #hjust = 0.5
              )
            
            # adjust plot height
            lth = nrow(tidy_data) * 18 + 50
            if(lth<=rv$box_hp){lth=rv$box_hp}
            
            p <- ggplotly(p, height = lth,
                          # margin=dict(
                          #     l=250,
                          #     r=0,
                          #     b=0,
                          #     t=50,
                          #     pad=0
                          # ),
                          tooltip=c("word","n","text"))
            
            return(p)
          }
          
          
        }
      }
    }
    
    # glist bar bubble volcano -------------------
    bar_plot2 <- function(pathways=rv$bar_pathway,up=rv$bar_up,down=rv$bar_down,pq=rv$bar_pq,cutoff_p=rv$bar_p_cutoff,cutoff_q=rv$bar_q_cutoff,abby=rv$bar_abb,abbn=rv$bar_abb_n){
        if(is.null(pathways)==T){
            return(NULL)
        }else{
          df = filter_plot_df(pathways, up, down, cutoff_p, cutoff_q)
          
            # df = rv$fgseagg %>% 
            #     dplyr::filter(db %in% pathways) %>% 
            #     mutate_if(is.numeric,  ~replace(., . == 0, 0.00001)) %>%
            #   dplyr::arrange(padj)
            # 
            # if(cutoff_p < 1){
            #     df = df %>% dplyr::filter(pval<cutoff_p)
            # }
            # if(cutoff_q < 1){
            #     df = df %>% dplyr::filter(padj<cutoff_q)
            # }
            

            if(is.null(df)==T || nrow(df)<1){
                return(NULL)
            }else{
                
                # df <- df %>%
                #   dplyr::slice_min(padj,n=up)
                
                rv$bar_pathway_list = df[["pathway"]]
                
                # get rid of db id
                y_pathway = unlist(lapply(df$pathway,function(x){unlist(strsplit(x,"%(?=[^%]+$)",perl=TRUE))[[1]]}))
                
                # abbreviate gene set names on y axis if too long
                if(abby == "y"){
                    y_pathway = lapply(y_pathway, function(x){if(nchar(x)<abbn){return(x)}else{return(paste0(substr(x,0,abbn),"..."))}})
                }
                
                fig <- df %>% 
                    ggplot(aes(x=-log10(df[[pq]]), y=factor(pathway, levels=pathway), fill=-log10(df[[pq]]),
                               text=paste0(
                                   "<b>",df[["pathway"]],"</b>\n",
                                   "P=",signif(df[["pval"]],digits=3),"; ",
                                   "P.adj=",signif(df[["padj"]],digits=3),"\n",
                                   tail(colnames(df),n=1)," (",df$overlap,"/",df[["size"]],"): \n",addlinebreaks(df[[ncol(df)]])
                                   
                               ))) +
                    geom_bar(stat="identity", width = 0.8) +
                    scale_fill_gradientn(limits = c(0,3),colours=gcols2, values=gvalues2, name=paste0("-log10(",pq,")"), oob=squish) +
                    xlab(paste0("-log10(",pq,")")) + ylab("") +
                    xlim(0,max(-log10(df[[pq]]))+0.5) +
                    theme_bw() +
                    theme(axis.text.y = element_text(size=10),
                          legend.title = element_text(size = 9)) +
                    scale_y_discrete(labels = y_pathway)

                
                yh = nrow(df) * 18 + 50
                if(yh<=rv$box_hp){yh=rv$box_hp}
                
                fig <- ggplotly(fig,
                                height = yh,
                                tooltip = "text",
                                source = "bar_plot_click"
                ) %>%
                    event_register("plotly_click")
                
                return(fig)
            }
        }
    }
    
    bubble_plot2 <- function(pathways=rv$bar_pathway,up=rv$bar_up,down=rv$bar_down,pq=rv$bar_pq,cutoff_p=rv$bar_p_cutoff,cutoff_q=rv$bar_q_cutoff,abby=rv$bar_abb,abbn=rv$bar_abb_n){
        if(is.null(pathways)==T){
            return(NULL)
        }else{
          df = filter_plot_df(pathways, up, down, cutoff_p, cutoff_q)
          
            # df = rv$fgseagg %>% 
            #     dplyr::filter(db %in% pathways) %>% 
            #     mutate_if(is.numeric,  ~replace(., . == 0, 0.00001)) %>%
            #   dplyr::arrange(padj)
            # 
            # if(cutoff_p < 1){
            #     df = df[which(df[["pval"]]<cutoff_p),]
            # }
            # if(cutoff_q < 1){
            #     df = df[which(df[["padj"]]<cutoff_q),]
            # }

            if(is.null(df)==T || nrow(df)<1){
                return(NULL)
            }else{
                                
                # df <- df %>%
                #   dplyr::slice_min(padj,n=up)
                
                rv$bubble_pathway_list = df[["pathway"]]
                
                # get rid of db id
                y_pathway = unlist(lapply(df$pathway,function(x){unlist(strsplit(x,"%(?=[^%]+$)",perl=TRUE))[[1]]}))
                
                # abbreviate gene set names on y axis if too long
                if(abby == "y"){
                    y_pathway = lapply(y_pathway, function(x){if(nchar(x)<abbn){return(x)}else{return(paste0(substr(x,0,abbn),"..."))}})
                }
                

                values = df$overlap
                values<-values[!is.na(values)]

                zmax = max(values) / 10
                zmin = min(values) / 10

                fig <- df %>% 
                    ggplot(aes(x=-log10(df[[pq]]), y=factor(pathway, levels=pathway), size=overlap, color=-log10(df[[pq]]),
                               text=paste0(
                                   "<b>",df[["pathway"]],"</b>\n",
                                   "P=",signif(df[["pval"]],digits=3),"; ",
                                   "P.adj=",signif(df[["padj"]],digits=3),"\n",
                                   tail(colnames(df),n=1)," (",df$overlap,"/",df[["size"]],"): \n",addlinebreaks(df[[ncol(df)]])
                                   
                               ))) +
                    geom_point(alpha=0.5) +
                    scale_size(range = c(zmin, zmax)) +
                    scale_color_gradientn(limits = c(0,3),colours=gcols2, values=gvalues2, name=paste0("-log10(",pq,")"), oob=squish) +
                    xlab(paste0("-log10(",pq,")")) + ylab("") +
                    xlim(0,max(-log10(df[[pq]]))+0.5) +
                    # geom_vline(xintercept=0, size=0.1) +
                    theme_bw() +
                    theme(axis.text.y = element_text(size=10),
                          legend.title = element_text(size = 9)) +
                    scale_y_discrete(labels = y_pathway)

                yh = nrow(df) * 18 + 50
                if(yh<=rv$box_hp){yh=rv$box_hp}
                
                fig <- ggplotly(fig,
                                height = yh,
                                tooltip = "text",
                                source = "bubble_plot_click"
                ) %>%
                    event_register("plotly_click")
                
                
                return(fig)
            }
            
        }
    }

    
    
    # enrichment plots ------------------
    density_plot <- function(term=rv$es_term){
        if(is.null(term)){
            return(NULL)
        }else{
            ranks <- rv$rnkgg
            names(ranks) = toupper(names(ranks))
            x <- toupper(rv$gmts[term][[1]])
            ranks2 <- ranks[x]
            ranks2 <- ranks2[!is.na(ranks2)]
            x <- rv$fgseagg[rv$fgseagg$pathway == term]$leadingEdge[[1]]
            ranks3 <- ranks[x]
            x = NULL
            
            cal_max_density_value <- function(){
                d1 <- max(density(ranks)$y)
                d2 <- max(density(ranks2)$y)
                peak <- max(d1,d2)
                return(peak)
            }
            
            p <- ggplot(as.data.frame(ranks),aes(x=ranks,colour = 'All genes')) + 
                geom_density(size=.9) + 
                geom_density(data=as.data.frame(ranks2),aes(x=ranks2,colour = 'Genes in gene set'),size=.9) +
                geom_dotplot(data=as.data.frame(ranks3),aes(x=ranks3,colour = 'Leading edge genes'),binwidth=.1,dotsize = 1) +
                labs(title=term,x="Rank score") +
                scale_color_manual(values = c('All genes' = 'blue', 'Genes in gene set' = 'orange', 'Leading edge genes' = 'Green')) +
                theme_minimal() +
                theme(panel.background = element_blank(), legend.position="bottom") + # , legend.position=c(1,1) , legend.justification=c(1, 1)
                geom_vline(xintercept = 0) +
                geom_hline(yintercept = 0) +
                ylim(0, cal_max_density_value())
            
            return(p)
        }
    }
    
    
    box_plot <- function(term=rv$es_term){
        if(is.null(term)){
            return(NULL)
        }else{
            p<-ggplot(rv$rr,aes(x=x,y=y)) + 
                geom_boxplot(color=c("blue","orange")) +
                labs(title=term,y="Rank score",x="") +
                theme_classic()
            return(p)
        }
        
    }
    
    violin_plot <- function(term=rv$es_term){
        if(is.null(term)){
            return(NULL)
        }else{
            p <- ggplot(rv$rr,aes(x=x,y=y,color=x)) +
                geom_violin(trim=FALSE) +
                scale_color_manual(values=c("blue","orange")) +
                stat_summary(fun.data=data_summary,geom="pointrange", color="grey") +
                # geom_jitter(height = 0, width = 0.1) +
                labs(title=term,y="Rank score",x="") +
                theme_classic() +
                theme(legend.position="none")
            return(p)
        }
    }
    
    #=======================================================================#
    ####----------------------- Functions: VisNetwork -------------------####
    #=======================================================================# 
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
    
    # get df
    
    dfNEL <- function(df=rv$fgseagg,p=rv$vis_p,q=rv$vis_q){
        if(p < 1){
            df = df %>% dplyr::filter(pval<p)
        }
        if(q < 1){
            df = df %>% dplyr::filter(padj<q)
        }
        return(df)
    }
    
    # get edge pre-matrix
    
    # edges <- function(a,a_gmt,b,method=rv$percent_method,cutoff=rv$percent_cutoff,edges_k=rv$vis_k){
    edges <- function(a,b,method=rv$percent_method,cutoff=rv$percent_cutoff,edges_k=rv$vis_k){
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
        print(Sys.time())
        
        # req(is.null(rv$vis_status) == T)
        rv$vis = NULL
        rv$vis_status = NULL
        # get df
        # df = dfNEL()
        df <- filter_plot_df(rv$vis_pathway, NULL, NULL, rv$vis_p,rv$vis_q)
        
        # print(nrow(df))
        if(is.null(df) || nrow(df)<1){
            rv$vis_status = "failed"
            return(NULL)
        # check if it exceeds the maximum data points
        } else if (nrow(df) > 500) {
          rv$vis_status = "max exceeded"
          return(NULL)
        }
        else{
            rv$vis_status = "success"
            
            rv$df_vis = df

            # leading edge genes
            a = df[[ncol(df)]] #df$leadingEdge
            # a = sapply(a, function(x) strsplit(x," "))
            names(a) <- df$pathway
            
            # GMT genes
            a_gmt = rv$gmts[names(rv$gmts) %in% df$pathway]
            
            edges_mat = NULL
            if(nrow(df)>1){
                # pathway combinations
                b_combn<-sapply(as.data.frame(combn(names(a),2)), function(x) as.character(x), simplify = FALSE)
                
                # edge pre-matrix
                # edges_mat = edges(a,a_gmt,b_combn)
                rv$edges_mat_zero_cutoff = edges(a,b_combn, cutoff = 0)
                edges_mat_zero_cutoff = rv$edges_mat_zero_cutoff
                edges_mat <- filter(edges_mat_zero_cutoff, percent >= rv$percent_cutoff)
                # rv$hc_edges = edges_mat[,c("from","to","percent")]
                # edges_mat = edges_mat[edges_mat$percent>rv$percent_cutoff,]
                rv$dendro_run = "success"
                }
            print(Sys.time())
            # nodes matrix
            # colors
            get_colors = function(pq="padj"){
                # colors = vector(mode="character", length=length(a))
                colors = rep("white",nrow(df))
                if(rv$run_mode == "gsea"){
                    colors[df[[pq]]<0.25 & df$ES>0] = "rgba(254,224,144)" #lightyellow
                    colors[df[[pq]]<0.1 & df$ES>0] = "rgba(253,174,97)" #yellow
                    colors[df[[pq]]<0.05 & df$ES>0] = "rgba(244,109,67)" #orange
                    colors[df[[pq]]<0.01 & df$ES>0] = "rgba(215,48,39)" #red
                    colors[df[[pq]]<0.001 & df$ES>0] = "rgba(165,0,38)" #dark red
                    
                    colors[df[[pq]]<0.25 & df$ES<0] = "rgba(198,219,239)" #pale blue
                    colors[df[[pq]]<0.1 & df$ES<0] = "rgba(158,202,225)" #light blue
                    colors[df[[pq]]<0.05 & df$ES<0] = "rgba(107,174,214)" #blue
                    colors[df[[pq]]<0.01 & df$ES<0] = "rgba(49,130,189)" #darker blue
                    colors[df[[pq]]<0.001 & df$ES<0] = "rgba(8,81,156)" #cornflower
                }else if(rv$run_mode == "glist"){
                    colors[df[[pq]]<0.25] = "rgba(254,224,144)" #lightyellow
                    colors[df[[pq]]<0.1] = "rgba(253,174,97)" #yellow
                    colors[df[[pq]]<0.05] = "rgba(244,109,67)" #orange
                    colors[df[[pq]]<0.01] = "rgba(215,48,39)" #red
                    colors[df[[pq]]<0.001] = "rgba(165,0,38)" #dark red
                }
                
                
                return(colors)
            }
            colors = get_colors((pq=rv$vis_pq))
            
            # shapes
            shapes = rep("dot",nrow(df))
            
            # sizes
            sizes = sapply(a, function(x) length(x))
            
            # # group for selection
            # if(rv$run_mode == "gsea"){
            #     group <- ifelse(df$ES >= 0, "Up", "Down")
            # }
            
            # node hover
            if(rv$run_mode == "gsea"){
                hovertexts <- paste0("<b>", df$pathway,"</b><br>
                     P=",round(df$pval,3),
                                     ";P.adj=",round(df$padj,3),
                                     "; ES=",round(df$ES,3),
                                     
                                     # "<br>leadingEdge:<br>", addlinebreaks_vis(df$leadingEdge))
                                     "<br>",tail(colnames(df),n=1)," (",sizes,"/",df$size,")",":<br>", addlinebreaks_vis(df[[ncol(df)]]))
                
            }else if(rv$run_mode == "glist"){
                hovertexts <- paste0("<b>", df$pathway,"</b><br>
                     P=",round(df$pval,3),
                                     ";P.adj=",round(df$padj,3),
                                     "<br>",tail(colnames(df),n=1)," (",sizes,"/",df$size,")",":<br>", addlinebreaks_vis(df[[ncol(df)]]))
            }
            
            # get rid of db id
            y_pathway = unlist(lapply(df$pathway,function(x){unlist(strsplit(x,"%(?=[^%]+$)",perl=TRUE))[[1]]}))
            
            #get the clusters_id
            print(head(df))
            if(nrow(df) == 1){
              df <- df %>%
                mutate(cluster_name = paste0("1: ", pathway))
              }
            else{
              edges_mat2 = edges_mat_zero_cutoff
            # Create a empty distance matrix with names
            dist_matrix <- matrix(0,nrow(df),nrow(df))
            colnames(dist_matrix) <- df$pathway
            rownames(dist_matrix) <- df$pathway
            
            # assign the distance matrix with value: dissimilarity percentage
            disimilarity_vector <- 1-(edges_mat2$percent)
            dist_matrix[lower.tri(dist_matrix,diag = FALSE)] = disimilarity_vector
            hclust_matrix <- as.dist(dist_matrix)
            
            # Do the hierarchical clustering
            hc <- hclust(hclust_matrix, method = "complete")
            
            # get the cluster id from the cutoff of similarity
            # create a dataframe for further data mining with cluster id in df_further
            cutoff_similarity = 0.3
            #cutoff_similarity <- rv$cutoff_point
            if(!is.null(rv$cutoff_point)){
              cutoff_similarity <- rv$cutoff_point
            }
            
            hc2 <- cutree(hc, h = 1 - cutoff_similarity)
            number_of_clusters <- max(hc2)
            #print(number_of_clusters)
            hc2_data <- hc2 %>%
              as.data.frame() %>%
              rownames_to_column() %>%
              dplyr::rename("pathway" = "rowname")
            
            # store the data frame with cluster ids
            df_further <- df %>%
              left_join(hc2_data, by = "pathway") %>%
              dplyr::rename("cluster" = ".")
            
            df_rank <- df_further %>%
              group_by(cluster) %>%
              dplyr::summarise(n = n())%>%
              mutate(rank = base::rank(-n, ties.method = "first" )) %>%
              ungroup()
            
            df_rank <- df_further %>%
              left_join(df_rank, by = "cluster") %>%
              dplyr::rename("origin_clu" = "cluster") %>%
              dplyr::rename("cluster" = "rank")
            
            df_further <- dplyr::select(df_rank, -origin_clu)
            rv$df_further = df_further
            rv$df_download <- df_further %>%
              dplyr::rename(c("cluster_size" = "n", "cluster_id" = "cluster")) %>%
              dplyr::select(cluster_id, -cluster_size, everything(), -cluster_size, -db) %>%
              dplyr::arrange(cluster_id)
            # print(head(rv$df_download))
            
            # create a data frame that has all the pathways having lowest P.adj. in each clusters
            if(rv$run_mode == "gsea"){
              df_padj <- df_further %>%
              group_by(cluster) %>%
              dplyr::mutate(n = n()) %>%
              filter(padj == min(padj)) %>%
              top_n(1, ES) %>%
              filter(row_number()==1) %>%
              dplyr::select(cluster, pathway, n, pval, ES, padj)
            } else {
              df_padj <- df_further %>%
                group_by(cluster) %>%
                dplyr::mutate(n = n()) %>%
                filter(padj == min(padj)) %>%
                filter(row_number()==1) %>%
                dplyr::select(cluster, pathway, n, pval, padj)
              }
            # assign the rvs that would be used in dendrogram
            rv$hc = hc
            rv$number_of_clusters = number_of_clusters
            rv$df_padj = df_padj
            rv$cutoff_similarity = cutoff_similarity
            
            # create the data frame that has the cluster name to be used in visnetwork
            df_significant <- df_further %>%
              left_join(df_padj, by = "cluster") %>%
              mutate(cluster_name = paste(cluster, ": ", pathway.y)) %>%
              dplyr::rename("pathway"= "pathway.x")
            df = df_significant
            }
            # saveRDS(rv$df_significant, file = "rvs/df_significant.rds")
            # generate nodes
            nodes <- data.frame(
                id = df$pathway,
                label = y_pathway,
                value = sizes * 2,  # sizes proportional to no of leading edge genes
                color = colors, # color represents ES and p
                shape = shapes, 
                cluster = df$cluster_name,
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
                               nodesIdSelection = TRUE, selectedBy = list(variable = "cluster")) %>% # , selectedBy = "group"once select a node, see relevant nodes and grey out the rest.
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
                               nodesIdSelection = TRUE, selectedBy = list(variable = "cluster")) %>% # once select a node, see relevant nodes and grey out the rest.
                    # visPhysics(stabilization = FALSE) %>%
                    visPhysics(solver = "barnesHut") %>% # node moving dynamics
                    visLayout(randomSeed = 12) # to always have the same network
                print(Sys.time())
                return(vis)
            }
        }
    }
    
    # plot an interactive dendrogram
    plot_dendro <- function(){
      df = rv$df_vis
      if(is.null(df) || nrow(df)<=1){
        rv$dendro_run = "fail"
        return(NULL)
      }else{
        rv$dendro_run = "success"
        
        #get all the rvs from the vis() function
        hc = rv$hc
        number_of_clusters = rv$number_of_clusters
        df_padj = rv$df_padj
        cutoff_similarity = rv$cutoff_similarity

        # plot a dendrogram nicely
        # convert it to a dendrogram object
        if(is.null(hc)){return(NULL)}
        dhc <- as.dendrogram(hc)
        
        # extract the dendrogram data
        ddata <- dendro_data(dhc, type = "rectangle")
        #color my dendrogram
        dendro <- dhc %>%
          #set("labels", label_short) %>%
          dendextend::set("branches_k_color", k = number_of_clusters) %>% 
          dendextend::set("branches_lwd", 0.3) %>%
          #dendextend::set("labels_cex", 0.3) %>% 
          #dendextend::set("labels_colors", k = 15) %>%
          dendextend::set("leaves_pch", 19) %>% 
          dendextend::set("leaves_cex", 0.4) 
        
        #convert it to a ggplot object
        gg_dendro <- as.ggdend(dendro)
        
        # extract the points that need to have hover function
        hover_points <- gg_dendro$nodes %>%
          filter(leaf == TRUE) %>%
          mutate(name = ddata$labels$label)
        
        cluster_size = 3
        if(!is.null(rv$cluster_size)){
          cluster_size <- rv$cluster_size
        }
        # create the points of the lowest p.adj for group labeling
        rv$max_cluster_size = max(df_padj$n)
        # the cluster size may be too small compared to the default size(3)
        if(rv$max_cluster_size < cluster_size){
          # df_padj_points <- tibble(
          #   x = max(hover_points$x),
          #   y = 0,
          #   complete_name = "Please lower the minimum cluster size to see the labels"
          # )
          cluster_size = 1
          rv$cluster_size = 1
        }
        df_padj_points <- df_padj %>%
          left_join(hover_points, by = c("pathway" = "name")) %>%
          filter(n >= cluster_size) %>%
          mutate(pathway = strsplit(pathway,"%")[[1]][1]) %>%
          mutate(complete_name = paste(cluster,": ", pathway))%>%
          mutate(length = str_length(complete_name))
        df_padj_points$complete_name = lapply(df_padj_points$complete_name, function(x){
          if(nchar(x) < 53){return(x)}
          else{return(paste0(substr(x, 0, 53),"..."))}})
        
        #rv$df_padj_points <- df_padj_points     
        
        # text size
        label_size <- 3
        if(!is.null(rv$label_size)){
          label_size <- rv$label_size
        }
        # plot it out
        ggplot_dendro <- gg_dendro %>%
          ggplot(theme = theme_minimal(), labels = FALSE) + #, horiz = TRUE
          ylim(1, -1.5) +
          coord_flip() +
          geom_point(data = hover_points, mapping = aes(x = x, y = y, text = name), size = 0.6)+
          geom_text(data = df_padj_points, mapping = aes(x = x, y = y - 0.01,label = complete_name), size = label_size) +
          ylab("distance") +
          theme(axis.text.y = element_blank(),
                axis.title.y = element_blank(),
                panel.grid.major.y = element_blank()) +
          geom_hline(yintercept = 1 - cutoff_similarity, linetype="dashed", color = "grey") #scale_y_continuous(sec.axis = dup_axis())
        # convert it to interative plotly diagram
        ggplotly_dendro <- ggplot_dendro %>%
          ggplotly(tooltip = c("name")) %>%
          layout(showlegend = FALSE, margin = list(l = 0)) %>%
          style(textposition = "right") %>%
          event_register("plotly_click")
        
        #ggplot_dendro
        return(ggplotly_dendro)
      }
        
    }
    
    plot_cluster_barplot <- function() {
      df = rv$df_vis
      if(is.null(df) || nrow(df)<=1){
        rv$cluster_bar_run = "fail"
        return(NULL)
      }else{
        rv$cluster_bar_run = "success"
       #get all the rvs from the vis() function
        hc = rv$hc
        number_of_clusters = rv$number_of_clusters
        df_padj = rv$df_padj
        cutoff_similarity = rv$cutoff_similarity
        df_further = rv$df_further

        # plot a dendrogram nicely
        # convert it to a dendrogram object
        dhc <- as.dendrogram(hc)
        
        # extract the dendrogram data
        ddata <- dendro_data(dhc, type = "rectangle")
        #color my dendrogram
        dendro <- dhc %>%
          #set("labels", label_short) %>%
          dendextend::set("branches_k_color", k = number_of_clusters) %>% 
          dendextend::set("branches_lwd", 0.3) %>%
          dendextend::set("leaves_pch", 19) %>% 
          dendextend::set("leaves_cex", 0.4) 
        
        #convert it to a ggplot object
        gg_dendro <- as.ggdend(dendro)
        
        # extract the points that need to have hover function
        hover_points <- gg_dendro$nodes %>%
          filter(leaf == TRUE) %>%
          mutate(name = ddata$labels$label)
        
        cluster_size = 3
        if(!is.null(rv$cluster_size)){
          cluster_size <- rv$cluster_size
        }
        # create the points of the lowest p.adj for group labeling
        rv$max_cluster_size = max(df_padj$n)
        # the cluster size may be too small compared to the default size(3)
        if(rv$max_cluster_size < cluster_size){
          cluster_size = 1
          rv$cluster_size = 1
        }
        df_padj_points <- df_padj %>%
          left_join(hover_points, by = c("pathway" = "name")) %>%
          filter(n >= cluster_size) %>%
          mutate(pathway = strsplit(pathway,"%")[[1]][1]) %>%
          mutate(complete_name = paste(cluster,": ", pathway))%>%
          mutate(length = str_length(complete_name)) %>%
          # add the hover texts to the data frame
          mutate(
            text_cluster = 
              map(cluster, function(x){
                a = 
                  df_further %>%
                  filter(cluster == x) %>%
                  dplyr::select(pathway) %>%
                  unname() %>%
                  unlist()
                if(length(a) > 13){
                  a = c(a[1:13], "... ...")
                }
                a = paste(a, collapse = "\n")
                return(a)
              })
          )
        

        # check if the user select the abbreviation checkbox; if yes, apply a abbreiviation method to the labels 
        if(rv$abbreviate_check == TRUE){
          abbreviate_length <- rv$abbreviate_length
          df_padj_points$complete_name = lapply(df_padj_points$complete_name, function(x){
            if(nchar(x) < abbreviate_length || is.na(nchar(x) < abbreviate_length)){return(x)}
            else{return(paste0(substr(x, 0, abbreviate_length),"..."))}})
        }
        
        df_padj_points <- df_padj_points %>% 
        arrange(desc(cluster))  
        
        cluster_barplot <- df_padj_points %>%
          ggplot(aes(x=ES, y=factor(complete_name, levels = complete_name),
                   fill=-log10(pval)*sign(ES),
                   text=paste0(
                     "<b>",df_padj_points[["complete_name"]],"</b>\n",
                     "ES=",signif(df_padj_points[["ES"]],digits=3),"; ",
                     "P=",signif(df_padj_points[["pval"]],digits=3),"; ",
                     "P.adj=",signif(df_padj_points[["padj"]],digits=3),"\n",
                     "Cluster size = ",n,"\n", "Cluster annotation:   ", text_cluster))) +
        geom_bar(stat="identity", width = 0.8) +
        scale_fill_gradientn(limits = c(-3,3),colours=gcols, values=gvalues, name=paste0("-log10(P.value)*sign(ES)"), oob=squish) +
        xlab("Enrichment Score (ES)") + ylab("") +
        geom_vline(xintercept=0, size=0.1) +
        theme_minimal() +
        theme( axis.text.y = element_text(size = 11),
              legend.title = element_text(size = 9))
        # # scale_y_discrete(position = "right")

        yh = nrow(df_padj_points) * 25 + 20
        if(yh<=500){yh=500}
        
      plotly_barplot <- ggplotly(cluster_barplot,
                                 height = yh,
                                 tooltip = "text",
                                 source = "bar_plot_click"
      ) %>%
        # layout(legend=list(colorbar=list(side="right"))) %>%
        event_register("plotly_click")
      plotly_barplot
      }
    }
    
    # Plot the cluster bubble plot
    plot_cluster_bubble <- function(zmin=rv$bubble_zmin,zmax=rv$bubble_zmax) {
      df = rv$df_vis
      if(is.null(df) || nrow(df)<=1){
        rv$cluster_bar_run = "fail"
        return(NULL)
      }else{
        rv$cluster_bar_run = "success"
        #get all the rvs from the vis() function
        hc = rv$hc
        number_of_clusters = rv$number_of_clusters
        df_padj = rv$df_padj
        cutoff_similarity = rv$cutoff_similarity
        df_further <- rv$df_further
        # plot a dendrogram nicely
        # convert it to a dendrogram object
        dhc <- as.dendrogram(hc)
        
        # extract the dendrogram data
        ddata <- dendro_data(dhc, type = "rectangle")
        # print(number_of_clusters)
        #color my dendrogram
        dendro <- dhc %>%
          #set("labels", label_short) %>%
          dendextend::set("branches_k_color", k = number_of_clusters) %>% 
          dendextend::set("branches_lwd", 0.3) %>%
          dendextend::set("leaves_pch", 19) %>% 
          dendextend::set("leaves_cex", 0.4) 
        
        #convert it to a ggplot object
        gg_dendro <- as.ggdend(dendro)
        
        # extract the points that need to have hover function
        hover_points <- gg_dendro$nodes %>%
          filter(leaf == TRUE) %>%
          mutate(name = ddata$labels$label)
        
        cluster_size = 3
        if(!is.null(rv$cluster_size)){
          cluster_size <- rv$cluster_size
        }
        # create the points of the lowest p.adj for group labeling
        rv$max_cluster_size = max(df_padj$n)
        # the cluster size may be too small compared to the default size(3)
        if(rv$max_cluster_size < cluster_size){
          cluster_size = 1
          rv$cluster_size = 1
        }
        df_padj_points <- df_padj %>%
          left_join(hover_points, by = c("pathway" = "name")) %>%
          filter(n >= cluster_size) %>%
          mutate(pathway = strsplit(pathway,"%")[[1]][1]) %>%
          mutate(complete_name = paste(cluster,": ", pathway))%>%
          mutate(length = str_length(complete_name))  %>%
          # add the hover texts to the data frame
          mutate(
            text_cluster = 
              map(cluster, function(x){
                a = 
                  df_further %>%
                  filter(cluster == x) %>%
                  dplyr::select(pathway) %>%
                  unname() %>%
                  unlist()
                if(length(a) > 13){
                  a = c(a[1:13], "... ...")
                }
                a = paste(a, collapse = "\n")
                return(a)
              })
          )
        
       # check if the user select the abbreviation checkbox; if yes, apply a abbreiviation method to the labels 
        if(rv$abbreviate_check == TRUE){
            abbreviate_length <- rv$abbreviate_length
          df_padj_points$complete_name = lapply(df_padj_points$complete_name, function(x){
            if(nchar(x) < abbreviate_length || is.na(nchar(x) < abbreviate_length)){return(x)}
            else{return(paste0(substr(x, 0, abbreviate_length),"..."))}})
        }
        
        # sort the table by the cluster id
        df_padj_points <- df_padj_points %>% 
          arrange(desc(cluster))
        
        cluster_bubble <- df_padj_points %>%
          ggplot(aes(x=ES, y=factor(complete_name, levels = complete_name),
                     size = n,
                     colour=-log10(pval)*sign(ES),
                     text=paste0(
                       "<b>",df_padj_points[["complete_name"]],"</b>\n",
                       "ES=",signif(df_padj_points[["ES"]],digits=3),"; ",
                       "P=",signif(df_padj_points[["pval"]],digits=3),"; ",
                       "P.adj=",signif(df_padj_points[["padj"]],digits=3),"\n",
                       "Cluster size = ",n,"\n", "Cluster annotation:   ", text_cluster))) +
          geom_point(alpha = 0.5) +
          scale_size(range = c(zmin, zmax)) +
          scale_color_gradientn(limits = c(-3,3),colours=gcols, values=gvalues, name=paste0("-log10(P.value)*sign(ES)"), oob=squish) +
          xlab("Enrichment Score (ES)") + ylab("") +
          geom_vline(xintercept=0, size=0.1) +
          theme_minimal() +
          theme( axis.text.y = element_text(size = 11),
                 legend.title = element_text(size = 9))
        # # scale_y_discrete(position = "right")

        yh = nrow(df_padj_points) * 25 + 20
        if(yh<=500){yh=500}
        
        plotly_bubble <- ggplotly(cluster_bubble,
                                   height = yh,
                                   tooltip = "text",
                                   source = "bubble_plot_click"
        ) %>%
          # layout(legend=list(colorbar=list(side="right"))) %>%
          event_register("plotly_click")
        plotly_bubble
      }
    }
    
    #=======================================================#
    #####         collapsible multicheckinputbox       ######
    #=======================================================#
    collapsibleAwesomeCheckboxGroupInput <- 
        function(inputId, label, i, choices = NULL, selected = NULL,  
                 status = "primary", width = NULL){
            input <- awesomeCheckboxGroup(inputId, label, choices = choices, 
                                          selected = selected, width = width,
                                          status = status)
            checkboxes <- input[[3]][[2]][[3]][[1]]
            id_btn <- paste0(inputId, "_btn")
            id_div <- paste0(inputId, "_collapsible")
            btn <- actionButton(id_btn, "More...", 
                                style = "margin-bottom: 12px",
                                icon = icon("collapse-up", lib = "glyphicon"), 
                                class = "btn-primary btn-sm", 
                                `data-toggle`="collapse", 
                                `data-target` = paste0("#", id_div))
            collapsible <- div(id = id_div, class = "collapse")
            collapsible$children <- checkboxes[(i+1):length(checkboxes)]
            children <- c(checkboxes[1:i], list(btn), list(collapsible))
            input[[3]][[2]][[3]][[1]] <- children
            script <- sprintf('$(document).ready(function(){
      $("#%s_collapsible").on("hide.bs.collapse", function(){
        $("#%s_btn").html("<span class=\\\"glyphicon glyphicon-collapse-down\\\"></span> More...");
      });
      $("#%s_collapsible").on("show.bs.collapse", function(){
        $("#%s_btn").html("<span class=\\\"glyphicon glyphicon-collapse-up\\\"></span> Less...");
      });
    });', inputId, inputId, inputId, inputId)
            tagList(input, tags$script(HTML(script)))
        }
    
    
    #=======================================================#
    #####         automatically convert gene IDs       ######
    #=======================================================#
    
    # convert gene list
    convert_gene_id <- function(species, genes){
        # gconvert to NCBI ACC #
        results = gconvert(
            genes,
            organism = species_names_go[species][[1]],
            target = "ENTREZGENE_ACC", # results$target is ACC, results$name is ACC symbol
            numeric_ns = input$num_acc,
            mthreshold = Inf,
            filter_na = TRUE
        )

        if(is.null(results)){
            lst = NULL
        }else{
            # genes_mat = results %>% dplyr::filter(tolower(input) == tolower(name))
            # perc = nrow(genes_mat)/nrow(results)
            # 
            # if(perc > 0.7){
            #     # input is SYMBOL, no need to convert
            #     g_perc = 1
            #     lst = list(g_perc,genes,results)
            # }else if(perc == 0){
            #     lst = NULL
            # }else{
                # input is not SYMBOL, rename genes
                genes_mat = distinct(results, input, .keep_all = TRUE)
                genes_after = genes[tolower(genes) %in% tolower(genes_mat$input)]
                genes_after = genes_mat$name#[tolower(genes_mat$input) %in% tolower(genes_after)]
                genes_after = unique(genes_after)
                
                # percent of genes maintained after conversion
                g_perc = length(genes_after)/length(genes)
                
                lst = list(g_perc,genes_after,genes_mat)
            # }
        }
        
        return(lst)
    }
    
    # convert ranks
    convert_rank_id <- function(species, ranks){
        genes_o = names(ranks)
        
        # load original DEG table
        df = rv$data_head_o
        df = df %>% dplyr::filter(.data[[input$gene_column]] %in% genes_o)

        # gconvert to NCBI ACC #
        results = gconvert(
            genes_o,
            organism = species_names_go[species][[1]],
            target = "ENTREZGENE_ACC", # results$target is ACC, results$name is ACC symbol
            numeric_ns = input$num_acc,
            mthreshold = Inf,
            filter_na = TRUE
        )

        if(is.null(results)){
            lst = NULL
        }else{
            # genes_mat = results %>% dplyr::filter(tolower(input) == tolower(name))
            # perc = nrow(genes_mat)/nrow(results)
            # 
            # if(perc > 0.7){
            #     # input is SYMBOL, no need to convert
            #     g_perc = 1
            #     lst = list(g_perc,ranks,results)
            # }else if(perc == 0){
            #     lst = NULL
            # }else{
                # input is not SYMBOL, rename genes
                genes_mat = distinct(results, input, .keep_all = TRUE)
                ranks_after = ranks[tolower(genes_o) %in% tolower(genes_mat$input)]
                names(ranks_after) = genes_mat$name#[tolower(genes_mat$input) %in% tolower(names(ranks_after))]
                
                # # rename DEG table
                df = df %>% dplyr::filter(tolower(.data[[input$gene_column]]) %in% tolower(genes_mat$input))
                df[[input$gene_column]] = genes_mat$name
                rv$data_head_o = df

                # percent of genes maintained after conversion
                g_perc = length(ranks_after)/length(ranks)
                
                lst = list(g_perc,ranks_after,genes_mat)
            # }
        }
        
        return(lst)
    }
    
    #=============================================#
    ########   convert & return RNK     ##########
    #=============================================#
    # generate ranks from RNK file
    convert_rnk <- function(data=rv$data_head_o){
      all_genes = data[[input$gene_column]]
      duplicates = duplicated(all_genes)
      
      if(TRUE %in% duplicates){
        data = data[!duplicates, ]
        rv$data_head_o = data
      }
      
      rv$infile_confirm = "confirm"
      
      data[[input$rank_column]] = as.numeric(data[[input$rank_column]])

      if(is.numeric(data[[input$rank_column]])){
        ranks <- setNames(data[[input$rank_column]], data[[input$gene_column]])
        ranks <- ranks[complete.cases(names(ranks))]
        rv$infile_check = "pass"
        rv$rnkgg <- ranks
      }else{
        rv$infile_check = "wrong_rnk"
      }
    }
    
    # generate ranks from DEG file
    convert_rnk_from_deg <- function(data=rv$data_head_o){
      all_genes = data[[input$gene_column]]
      duplicates = duplicated(all_genes)
      
      if(TRUE %in% duplicates){
        data = data[!duplicates, ]
        rv$data_head_o = data
      }
      
      rv$infile_confirm = "confirm"
      
      data[[input$logfc_column]] = as.numeric(data[[input$logfc_column]])
      data[[input$p_column]] = as.numeric(data[[input$p_column]])
      
      data = data[complete.cases(data),]
      
      genes <- data[[input$gene_column]]
      logfc <- data[[input$logfc_column]]
      pval <- data[[input$p_column]] #%>% mutate_if(is.numeric,  ~replace(., . == 0, 0.00001))
      pval[pval==0] = 0.000000001
      
      if(is.numeric(pval) && is.numeric(logfc)){
        rank_values <- -log10(pval) * sign(logfc)
        ranks <- setNames(rank_values,genes)
        ranks <- ranks[complete.cases(names(ranks))]
        rv$infile_check = "pass"
        rv$rnkgg <- ranks 
      }else{
        rv$infile_check = "wrong_deg"
      }
    }
    
    # check ranks and auto-convert gene IDs if applicable
    return_rnk <- function(data=rv$data_head){
      if(is.null(rv$rnkgg)==F){
        # total no of genes before conversion
        rv$total_genes = nrow(data)
        
        # # clear rv which was used to store input file data
        # rv$data_head = NULL
        
        if(input$gene_identifier=="other" && input$selected_species !="other"){
          # autodetect and convert into SYMBOL (if applicable) using gprofiler2
          species = isolate(input$selected_species)
          
          withProgress(message = "Autodetecting and converting gene IDs. Please wait a minute...",{
            Sys.sleep(0.1)
            incProgress(1)
            lst = convert_rank_id(species,rv$rnkgg)
            
            if(is.null(lst)){
              # no ID detected in database
              rv$rnk_check = "none"
              rv$rnkgg = NULL
            }else{
              # check percentage of IDs found in database
              g_perc = lst[[1]]
              
              # if <30%, reports error
              if(g_perc < 0.5){
                rv$rnk_check = "low"
              }else{
                rv$rnk_check = "pass"
              }
              
              # convert ID and save converted IDs & conversion table into RVs
              rv$rnkgg = lst[[2]]
              rv$gene_lists_mat1 = lst[[3]]
              
              # count # of genes after conversion
              rv$total_genes_after = length(rv$rnkgg)
              
            }
          })
        }else{
          rv$rnk_check = "pass"
          rv$total_genes_after = length(rv$rnkgg)
        }
      }
    }
    
    #===================================================#
    ###### reset all RVs when clicking RUN button #######
    #===================================================#

    reset_rvs <- function(){
      rv$run = NULL
      rv$fgseagg=NULL
      rv$gmts=NULL
      
      rv$no_up_01 = 0;rv$no_up_05 = 0;rv$no_down_01 = 0;rv$no_down_05 = 0
      rv$no_up_025 = 0; rv$no_down_025 = 0
      rv$es_term = NULL
      
      rv$kegg_yes=NULL;rv$kegg_confirm=NULL;rv$reactome_yes=NULL;rv$reactome_confirm=NULL
      rv$wp_yes = NULL;rv$wp_confirm=NULL;rv$vis=NULL
    }
    
    #===================================================#
    ######             reset RNK button          #######
    #===================================================#
    reset_rnk <- function(){
      rv$file_upload_status = "reset"
      
      # rv$run = NULL
      rv$rnk_check = NULL
      rv$infile_check = NULL
      rv$example_file = NULL
      rv$infile_confirm = NULL
      rv$infile_name = NULL
      rv$infile_path = NULL
      rv$file_upload_status = NULL
      rv$rnk_or_deg = NULL
      rv$gene_lists_mat1 = NULL; rv$gene_lists_mat2 = NULL
      
      shinyjs::reset("rnkfile")
      shinyjs::enable("rnkfile")
    }
    
    # Displays when the run is not initiated
    # ------------------ Panell Null -------------------
    panel_null <- function(){
      box(
        title = span( icon("exclamation"), "Notification"), status = "warning", width=6,
        "Visualization available upon successful run."
      )
    }
    
    # Function to draw an info box to guide the user along the pipeline
    #--------------------- guide box ----------------------
    # You can pass html string into msg, e.g. : guide_box("<strong>This is a bold message</strong>")
    # default color is blue
    # default width is 12 (maximum), must be is an integer value
    # To make it appear on condition, call it in a conditional renderUI({})
    
    # guide_box <- function(msg="Navigate to <b>Enrichment Results</b> for details", color="orange", width=12){
    #   box(
    #     title = NULL, background = color, solidHeader = TRUE, width=width,
    #     h4(HTML(msg))
    #   )
    # }
    
    guide_box <- function(id,msg="Navigate to <b>Enrichment Results</b> for details",color="warning",size="md",style="simple"){
      actionBttn(
        id,
        HTML(msg),
        icon=icon("angle-double-right"),
        style = style, color = color, size = size,
        block = T
      )
    }
    
    path_box <- function(id,msg){
      actionBttn(
        id,
        HTML(msg),
        # HTML(paste0("<p style='font-size:110%;word-break:break-all;vertical-align:middle;'>",msg,"</p>")),
        icon=icon("mouse"),
        style = "simple", color="primary", size = "md",
        block = T
      )
    }
    
    nav_btn_b <- function(id, color=rv$nav_btn_color, style=rv$nav_btn_style, size=rv$nav_btn_size){
      actionBttn(inputId=id, 
                 icon = icon("angle-double-left"),
                 color = color, style = style, size = size
      )
    }
    
    nav_btn_f <- function(id, color=rv$nav_btn_color, style=rv$nav_btn_style, size=rv$nav_btn_size){
      actionBttn(inputId=id, 
                 icon = icon("angle-double-right"),
                 color = color, style = style, size = size
      )
    }
    
    #========================================================#
    ######  set colnames as first row, rename colnames #######
    #========================================================#
    reset_colnames <- function(df){
      c_names_o = colnames(df)
      c_names = paste0("X",1:ncol(df))
      
      df = rbind(c_names_o, df)
      colnames(df) = c_names

      return(df)
    }
    
    #========================================================#
    ######             run GSEA or ORA                 #######
    #========================================================#
    
    run_gsea <- function(cat_name,gmt_path,ranks,errors){
      m_list <- gmtPathways(gmt_path)
      
      # save GMT into RV
      rv$gmts = c(rv$gmts,m_list)
      
      m_list <- lapply(m_list, function(x) toupper(x))

      # calculate gene #s in each term
      a_lens = lengths(m_list)
      
      if(max(a_lens)<rv$gmin || min(a_lens)>rv$gmax){errors = errors + 1}
      
      frun <- try(fgseaRes <- fgsea(pathways = m_list,
                                    stats    = ranks,
                                    minSize  = rv$gmin,
                                    maxSize  = rv$gmax,
                                    nperm = rv$gperm))
      
      if(inherits(frun, "try-error")) {        
        errors = errors + 1
      }else{
        db <- rep(cat_name, nrow(fgseaRes))
        fgseaRes <- cbind(db,fgseaRes)
        #print(head(fgseaRes))
        rv$fgseagg <- rbind(rv$fgseagg, fgseaRes)
        # rv$fgseagg <- c(rv$fgseagg, list(fgseaRes))
        rv$no_up_01 = rv$no_up_01 + sum(fgseaRes$padj<0.25&fgseaRes$ES>0,na.rm=TRUE)
        rv$no_up_05 = rv$no_up_05 + sum(fgseaRes$padj<0.05&fgseaRes$ES>0,na.rm=TRUE)
        rv$no_up_025 = rv$no_up_025 + sum(fgseaRes$padj<0.025&fgseaRes$ES>0,na.rm=TRUE)
        
        rv$no_down_01 = rv$no_down_01 + sum(fgseaRes$padj<0.25&fgseaRes$ES<0,na.rm=TRUE)
        rv$no_down_05 = rv$no_down_05 + sum(fgseaRes$padj<0.05&fgseaRes$ES<0,na.rm=TRUE)
        rv$no_down_025 = rv$no_down_025 + sum(fgseaRes$padj<0.025&fgseaRes$ES<0,na.rm=TRUE)
        
        sig_no <- rv$no_up_05 + rv$no_down_05
        if(sig_no >= 5){rv$bar_q_cutoff <- .25;rv$vis_q <- .25}
        sig_no <- rv$no_up_01 + rv$no_down_01
        if(sig_no >= 100){rv$bar_q_cutoff <- .05;rv$vis_q <- .05}
        sig_no <- rv$no_up_025 + rv$no_down_025
        if(sig_no >= 20){rv$bar_q_cutoff <- .025;rv$vis_q <- .025}
        incProgress(0.2)
      }
    }
    
    run_ora <- function(cat_name,gmt_path,genelist,errors){
      m_list <- gmtPathways(gmt_path)
      
      # save GMT into RV
      rv$gmts = c(rv$gmts,m_list)
      
      m_list <- lapply(m_list, function(x) toupper(x))
      
      # get all genes
      a_genes = toupper(unname(unlist(m_list,recursive = T))) %>% unique(.)

      # genes present in the database
      in_genes = genelist[genelist %in% a_genes]
      
      if(! identical(in_genes,character(0))){
        frun <- try(fgseaRes <- fora(pathways = m_list,
                                     genes    = in_genes,
                                     universe = a_genes,
                                     minSize  = rv$gmin,
                                     maxSize  = rv$gmax
        ))
        
        if(inherits(frun, "try-error")) {        
          errors = errors + 1
        }else{
          db <- rep(cat_name, nrow(fgseaRes))
          fgseaRes <- cbind(db,fgseaRes)
          rv$fgseagg <- rbind(rv$fgseagg, fgseaRes)
          rv$no_up_01 = rv$no_up_01 + sum(fgseaRes$padj<0.25,na.rm=TRUE)
          rv$no_up_05 = rv$no_up_05 + sum(fgseaRes$padj<0.05,na.rm=TRUE)
          
          if(rv$no_up_05 >= 5){rv$bar_q_cutoff <- .25;rv$vis_q <- .25}
          # if(rv$no_up_01 >= 1){rv$bar_q_cutoff <- .05;rv$vis_q <- .05}
        }
        
        incProgress(0.2)
        
      }
      
      
    }
    
    
    
    area_upload <- function(...,label=""){
      tmp = fileInput(...,label=label)
      print(tmp)
    }
    
    
    # =========== initialize RVs for a demo run ==================
    init_demo_gsea <- function(){
      updateSelectizeInput(session,"selected_species",selected = "hsa")
      #Demo session RVs for GSEA data store in rvs folder.
      rv$data_head_o <- readRDS(paste0(getwd(),"/rvs/data_head_o.rds"))
      rv$data_head <- readRDS(paste0(getwd(),"/rvs/data_head_o.rds"))
      rv$db_status <- readRDS(paste0(getwd(),"/rvs/db_status.rds"))
      rv$dbs <- readRDS(paste0(getwd(),"/rvs/dbs.rds"))
      rv$file_upload_status <- readRDS(paste0(getwd(),"/rvs/file_upload_status.rds"))
      rv$gene_lists_after <- readRDS(paste0(getwd(),"/rvs/gene_lists_after.rds"))
      rv$glist_check <- readRDS(paste0(getwd(),"/rvs/glist_check.rds"))
      rv$rnk_or_deg <- readRDS(paste0(getwd(),"/rvs/rnk_or_deg.rds"))
      rv$rnkll <- readRDS(paste0(getwd(),"/rvs/rnkll.rds"))
      rv$run <- readRDS(paste0(getwd(),"/rvs/run.rds"))
      rv$run_mode <- readRDS(paste0(getwd(),"/rvs/run_mode.rds"))
      rv$volcano_pathway <- readRDS(paste0(getwd(),"/rvs/volcano_pathway.rds"))
      rv$infile_name <- readRDS(paste0(getwd(),"/rvs/infile_name.rds"))
      rv$infile_path <- paste0(getwd(),"/inc/hsa.csv")
      rv$infile_confirm <- readRDS(paste0(getwd(),"/rvs/infile_confirm.rds"))
      rv$rnkgg <- readRDS(paste0(getwd(),"/rvs/rnkgg.rds"))
      rv$bar_pathway <- readRDS(paste0(getwd(),"/rvs/bar_pathway.rds"))
      rv$bubble_pathway <- readRDS(paste0(getwd(),"/rvs/bubble_pathway.rds"))
      rv$vis_pathway <- readRDS(paste0(getwd(),"/rvs/bar_pathway.rds"))
      rv$db_modal <- readRDS(paste0(getwd(),"/rvs/db_modal.rds"))
      rv$fgseagg <- readRDS(paste0(getwd(),"/rvs/fgseagg.rds"))
      rv$gmt_cs <- readRDS(paste0(getwd(),"/rvs/gmt_cs.rds"))
      rv$gmt_cs_paths <- readRDS(paste0(getwd(),"/rvs/gmt_cs_paths.rds"))
      rv$gmts <- readRDS(paste0(getwd(),"/rvs/gmts.rds"))
      rv$gmts_length <- readRDS(paste0(getwd(),"/rvs/gmts_length.rds"))
      rv$sd_high <- readRDS(paste0(getwd(),"/rvs/sd_high.rds"))
      rv$no_up_05 <- readRDS(paste0(getwd(),"/rvs/no_up_05.rds"))
      rv$no_up_01 <- readRDS(paste0(getwd(),"/rvs/no_up_01.rds"))
      rv$no_down_05 <- readRDS(paste0(getwd(),"/rvs/no_down_05.rds"))
      rv$no_down_01<- readRDS(paste0(getwd(),"/rvs/no_down_01.rds"))
      rv$infile_check <- readRDS(paste0(getwd(),"/rvs/infile_check.rds"))
      rv$rnk_check <- readRDS(paste0(getwd(),"/rvs/rnk_check.rds"))
      rv$gene_lists_mat1 <- readRDS(paste0(getwd(),"/rvs/gene_lists_mat1.rds"))
      rv$total_genes_after <- 23710
      rv$total_genes <- 23710
      # rv$es_term <- "KEGG_Viral_protein_interaction_with_cytokine_and_cytokine_receptor%hsa04061"
      # rv$kegg_confirm <- "yes"
      rv$es_term <- "WP_Type_I_Interferon_Induction_and_Signaling_During_SARS-CoV-2_Infection%WP4868"
      rv$wp_yes <- "yes"
      rv$wp_confirm <- "yes"
      # rv$es_term <- "RA_Interferon_Signaling%R-HSA-913531"
      # rv$reactome_confirm <- "yes"
      rv$run <- "success"
      rv$demo_mode <- "gsea"
      
      rv$bar_q_cutoff <- 0.025
      rv$vis_q <- 0.025

    }
    
    init_demo_ora <- function(){
      updateRadioButtons(session,"selected_mode",selected = "glist")
      updateSelectizeInput(session,"selected_species",selected = "cel")
      updateTextAreaInput(session,
                          inputId = "gene_list",
                          value = "mdt-1\nmdt-2\nmdt-3\nmdt-4\nmdt-5\nmdt-6\nmdt-7\nmdt-8\nmdt-9\nmdt-10\nmdt-11\nmdt-12\nmdt-13\nmdt-14\nmdt-15\nmdt-16\nmdt-17\nmdt-18\nmdt-19\nmdt-20\nmdt-21\nmdt-22\nmdt-26\nmdt-31\ncdk-8"
      )
      updateTextInput(session,"glist_name",value="unnamed")
      #Demo session RVs for ORA. Data stored in rvs2 folder
      # #IMPORTANT: please check 1.ui_run.R Line 12 to set the default mode to "glist"
      rv$db_status <- readRDS(paste0(getwd(),"/rvs2/db_status.rds"))
      rv$dbs <- readRDS(paste0(getwd(),"/rvs2/dbs.rds"))
      rv$gene_lists_after <- readRDS(paste0(getwd(),"/rvs2/gene_lists_after.rds"))
      rv$glist_check <- readRDS(paste0(getwd(),"/rvs2/glist_check.rds"))
      rv$rnkll <- readRDS(paste0(getwd(),"/rvs2/rnkll.rds"))
      rv$run <- readRDS(paste0(getwd(),"/rvs2/run.rds"))
      rv$run_mode <- readRDS(paste0(getwd(),"/rvs2/run_mode.rds"))
      rv$volcano_pathway <- readRDS(paste0(getwd(),"/rvs2/volcano_pathway.rds"))
      rv$bar_pathway <- readRDS(paste0(getwd(),"/rvs2/bar_pathway.rds"))
      rv$bubble_pathway <- readRDS(paste0(getwd(),"/rvs2/bubble_pathway.rds"))
      rv$vis_pathway <- readRDS(paste0(getwd(),"/rvs2/bar_pathway.rds"))
      rv$db_modal <- readRDS(paste0(getwd(),"/rvs2/db_modal.rds"))
      rv$fgseagg <- readRDS(paste0(getwd(),"/rvs2/fgseagg.rds"))
      rv$gmt_cs <- readRDS(paste0(getwd(),"/rvs2/gmt_cs.rds"))
      rv$gmt_cs_paths <- readRDS(paste0(getwd(),"/rvs2/gmt_cs_paths.rds"))
      rv$gmts <- readRDS(paste0(getwd(),"/rvs2/gmts.rds"))
      rv$gmts_length <- readRDS(paste0(getwd(),"/rvs2/gmts_length.rds"))
      rv$sd_high <- readRDS(paste0(getwd(),"/rvs2/sd_high.rds"))
      rv$no_up_05 <- readRDS(paste0(getwd(),"/rvs2/no_up_05.rds"))
      rv$no_up_01 <- readRDS(paste0(getwd(),"/rvs2/no_up_01.rds"))
      rv$no_down_05 <- readRDS(paste0(getwd(),"/rvs2/no_down_05.rds"))
      rv$no_down_01<- readRDS(paste0(getwd(),"/rvs2/no_down_01.rds"))
      rv$gene_lists_mat2 <- readRDS(paste0(getwd(),"/rvs2/gene_lists_mat2.rds"))
      # rv$run_n <- readRDS(paste0(getwd(),"/rvs2/run_n.rds"))
      rv$gene_lists <- readRDS(paste0(getwd(),"/rvs2/gene_lists.rds"))
      rv$es_term <- "RA_Transcriptional_activity_of_SMAD2/SMAD3:SMAD4_heterotrimer%R-CEL-2173793"
      rv$reactome_yes <- "yes"
      rv$reactome_confirm <- "yes"
      rv$run <- "success"
      rv$demo_mode <- "ora"
    }
    
    # unload example
    init_demo_gsea_d <- function(){
      updateSelectizeInput(session,"selected_species",selected = "")
      shinyjs::enable("selected_species")
      #Demo session RVs for GSEA data store in rvs folder.
      rv$data_head_o <- NULL
      rv$data_head <- NULL
      rv$db_status <- NULL
      rv$dbs <- NULL
      rv$file_upload_status <- NULL
      rv$gene_lists_after <- NULL
      rv$glist_check <- NULL
      rv$rnk_or_deg <- NULL
      rv$rnkll <- NULL
      rv$run <- NULL
      rv$run_mode <- NULL
      rv$volcano_pathway <- NULL
      rv$infile_name <- NULL
      rv$infile_path <- NULL
      rv$infile_confirm <- NULL
      rv$rnkgg <- NULL
      rv$bar_pathway <- NULL
      rv$bubble_pathway <- NULL
      rv$db_modal <- NULL
      rv$fgseagg <- NULL
      rv$gmt_cs <- NULL
      rv$gmt_cs_paths <- NULL
      rv$gmts <- NULL
      rv$gmts_length <- NULL
      rv$no_up_05 <- NULL
      rv$no_up_01 <- NULL
      rv$no_up_025 <- NULL
      rv$no_down_05 <- NULL
      rv$no_down_01<- NULL
      rv$no_down_025 <- NULL
      rv$infile_check <- NULL
      rv$rnk_check <- NULL
      rv$gene_lists_mat1 <- NULL
      # rv$run_n <- readRDS(paste0(getwd(),"/rvs/run_n.rds"))
      rv$total_genes_after <- NULL
      rv$total_genes <- NULL
      rv$es_term <- NULL
      rv$kegg_confirm <- NULL
      rv$wp_yes <- NULL
      rv$wp_confirm <- NULL
      rv$reactome_confirm <- NULL
      rv$run <- NULL
      rv$demo_mode <- ""
      
      rv$bar_q_cutoff <- 1
      rv$vis_q <- 1
    }
    
    init_demo_ora_d <- function(){
      updateSelectizeInput(session,"selected_species",selected = "")
      shinyjs::enable("selected_species")
      updateTextAreaInput(session,
                          inputId = "gene_list", value = ""
      )
      updateTextInput(session,"glist_name",value="")
      #Demo session RVs for ORA. Data stored in rvs2 folder
      # #IMPORTANT: please check 1.ui_run.R Line 12 to set the default mode to "glist"
      rv$db_status <- NULL
      rv$dbs <- NULL
      rv$gene_lists_after <- NULL
      rv$glist_check <- NULL
      rv$rnkll <- NULL
      rv$run <- NULL
      rv$volcano_pathway <- NULL
      rv$bar_pathway <- NULL
      rv$bubble_pathway <- NULL
      rv$db_modal <- NULL
      rv$fgseagg <- NULL
      rv$gmt_cs <- NULL
      rv$gmt_cs_paths <- NULL
      rv$gmts <- NULL
      rv$gmts_length <- NULL
      rv$no_up_05 <- NULL
      rv$no_up_01 <- NULL
      rv$no_down_05 <- NULL
      rv$no_down_01<- NULL
      rv$gene_lists_mat2 <- NULL
      # rv$run_n <- readRDS(paste0(getwd(),"/rvs2/run_n.rds"))
      rv$gene_lists <- NULL
      rv$es_term <- NULL
      rv$kegg_confirm <- NULL
      rv$wp_yes <- NULL
      rv$wp_confirm <- NULL
      rv$reactome_confirm <- NULL
      rv$run <- NULL
      rv$demo_mode = ""
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
                     if(input$selected_mode == "gsea"){
                       if(rv$demo_n %% 2 == 1){
                         init_demo_gsea_d()
                         clear_plot_rv()
                       }else{
                         init_demo_gsea()
                       }
                     }else if(input$selected_mode == "glist"){
                       if(rv$demo_n %% 2 == 1){
                         init_demo_ora_d()
                         clear_plot_rv()
                       }else{
                         init_demo_ora()
                       }
                     }
                     
                   })
    }
