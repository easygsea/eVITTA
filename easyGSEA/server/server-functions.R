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
    enrichmentplot <- function() {
        ranks = rv$rnkgg
        names(ranks) = toupper(names(ranks))
        gmt = rv$gmts[[rv$es_term]]
        plotEnrichment(toupper(gmt),ranks) + labs(title = rv$es_term)
    }
    
    bar_plot <- function(pathways=rv$bar_pathway,up=rv$bar_up,down=rv$bar_down,pq=rv$bar_pq,cutoff_p=rv$bar_p_cutoff,cutoff_q=rv$bar_q_cutoff,abby=rv$bar_abb,abbn=rv$bar_abb_n){
        if(is.null(pathways)==T){
            return(NULL)
        }else{
            df = rv$fgseagg %>% dplyr::filter(!(is.na(pval)))
            
            df = df %>% 
                dplyr::filter(db %in% pathways) %>% 
                mutate_if(is.numeric,  ~replace(., . == 0, 0.00001))
            
            if(cutoff_p < 1){
                df = df %>% dplyr::filter(pval < cutoff_p)
            }
            if(cutoff_q < 1){
                df = df %>% dplyr::filter(padj < cutoff_q)
            }
            
            if(is.null(df)==T || nrow(df)<1){
                return(NULL)
            }else{
                df1 <- df %>% dplyr::filter(ES > 0)
                if(nrow(df1)<up){
                    df1 = df1[order(df1[[pq]])]
                }else{
                    df1 = df1[head(order(df1[[pq]]),n=up)]
                }
                
                df2 <- df  %>% dplyr::filter(ES < 0)
                if(nrow(df2)<down){
                    df2 = df2[order(df2[[pq]])]
                }else{
                    df2 = df2[head(order(df2[[pq]]),n=down)]
                }
                
                df <- rbind(df1,df2)
                df <- df %>% arrange(desc(ES))
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
                
                fig <- ggplotly(fig,tooltip = "text",
                                source = "bar_plot_click"
                ) %>%
                    event_register("plotly_click")
                
                return(fig)
            }
        }
    }
    
    bubble_plot <- function(pathways=rv$bar_pathway,up=rv$bar_up,down=rv$bar_down,pq=rv$bar_pq,cutoff_p=rv$bar_p_cutoff,cutoff_q=rv$bar_q_cutoff,abby=rv$bar_abb,abbn=rv$bar_abb_n,zmin=rv$bubble_zmin,zmax=rv$bubble_zmax){
        if(is.null(pathways)==T){
            return(NULL)
        }else{
            df = rv$fgseagg %>% dplyr::filter(!(is.na(pval)))
            
            df = df %>% 
                dplyr::filter(db %in% pathways) %>% 
                mutate_if(is.numeric,  ~replace(., . == 0, 0.00001))
            
            if(cutoff_p < 1){
                df = df %>% dplyr::filter(pval < cutoff_p)
            }
            if(cutoff_q < 1){
                df = df %>% dplyr::filter(padj < cutoff_q)
            }
            
            if(is.null(df)==T || nrow(df)<1){
                return(NULL)
            }else{
                df1 <- df %>% dplyr::filter(ES > 0)
                if(nrow(df1)<up){
                    df1 = df1[order(df1[[pq]])]
                }else{
                    df1 = df1[head(order(df1[[pq]]),n=up)]
                }
                
                df2 <- df  %>% dplyr::filter(ES < 0)
                if(nrow(df2)<down){
                    df2 = df2[order(df2[[pq]])]
                }else{
                    df2 = df2[head(order(df2[[pq]]),n=down)]
                }
                
                df <- rbind(df1,df2)
                df <- df %>% arrange(desc(ES))
                
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
                
                    
                
                
                fig <- ggplotly(fig,tooltip = "text",
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
                mutate_if(is.numeric,  ~replace(., . == 0, 0.00001))
            
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
                mutate_if(is.numeric,  ~replace(., . == 0, 0.00001))
            
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
                mutate_if(is.numeric,  ~replace(., . == 0, 0.00001))
            
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
    
    # glist bar bubble volcano -------------------
    bar_plot2 <- function(pathways=rv$bar_pathway,up=rv$bar_up,down=rv$bar_down,pq=rv$bar_pq,cutoff_p=rv$bar_p_cutoff,cutoff_q=rv$bar_q_cutoff,abby=rv$bar_abb,abbn=rv$bar_abb_n){
        if(is.null(pathways)==T){
            return(NULL)
        }else{
            df = rv$fgseagg %>% 
                dplyr::filter(db %in% pathways) %>% 
                mutate_if(is.numeric,  ~replace(., . == 0, 0.00001))
            
            if(cutoff_p < 1){
                df = df %>% dplyr::filter(pval<cutoff_p)
            }
            if(cutoff_q < 1){
                df = df %>% dplyr::filter(padj<cutoff_q)
            }
            

            if(is.null(df)==T || nrow(df)<1){
                return(NULL)
            }else{
                
                df <- df[head(order(df[[pq]]),n=up)]
                
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

                
                
                fig <- ggplotly(fig,tooltip = "text",
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
            df = rv$fgseagg %>% 
                dplyr::filter(db %in% pathways) %>% 
                mutate_if(is.numeric,  ~replace(., . == 0, 0.00001))
            
            if(cutoff_p < 1){
                df = df[which(df[["pval"]]<cutoff_p),]
            }
            if(cutoff_q < 1){
                df = df[which(df[["padj"]]<cutoff_q),]
            }

            if(is.null(df)==T || nrow(df)<1){
                return(NULL)
            }else{
                                
                df <- df[head(order(df[[pq]]),n=up)]
                
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

                
                fig <- ggplotly(fig,tooltip = "text",
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
            x <- rv$gmts[term][[1]]
            ranks2 <- rv$rnkgg[x]
            ranks2 <- ranks2[!is.na(ranks2)]
            x <- rv$fgseagg[rv$fgseagg$pathway == term]$leadingEdge[[1]]
            ranks3 <- rv$rnkgg[x]
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
        # req(is.null(rv$vis_status) == T)
        rv$vis = NULL
        rv$vis_status = NULL
        # get df
        df = dfNEL()
        
        # print(nrow(df))
        if(nrow(df)<1){
            rv$vis_status = "failed"
            return(NULL)
        }else{
            rv$vis_status = "success"
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
                edges_mat = edges(a,b_combn)
                # rv$hc_edges = edges_mat[,c("from","to","percent")]
                # edges_mat = edges_mat[edges_mat$percent>rv$percent_cutoff,]
            }
            
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
            
            # generate nodes
            nodes <- data.frame(
                id = df$pathway,
                label = y_pathway,
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
    
