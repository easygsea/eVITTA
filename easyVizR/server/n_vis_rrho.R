

rrho_data_handler <- function(x_axis,y_axis){
  df <- rv$df_n
  gene_stat_one = paste0("Stat_",x_axis)
  gene_pval_one = paste0("PValue_",x_axis)
  gene_stat_two = paste0("Stat_",y_axis)
  gene_pval_two = paste0("PValue_",y_axis)
  # print(gene_stat_one)
  # print(gene_stat_two)
  # print(names(df)[2])
  
  dataset_one = data.frame(df$Name,df[gene_stat_one],df[gene_pval_one])
  #names(dataset_one) <- c("Name","STAT","PValue")
  
  dataset_two = data.frame(df$Name,df[gene_stat_two],df[gene_pval_two])
  
  return(c(dataset_one,dataset_two))
}

get_rrho_rnk <-function(dataset,DataGeneIdentifier,pvalue,STAT){
  
  
  rnk = data.frame(
    GeneIdentifier=dataset[DataGeneIdentifier], 
    RankingVal=-log(dataset[pvalue])*sign(dataset[STAT])
  )
  
  colnames(rnk) <- c("GeneIdentifier","RankingVal")
  return(rnk)
  # generate rnk dataframe to input into rrho_plot()
}

rrho_color_handler <- function(palette,reverse = F){
  jet.colors  <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
  if(palette == "default"){jet.colors <- jet.colors(100)}
  else{
    if(reverse == T){
      jet.colors <-colorRampPalette(rev(brewer.pal(8, palette)))(50)
    }else{
      jet.colors <-colorRampPalette(brewer.pal(8, palette))(50)
    }
    
  }
  return (jet.colors)
}


run_rrho <- function (rnk1, rnk2,
                      BY=F,
                      to_put_alternative = "two.sided",
                      log10.ind=T,
                      palette="default",
                      reverse = F
)
{
  #This step is to only keep the ones in common
  
  original_names <- names(rnk1)
  rnk_merge <- merge(rnk1,rnk2,by.x = "GeneIdentifier",by.y ="GeneIdentifier")
  rnk_merge <- na.omit(rnk_merge)
  View(rnk_merge)
  rnk1 <- data.frame(rnk_merge$GeneIdentifier,rnk_merge$RankingVal.x)
  names(rnk1) <- original_names
  rnk2 <- data.frame(rnk_merge$GeneIdentifier,rnk_merge$RankingVal.y)
  names(rnk2) <- original_names
  RRHO.xy <- RRHO(rnk1, rnk2, 
                  plots = F, 
                  outputdir = paste0(getwd(),"//RRHOtest"),
                  BY=T, 
                  alternative="two.sided",
                  log10.ind = T
  )
  jet.colors  <- rrho_color_handler(palette = palette)
  
  rrho_plot <- lattice::levelplot(RRHO.xy$hypermat,col.regions = jet.colors) # shows the graph
  pval.testing <- pvalRRHO(RRHO.xy, 50)
  p_value <- pval.testing$pval
  
  jet.colors <- NULL
  pval.testing <- NULL
  return(list(rrho_plot, p_value)) #SUBJECT TO MORE FEATURES, will do later
}

#DataGeneIdentifier,pvalue,STAT
rrho_level_value <- reactive({
  req(is.null(n_ins_full())==F)
  datasets <- rrho_data_handler(rv$rrho_x,rv$rrho_y)
  data1 <- data.frame(datasets[[1]],datasets[[2]],datasets[[3]])
  names(data1) <- c("Name","STAT","PValue")
  data2 <- data.frame(datasets[[4]],datasets[[5]],datasets[[6]])
  names(data2) <- c("Name","STAT","PValue")
  rnk1 <- get_rrho_rnk(data1,DataGeneIdentifier = "Name",pvalue = "PValue",STAT = "STAT")
  rnk2 <- get_rrho_rnk(data2,DataGeneIdentifier = "Name",pvalue = "PValue",STAT = "STAT")
  rv$result_plot <-             run_rrho(rnk1, rnk2,
                                    BY=F,
                                    to_put_alternative = "two.sided",
                                    log10.ind=T,
                                    palette="default",
                                    reverse = F)
  print(rv$result_plot[[2]])
  rrho_level_fig <- rv$result_plot[[1]]
  rrho_level_fig
  
})

output$rrho_level <- renderPlot(
  rrho_level_value()
)


output$rrho_p_value <- renderUI({
  box(
    title = NULL, background = "aqua", solidHeader = TRUE, width=12,
    strong("Correlation line:"),br(),
    column( 12,align="center" ,
            paste0("The pvalue is ",rv$result_plot[[2]])
    )
  )
})
