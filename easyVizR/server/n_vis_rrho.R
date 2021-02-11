

rrho_data_handler <- function(x_axis,y_axis){
  df <- n_ins_full()
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




output$rrho_p_value <- renderUI({
  req(is.null(n_ins_full())==F)
  rv$datasets <- rrho_data_handler(rv$rrho_x,rv$rrho_y)
  View(datasets)
  data1 <- data.frame(rv$datasets[[1]],rv$datasets[[2]],rv$datasets[[3]])
  names(data1) <- c("Name","STAT","PValue")
  data2 <- data.frame(rv$datasets[[4]],rv$datasets[[5]],rv$datasets[[6]])
  names(data2) <- c("Name","STAT","PValue")
  rnk1 <- get_rrho_rnk(data1,DataGeneIdentifier = "Name",pvalue = "PValue",STAT = "STAT")
  rnk2 <- get_rrho_rnk(data2,DataGeneIdentifier = "Name",pvalue = "PValue",STAT = "STAT")
  rv$result <-             run_rrho(rnk1, rnk2,
                                    BY=F,
                                    to_put_alternative = "two.sided",
                                    log10.ind=T,
                                    palette="default",
                                    reverse = F)
  rv$rrho_p_value <- rv$result[2]
  intercept = coef(rv$fit_nxy)[[1]]
  print(intercept)
  box(
    title = NULL, background = "aqua", solidHeader = TRUE, width=12,
    strong("Correlation line:"),br(),
    column( 12,align="center" ,
            paste0("The pvalue is ",intercept)
    )
  )
})