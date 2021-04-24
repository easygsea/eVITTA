###-----------------------------------------------------------------###
###   Part of the code adapted from RRHO package:                   ###
###   https://rdrr.io/bioc/RRHO/src/R/ExpressionAnalysis.R          ###
###   (Author: Jonathan Rosenblatt and Jason Stein)                 ###
###-----------------------------------------------------------------###

## Suggest default step size
#------------------------------
defaultStepSize <-function(list1, list2){
  
  n1<- dim(list1)[1]
  n2<- dim(list2)[1]
  
  max_n <- 1000 # max allowed number of rows
  max_steps <- ceiling(min(sqrt(max_n)))	 # max allowed number of steps
  
  if (max(n1, n2)<max_n){
    result <- ceiling(min(sqrt(c(n1,n2))))	
  } else {
    result <- max(n1,n2)/max_steps 
  }
  return(result)
}	

#Calculate the max value for slider
maximumStepSize <-function(list1, list2){
  n1<- dim(list1)[1]
  n2<- dim(list2)[1]
  result <- ceiling(min(sqrt(c(n1,n2))))
  return(result)
}



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
  
  if(palette == "default"){
    default_order <- c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000")
    if(reverse == T){
      default_order <- rev(default_order)
    }
    jet.colors  <- colorRampPalette(default_order)
    jet.colors <- jet.colors(100)
    }
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
                      stepsize = stepsize,
                      BY_correction=T,
                      to_put_alternative = "two.sided",
                      log10.ind=T,
                      color_scheme
                      #palette="default",
                      #reverse = F
)
{
  #This step is to only keep the ones in common

  
  RRHO.xy <- RRHO(rnk1, rnk2, 
                  stepsize,
                  plots = F, 
                  outputdir = paste0(getwd(),"//RRHOtest"),
                  BY=T, 
                  alternative="two.sided",
                  log10.ind = T
  )
  jet.colors  <- color_scheme
  if(BY_correction == T){
    rrho_plot <- lattice::levelplot(RRHO.xy$hypermat.by,col.regions = jet.colors,xlab = rv$rrho_x,ylab = rv$rrho_y)
  }else{
    rrho_plot <- lattice::levelplot(RRHO.xy$hypermat,col.regions = jet.colors,xlab = rv$rrho_x,ylab = rv$rrho_y) # shows the graph
  }
  # pval.testing <- pvalRRHO(RRHO.xy, 50)
  # p_value <- pval.testing$pval
  
  jet.colors <- NULL
  # pval.testing <- NULL
  return(list(rrho_plot
              # , p_value
              )) #SUBJECT TO MORE FEATURES, will do later
}

#DataGeneIdentifier,pvalue,STAT
rrho_level_value <- reactive({
  req(is.null(n_ins_full())==F)
  req(rv$rrho_level_setting == input$rrho_level_setting) # fixes double refresh
  
  datasets <- rrho_data_handler(rv$rrho_x,rv$rrho_y)
  data1 <- data.frame(datasets[[1]],datasets[[2]],datasets[[3]])
  names(data1) <- c("Name","STAT","PValue")
  data2 <- data.frame(datasets[[4]],datasets[[5]],datasets[[6]])
  names(data2) <- c("Name","STAT","PValue")
  rnk1 <- get_rrho_rnk(data1,DataGeneIdentifier = "Name",pvalue = "PValue",STAT = "STAT")
  rnk2 <- get_rrho_rnk(data2,DataGeneIdentifier = "Name",pvalue = "PValue",STAT = "STAT")
  #Remove NA
  original_names <- names(rnk1)
  rnk_merge <- merge(rnk1,rnk2,by.x = "GeneIdentifier",by.y ="GeneIdentifier")
  rnk_merge <- na.omit(rnk_merge)
  rnk1 <- data.frame(rnk_merge$GeneIdentifier,rnk_merge$RankingVal.x)
  names(rnk1) <- original_names
  rnk2 <- data.frame(rnk_merge$GeneIdentifier,rnk_merge$RankingVal.y)
  names(rnk2) <- original_names
  #Remove NA
  rv$rnk1 <- rnk1
  rv$rnk2 <- rnk2
  rv$max_step_size <- maximumStepSize(rnk1,rnk2)
  print('max step size is: ')
  print(rv$max_step_size)
  rv$stepsize <- defaultStepSize(rnk1,rnk2)
  print('current step size is: ')
  print(rv$stepsize)
  # specify stepsize; default for n<1000, manually defined when n>1000
  stepsize <- defaultStepSize(rnk1,rnk2)
  
  # print(head(rnk1))
  
  color_scheme <- rrho_color_handler(palette=rv$rrho_level_palette,reverse = rv$rrho_level_palette_reverse)
  withProgress(message = 'Generating plots ...',value = 1, {
    rv$result_plot <-             run_rrho(rnk1, rnk2,
                                           stepsize = stepsize,
                                           BY_correction = rv$rrho_level_setting,
                                           to_put_alternative = "two.sided",
                                           log10.ind=T,
                                           color_scheme = color_scheme
                                           )
                                           #palette=rv$rrho_level_palette,
                                           #reverse = rv$rrho_level_palette_reverse)
  })
  
  
  rrho_level_fig <- rv$result_plot[[1]]
  
  #print(rv$result_plot[[2]])
  rrho_level_fig
  
})
#THIS Reactive is for scatter plot
rrho_scatter_value <- reactive({
  req(rv$rrho_level_setting == input$rrho_level_setting) # fixes double refresh
  
list1  <- rv$rnk1[order(rv$rnk1[,2],decreasing=TRUE),]
list2  <- rv$rnk2[order(rv$rnk2[,2],decreasing=TRUE),]
list2ind  <- match(list1[,1],list2[,1])
list1ind  <- 1:length(list1[,1])
cor=cor.test(list1ind,list2ind,alternative="two.sided",method="spearman")
corval  <- cor[[4]]
corPVal <- cor[[3]]
print(cor)

plot(list1ind,list2ind,xlab=paste(rv$rrho_x,"(Rank)"), 
     ylab=paste(rv$rrho_y,"(Rank)"), pch=20, 
     main=paste(
       "Rank-Rank Scatter (rho = ",signif(corval,digits=3),
       "; p = ",corPVal,
       ")"
       ,sep=""), cex=0.5)
model  <- lm(list2ind~list1ind)
lines(predict(model),col="red",lwd=3)
})

# rrho_p_value <- reactive({
#   new_rrho_p_value = rv$result_plot[[2]]
#   new_rrho_p_value
# })

output$rrho_level <- renderPlot(
  rrho_level_value()
)

output$rrho_scatter_plot <- renderPlot(
  rrho_scatter_value()
)


# output$rrho_p_value <- renderUI({
#   
#   box(
#     title = NULL, background = "aqua", solidHeader = TRUE, width=12,
#     strong("Correlation line:"),br(),
#     column( 12,align="center" ,
#             paste0("The pvalue is ",rrho_p_value())
#             #rv$result_plot[[2]]
#     )
#   )
# })
