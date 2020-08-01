# ==== global.R START ===========================================================
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#


library(shiny)
library(ggplot2)
library(DT)
library(pheatmap)
library(dplyr)
library(plyr)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(rvest)
library(htmlwidgets)
library(BiocManager)
options(repos = BiocManager::repositories())


# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# BiocManager::install("fgsea")
# setRepositories(addURLs = c(BioC = "https://bioconductor.org/packages/3.8/bioc"))


options(spinner.color="#0dc5c1")



# calculate and append rank
append_rank = function(x){
    x$Rank <- -sign(x$logFC) * log10(x$PValue)
    return(x)}
# remove na rows
remove_nas = function(x){return (x[complete.cases(x), ])}


# list filenames
ll <- c("Sample_Hibshman_lite.csv")
# list paths
pl <- paste0(getwd(),"/inc/", ll, sep = '')
# list dfs
gg <- lapply(pl, read.csv)
# remove any row with na anywhere
gg <- lapply(gg, remove_nas)

#gg <- lapply(gg, append_rank)
tt <- list("logFC")

cscale <- list(c(0, "rgb(255, 255, 255)"), # 0 = white
               list(0.200687, "rgb(254,224,144)"), # 0.25 = light yellow
               list(0.333333, "rgb(253,174,97)"), # 0.1 = yellow
               list(0.433677, "rgb(244,109,67)"), # 0.05 = orange
               list(0.666667, "rgb(215,48,39)"), # 0.01 = red
               list(1, "rgb(165,0,38)") # 0.001 = dark red
               )


# match string to list and return first match in list. if not found, return null
# input (string, list_of_strings)
firstmatch <- function(x,y){
  matches <- lapply(x, function(i){
    grep(i,y)
  })
  matches <- matches[lapply(matches,length)>0]
  t <- try(matches[[1]][[1]],silent=T)
  if("try-error" %in% class(t)) return(NULL)
  else return(y[[t]])
}

# match string to list and return the string that is matched
itemmatched <- function(x,y){
  matches <- lapply(x, function(i){
    grep(i,y)
  })
  index <- sapply(seq_along(matches), function(i){
    if(length(matches[[i]])>0){return (i)}
  })
  index <- index[lapply(index,length)>0]
  t <- try(index[[1]][[1]],silent=T)
  if("try-error" %in% class(t)) return(NULL)
  else return(x[[t]])
}

gene_alias <- c("X","Gene","gene")
stat_alias <- c("logFC","ES","log2FC","logfc","log2fc","Enrichment score")
p_alias <- c("PValue","p-value","P-Value","^P","pval","^p")
q_alias <- c("FDR","q-value","Q-Value","Q","qval","q","padj")


# ===================================================== global.R END ============

# Run the application 
#shinyApp(ui = ui, server = server)
