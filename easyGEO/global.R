if(!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
if (!require('shiny')) {install.packages('shiny')}
if (!require('shinydashboard')) {install.packages('shinydashboard')}
if (!require('shinyWidgets')) {install.packages('shinyWidgets')}
if (!require('shinyBS')) {install.packages('shinyBS')}
if (!require('shinyjs')) {install.packages('shinyjs')}
if (!require('GEOquery')) {install.packages('GEOquery')}
if (!require('tidyverse')) {install.packages('tidyverse')}
if (!require('data.table')) {install.packages('data.table')}
if (!require('shinyjs')) {install.packages('shinyjs')}
if (!require('DT')) {install.packages('DT')}
if (!require('limma')) {install.packages('limma')}
if (!require('edgeR')) {install.packages('edgeR')}
if (!require('ggrepel')) {install.packages('ggrepel')}
if (!require('plotly')) {install.packages('plotly')}
if (!require('BiocManager')) {install.packages('BiocManager')}
if (!require('waiter')) {install.packages('waiter')}
if (!require('statmod')) {install.packages('statmod')}
if (!require('scales')) {install.packages('scales')}
if (!require('glue')) {install.packages('glue')}
if (!require('shinyalert')) {install.packages('shinyalert')}
if (!require('lobstr')) {install.packages('lobstr')}
if (!require('shinydisconnect')) {install.packages('shinydisconnect')}
if (!require('rintrojs')) {install.packages('rintrojs')}
if (!require('knitr')) {install.packages('knitr')}
if (!require('RColorBrewer')) {install.packages('RColorBrewer')}


library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(GEOquery)
library(tidyverse)
library(data.table)
library(shinyjs)
library(DT)
library(limma)
library(edgeR)
library(ggrepel)
library(plotly)
library(BiocManager)
library(waiter)
library(statmod)
library(scales)
library(glue)
library(shinyalert)
library(lobstr)
library(shinydisconnect)
library(rintrojs)
library(knitr) #might need for transforming rmd to html
library(RColorBrewer)

options(repos = BiocManager::repositories())

source("global/functions.R")

# --------------- Initialize introjs -------------------
intropath <- paste0(getwd(), "/intro/")
filepaths <- list.files(intropath, full.names=T)
intros <- lapply(filepaths, function(x){
  df <- data.frame(read.csv(x, header=T, sep="\t"))
  df$element <- sub("^", "#", df$element)
  df[df=="#"] <- NA
  df
})
names(intros) <- tools::file_path_sans_ext(list.files(intropath, full.names=F))
rownames(intros) <- NULL


# load screen message
loadMsg = "easyGEO"


# accepted study types
accepted_study_types <- c("Expression profiling by high throughput sequencing", "Expression profiling by array")


# slider cutoffs for p/q
cutoff_slider = c(0.0001,0.0005,0.001,0.005,0.01,0.05,0.1,0.25,0.3,0.5,1)

# labeling options
label_options = list("By thresholds"="threshold","By top #"="top","By manual input"="manual")

# function to extract the first no of elements and attach "... ..." to an R vector
abbreviate_vector <- function(x,no=3){
  if(length(x)>no){
    x = paste(x[1:no],collapse = ", ") %>%
      paste0(.," ... ...")
  }else{
    x = paste(x,collapse = ", ")
  }
  return(x)
}

# -------------- color scales -------------
# -3 to 3 colorscale adapted from cscale + cscale_bu2. evenly distributed.
cscale_zscore <- list(c(0, "rgb(8,81,156)"), # -0.001 = cornflower,
                      list(0.1, "rgb(49,130,189)"), # -0.01 = darker blue
                      list(0.2, "rgb(107,174,214)"), # -0.05 = blue
                      list(0.3, "rgb(158,202,225)"), # -0.1 = light blue
                      list(0.4, "rgb(198,219,239)"), # -0.25 = pale blue
                      list(0.5, "rgb(255, 255, 255)"), # 0 = white
                      list(0.6, "rgb(254,224,144)"), # 0.25 = light yellow
                      list(0.7, "rgb(253,174,97)"), # 0.1 = yellow
                      list(0.8, "rgb(244,109,67)"), # 0.05 = orange
                      list(0.9, "rgb(215,48,39)"), # 0.01 = red
                      list(1, "rgb(165,0,38)") # 0.001 = dark red
)

# -------------- potential terms in DEG file, so as to tell users it's already analyzed files -------------
deg_colnames <- c("logfc","fc","log2_fold_change"
                  ,"p","pval","pvalue","p.value","p_value"
                  ,"fdr","padj","adj.p.val","q_value")

# ===================== remove previous files downloaded from GEO ftp sites =====================
do.call(file.remove, list(list.files(paste0(getwd(),"/www/tmp/"),full.names = TRUE)))

