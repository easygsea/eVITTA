library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(shinyBS)
library(shinyjs)
library(visNetwork)
library(DT)
library(tidyverse)
library(tidytext)
library(stringr)
library(gprofiler2)
library(pathview)
library(ggrepel)
library(scales)
library(fgsea)
library(data.table)
library(RColorBrewer)
library(plotly)
library(htmltools)
library(AnnotationDbi)
library(waiter)
library(shinyalert)

options(repos = BiocManager::repositories())

# p_min to convert p=0
p_min = 1e-300

# slider cutoffs for p/q
cutoff_slider = c(0.0001,0.0005,0.001,0.005,0.01,0.05,0.1,0.25,0.3,0.5,1)

# stop words and words with minimum meaning to be filtered for word counts
data(stop_words)
useless_words <- read_csv(paste0(getwd(),"/inc/some_words.csv"))

# run modes
run_modes = list("Pre-ranked GSEA"="gsea","Overrepresentation Analysis"="glist")

# gene identifiers
gene_identifiers = list("SYMBOL"="symbol","Other/Mixed" = "other")

# wellpanel background colors
bcol1 = "#e6f4fc"
bcol2 = "#ffffe6"
bcol3 = "#e6f4fc"

# ===================== FUNCTIONS =====================
# function to abbreviate strings
abbreviate_string <- function(x){
  abbreviate(x, use.classes = T, dot = F, named = F)
}

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

# function to convert between full and abbreviated species names
species_translate <- function(x,source_list = species_names){
  if(nchar(x)>5){
    translated <- source_list[x][[1]]
  }else if(nchar(x)<=5){
    translated <- names(source_list)[which(source_list == x)]
  }else{
    translated <- NULL
  }
  return(translated)
}

# ===================== currently supported SPECIES list =====================
# species names for main menu display
species_names <- list(
  "Bos taurus" = "bta",
  "Caenorhabditis elegans" = "cel",
  "Canis lupus familiaris" = "cfa",
  "Danio rerio" = "dre",
  "Drosophila melanogaster" = "dme",
  "Gallus gallus" = "gga",
  "Homo sapiens" = "hsa",
  "Mus musculus" = "mmu",
  "Rattus norvegicus" = "rno",
  "Saccharomyces cerevisiae" = "sce",
  "Sus scrofa" = "ssc"
)

# species names for gprofiler id autodetection and conversion
species_names_go <- list(
  "bta" = "btaurus",
  "cel" = "celegans",
  "cfa" = "cfamiliaris",
  "dre" = "drerio",
  "dme" = "dmelanogaster",
  "gga" = "ggallus",
  "hsa" = "hsapiens",
  "mmu" = "mmusculus",
  "rno" = "rnorvegicus",
  "sce" = "scerevisiae",
  "ssc" = "sscrofa"
)

# ===================== Column names & match for DEG/RNK =====================
col_gene_names <- c("x","id","gene","symbol","genesymbol","gene_id","x1","genename","name")
col_fc_names <- c("logfc","fc")
col_p_names <- c("p","pval","pvalue","p.value")
col_fdr_names <- c("fdr","padj","adj.p.val")
col_rank_names <- c("rank","ranks","score")

match_colnames <- function(dict = col_gene_names, col_names){
  col_names[which(tolower(col_names) %in% dict)[1]]
}

# ===================== Example data for ORA run mode =====================
# read in csv files
glist_example = read_csv(paste0(getwd(),"/inc/glist_example_genes.csv"))
glist_example_species = glist_example[[1]]
glist_example = as.list(glist_example[[2]])
names(glist_example) = glist_example_species
remove(glist_example_species)

# ===================== COLOR SCALES for visualizations =====================
# color bar for GSEA output visualizations (blue and red) - ggolot version
gcols = c(rgb(8,81,156,maxColorValue = 255), # -0.001 = cornflower,
          rgb(49,130,189,maxColorValue = 255), # -0.01 = darker blue
          rgb(107,174,214,maxColorValue = 255), # -0.05 = blue
          rgb(158,202,225,maxColorValue = 255), # -0.1 = light blue
          rgb(198,219,239,maxColorValue = 255), # -0.25 = pale blue
          rgb(255, 255, 255,maxColorValue = 255), # 0 = white
          rgb(254,224,144,maxColorValue = 255), # 0.25 = light yellow
          rgb(253,174,97,maxColorValue = 255), # 0.1 = yellow
          rgb(244,109,67,maxColorValue = 255), # 0.05 = orange
          rgb(215,48,39,maxColorValue = 255), # 0.01 = red
          rgb(165,0,38,maxColorValue = 255)) # 0.001 = dark red

# value scales for GSEA output visualizations (blue and red) - ggolot version
gvalues = rescale(c(-3,-2,log10(0.05),-1,log10(0.25),0,-log10(0.25),1,-log10(0.05),2,3))

# color bar for ORA output visualizations (red only) - ggolot version
gcols2 = c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
          rgb(254,224,144,maxColorValue = 255),
          rgb(253,174,97,maxColorValue = 255),
          rgb(244,109,67,maxColorValue = 255),
          rgb(215,48,39,maxColorValue = 255),
          rgb(165,0,38,maxColorValue = 255)) # 0.001 = dark red

# value scales for ORA output visualizations (red only) - ggolot version
gvalues2 = rescale(c(0,-log10(0.25),1,-log10(0.05),2,3))

#===================== GMT collections =====================
# initialize three list vectors
# 1. a list vector to store paths to database collection (.GMT) files
gmt_collections_paths <- vector("list")
# 2. a list vector to store database collection categories and names
gmt_collections <- vector("list")
# 3. a list vector to store default selected database collections categories and names
gmt_collections_selected <- vector("list")

# read in the file which stores GMTs to load
test = read.csv(paste0(getwd(),"/www/gmts/gmts_list.csv"),header=F,stringsAsFactors = F)

# names of databases
dbs = strsplit(test$V3,";")
for(i in seq_along(dbs)){
  # tidy up database names by removing ".gmt" and the dates it's created; convert _ to spaces
  names <- gsub(".gmt$","",dbs[[i]]);names <- gsub("\\d\\d+$","",names);names <- gsub("_"," ",names)
  names_abbr = abbreviate_string(names); coll = names_abbr
  names(coll) = names
  
  # store databases names into the list vector that stores collections
  gmt_collections = c(gmt_collections, list(coll))
  
  # paths to GMT files
  paths = paste0(getwd(),"/www/gmts/",test$V1[[i]],"/",test$V2[[i]],"/",dbs[[i]])
  names(paths) = names_abbr
  
  gmt_collections_paths = c(gmt_collections_paths, list(paths))
  

}

# name collections by species names
# gmt_collections = Map(setNames, as.list(gmt_collections), names)
names(gmt_collections) = test$V2
gmt_collections = split(gmt_collections,test$V1)

# name collection (.GMT) paths by species names
# gmt_collections_paths = Map(setNames, as.list(gmt_collections_paths), names_abbr)
names(gmt_collections_paths) = test$V2
gmt_collections_paths = split(gmt_collections_paths,test$V1)


## read in GMTs selected as default
test = read.csv(paste0(getwd(),"/www/gmts/gmts_list_selected.csv"),header=F,stringsAsFactors = F)
# names of databases
dbs = strsplit(test$V3,";")
for(i in seq_along(dbs)){
  # tidy up database names by removing ".gmt" and the dates it's created; convert _ to spaces
  names <- gsub(".gmt$","",dbs[[i]]);names <- gsub("\\d\\d+$","",names);names <- gsub("_"," ",names)
  names_abbr = abbreviate_string(names); coll = names_abbr
  names(coll) = names
  
  # store databases names into the list vector that stores collections
  gmt_collections_selected = c(gmt_collections_selected, list(coll))
}

# name collections by species names
names(gmt_collections_selected) = test$V2
gmt_collections_selected = split(gmt_collections_selected,test$V1)

remove(test); remove(dbs)

# ======================= renderDataTable options ===========================
# apply options to datatable. example: options = dt_options(80,F,F,T,T,T,10)
# To apply options to datatable. only works with ellipsis enabled
#   dt_options (max_char, scrollX=F, scrollY=F, paging=T, searching=T, info=T, pageLength = 10, autoWidth=T)
# example:
#   renderDataTable({df}, plugins="ellipsis", options = dt_options(80,F,F,T,T,T,10))

dt_options <- function(max_char=60, scrollX=T, scrollY=F, paging=T, searching=T, info=T, pageLength = 10, autoWidth=T){
  list(scrollX=scrollX, scrollY=scrollY,
       paging=paging, searching=searching, info=info, pageLength = pageLength,
       autoWidth = autoWidth,
       columnDefs = list(
         list(
           targets = "_all",
           render = JS(paste0("$.fn.dataTable.render.ellipsis( ",max_char,", false )"))
         ))
  )
}


# ===================== remove previous files generated by pathview =====================
do.call(file.remove, list(list.files(paste0(getwd(),"/www/"),full.names = TRUE)[grepl("pdf$|xml$|png$",list.files(paste0(getwd(),"/www/")))]))
