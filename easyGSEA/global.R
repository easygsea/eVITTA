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
library(shinydisconnect)
library(lobstr)
library(rintrojs)
library(dendextend) # for dendrogram
library(ggdendro) # for dendrogram

options(repos = BiocManager::repositories())

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

# add help buttons to labels (need to wrap again in HTML)
# example of use: label=HTML("Label here", add_help("id1", style="padding:1px 1px 1px 1px;") )
add_help <- function(id, color="#00c0ef", style=""){
  out <- paste0("<i class='fa fa-question-circle'
                style = 'color:",color,";
                font-size:medium;",style,"'
                id='",id,"'></i>")

  HTML(out)
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
  "Sus scrofa" = "ssc",
  "Other (custom GMT)" = "other"
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
col_gene_names <- c("id","gene","symbol","genesymbol","gene_id","genename","name","x1","x")
col_fc_names <- c("logfc","fc","log2_fold_change","log2FoldChange")
col_p_names <- c("p","pval","pvalue","p.value","p_value")
col_fdr_names <- c("fdr","padj","adj.p.val","q_value")
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

# ======================= renderDataTable options ===========================
# apply options to datatable. example: options = dt_options(80,F,F,T,T,T,10)
# To apply options to datatable. only works with ellipsis enabled
#   dt_options (max_char, scrollX=F, scrollY=F, paging=T, searching=T, info=T, pageLength = 10, autoWidth=T)
# example:
#   renderDataTable({df}, plugins="ellipsis", options = dt_options(80,F,F,T,T,T,10))

dt_options <- function(max_char=80, scrollX=T, scrollY=F, paging=T, searching=T, info=T, pageLength = 5, autoWidth=T){
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
