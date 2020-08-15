# ==== global.R START ===========================================================

# --------------- Load and configure libraries -------------------

library(shiny)
library(ggplot2)
library(DT)
# library(pheatmap)
library(UpSetR)
library(eulerr)
library(VennDiagram)
library(wordcloud)
library(dplyr)
library(plyr)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(rvest)
library(htmlwidgets)
library(data.table)
# library(dqshiny)
library(shinythemes)
library(shinyBS)
library(BiocManager)
options(repos = BiocManager::repositories())
futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger") # to suppress creation of log file

# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# BiocManager::install("fgsea")
# setRepositories(addURLs = c(BioC = "https://bioconductor.org/packages/3.8/bioc"))



# ------------- Miscellaneous functions ------------------

# calculate and append rank
append_rank = function(x){
    x$Rank <- -sign(x$Stat) * log10(x$PValue)
    return(x)}
# remove na rows
remove_nas = function(x){return (x[complete.cases(x), ])}


# tidy filenames and prevent duplicates
tidy_filename <- function(name, ll){
  # get rid of extension
  name <- gsub(".csv","",name)
  name <- gsub(".txt","",name)
  
  # if the name is duplicated, add (2) to the end
  repeat{
    if(name %in% ll){
      name <- paste0(name," (2)")
    }
    else{break}
  }
  return(name)
}

addlinebreaks <- function(x, max, lbtype="<br>"){
  # this cuts at spaces/ ;/ underscore/ period
  gsub(paste0('(.{1,',max,'})(\\s|;|_|\\.|$)'), paste0('\\1',lbtype), x)
}

# pattern matching of t/f vector. in the pattern, na is what you don't care about.
match_skipna <- function(x,pattern){
  match <- na.omit(x==pattern)
  return(all(match))
}


roundDown <- function(x,to=0.01)
{
  to*(x%/%to - as.logical(x%%to))+to
}

generate_scale <- function(n, ivs=10){
  logscale = 10^floor(log10(n/ivs))
  step = roundDown(n/ivs,logscale)
  max=roundDown(n,step)
  seq(0,max,step)
}

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

# ------------- Database management functions ------------------

# given a vector of df names, merge those dfs and annotate the columns with df names
#-----------------------------
# example: df_n <- build_df_n(input$heatmap_dfs, rv$gg, rv$ll, input_mode="names")

build_df_n <- function(input, gg, ll, input_mode){
  
  if(input_mode == "names"){
    names <- input
    # get vector of indices from names
    indices <- unlist(lapply(names, function(x){ match(x, ll) }))
  } else if (input_mode=="indices"){
    indices <- input
    names <- unlist(lapply(indices, function(x){ ll[[x]] }))
  }

  # gather dfs into list
  data <- lapply(indices, function(x){ gg[[x]] }) 
  
  # annotate the non-name columns in each df in list with filename
  data <- lapply(seq_along(data), function(x, y){
    colnames(data[[x]])[-1] <- paste(colnames(data[[x]])[-1],y[[x]], sep="_")
    return (data[[x]])}
    , y=names)
  
  # merge list of dfs on gene
  df <- Reduce(function(x,y) merge(x,y, by = "Name", all=T), data) 
  
  return(df)
}

# given a vector of df names, detect number of shared cols and rows
#-----------------------------
# example: detect_shared_dimensions(rv$heatmap_i, rv$gg, rv$ll, input_mode="indices")
# gives a list. call list$shared_cols and list$shared_rows

detect_shared_dimensions <- function(input, gg, ll, input_mode){
  if(input_mode == "names"){
    names <- input
    # get vector of indices from names
    indices <- unlist(lapply(names, function(x){ match(x, ll) }))
  } else if (input_mode=="indices"){
    indices <- input
    names <- unlist(lapply(indices, function(x){ ll[[x]] }))
  }
  
  # get list of columns
  colns <- lapply(indices, function(x){
    colnames(gg[[x]][-1]) # exclude genename column
  })
  
  # get list of rows
  rowns <- lapply(indices, function(x){
    gg[[x]]$Name 
  })
  
  out <- list(shared_cols = Reduce(intersect, colns),
              shared_rows = Reduce(intersect, rowns))
  return(out)
}



# ------------- filtering functions ------------------

# apply cutoffs globally
#-----------------------------
# example: apply_n_cutoffs(df_n, p=0.05, q=0.05, stat=1, tolerate=F)
# tolerate: ignore na values

apply_n_cutoffs <- function(df, p, q, stat, tolerate=F){
  if (tolerate ==T){
    naval <- 123456
    df[is.na(df)] <- -naval
    df <- df %>% mutate(m = do.call(pmax, dplyr::select(df, contains("PValue_")))) %>%
      filter(m < p)
    df <- df %>% mutate(m = do.call(pmax, dplyr::select(df, contains("FDR_")))) %>%
      filter(m < q)
    df[df==-naval] <- naval
    df <- df %>% mutate(m = do.call(pmin, abs(dplyr::select(df, contains("Stat"))))) %>%
      filter(m > stat)
    df <- df[1:(length(df)-1)] # delete last helper column
    df[df==naval] <- NA
  }
  else if (tolerate ==F){
    df <- df %>% mutate(m = do.call(pmax, dplyr::select(df, contains("PValue_")))) %>%
      filter(m < p)
    df <- df %>% mutate(m = do.call(pmax, dplyr::select(df, contains("FDR_")))) %>%
      filter(m < q)
    df <- df %>% mutate(m = do.call(pmin, abs(dplyr::select(df, contains("Stat"))))) %>%
      filter(m > stat)
    df <- df[1:(length(df)-1)] # delete last helper column
  }
  
  
  
  return(df)
}



# apply cutoffs to a single dataset
#-----------------------------
# example: apply_single_cutoff(df_n, "efk1SvF", p=0.05, q=0.05, stat=1, tolerate=F)
# this applies the cutoff to cols named "PValue_efk1SvF", "FDR_efk1SvF", "Stat_efk1SvF"

apply_single_cutoff <- function(df, colname, p, q, stat, tolerate=F){
  pcol <- paste0("PValue_",colname)
  qcol <- paste0("FDR_",colname)
  statcol <- paste0("Stat_",colname)
  
  # first, filter while ignoring na in the selected col.
  df <- df %>% filter(!!sym(pcol) <= p | is.na(!!sym(pcol)))
  df <- df %>% filter(!!sym(qcol) <= q | is.na(!!sym(qcol)))
  df <- df %>% filter(abs(!!sym(statcol)) >= stat | is.na(!!sym(statcol)))
  
  # if no na allowed, get rid of nas in the selected col.
  if (tolerate==F){
    df <- df[is.na(df[,pcol])==F,]
    df <- df[is.na(df[,qcol])==F,]
    df <- df[is.na(df[,statcol])==F,]
  }
  return(df)
}


# filter column by sign.
#-----------------------------
# example: filter_by_sign(df_n, "Stat_efk1SvF", sign="Positive", tolerate=F)
# df: the df you want filtered; 
# colname: substring to select columns. e.g. "Stat" (if used globally); or "Stat_efk1SvF" if used on 1 dataset
# sign: "All", "Positive" or "Negative"

filter_by_sign <- function(df, colname, sign, tolerate=F){
  naval <- 123456
  
  if (sign == "Positive"){
    if (tolerate ==T){
      df[is.na(df)] <- naval
    } 
    df <- df %>% mutate(m = do.call(pmin, dplyr::select(df, contains(colname)))) %>%
      filter(m > 0)
    df <- df[1:(length(df)-1)] # delete last helper column
    if (tolerate ==T){df[df==naval] <- NA} 
  }
  else if (sign == "Negative"){
    if (tolerate ==T){
      df[is.na(df)] <- -naval
    }
    df <- df %>% mutate(m = do.call(pmax, dplyr::select(df, contains(colname)))) %>%
      filter(m < 0)
    df <- df[1:(length(df)-1)] # delete last helper column
    if (tolerate ==T){df[df==-naval] <- NA}
  }
  return(df)
}



# --------------- Presets -------------------

options(spinner.color="#0dc5c1")

# red colorscale
cscale <- list(c(0, "rgb(255, 255, 255)"), # 0 = white
               list(0.200687, "rgb(254,224,144)"), # 0.25 = light yellow
               list(0.333333, "rgb(253,174,97)"), # 0.1 = yellow
               list(0.433677, "rgb(244,109,67)"), # 0.05 = orange
               list(0.666667, "rgb(215,48,39)"), # 0.01 = red
               list(1, "rgb(165,0,38)") # 0.001 = dark red
)
#adapted from colorbrewer blues
cscale_bu2 <- list(c(0, "rgb(255, 255, 255)"), # 0 = white
                   list(0.200687, "rgb(198,219,239)"), # 0.25 = pale blue
                   list(0.333333, "rgb(158,202,225)"), # 0.1 = light blue
                   list(0.433677, "rgb(107,174,214)"), # 0.05 = blue
                   list(0.666667, "rgb(49,130,189)"), # 0.01 = darker blue
                   list(1, "rgb(8,81,156)") # 0.001 = cornflower
)
# -3 to 3 colorscale adapted from cscale + cscale_bu2. Ideal for -log10(p)*sign(logFC).
cscale_div <- list(c(0, "rgb(8,81,156)"), # -0.001 = cornflower,
                   list(0.166667, "rgb(49,130,189)"), # -0.01 = darker blue
                   list(0.283161, "rgb(107,174,214)"), # -0.05 = blue
                   list(0.333333, "rgb(158,202,225)"), # -0.1 = light blue
                   list(0.399656, "rgb(198,219,239)"), # -0.25 = pale blue
                   list(0.5, "rgb(255, 255, 255)"), # 0 = white
                   list(0.600344, "rgb(254,224,144)"), # 0.25 = light yellow
                   list(0.666667, "rgb(253,174,97)"), # 0.1 = yellow
                   list(0.716839, "rgb(244,109,67)"), # 0.05 = orange
                   list(0.833334, "rgb(215,48,39)"), # 0.01 = red
                   list(1, "rgb(165,0,38)") # 0.001 = dark red
)

# -3 to 3 colorscale adapted from cscale + cscale_bu2. evenly distributed.
cscale_simple <- list(c(0, "rgb(8,81,156)"), # -0.001 = cornflower,
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

gene_alias <- c("(?i)g(?-i)ene","(?i)n(?-i)ame","(?i)g(?-i)ene.(?i)n(?-i)ame",
                "(?i)S(?-i)ymbol",
                "(?i)p(?-i)athway", "(?i)p(?-i)twy"
)
stat_alias <- c("log2(?i)FC(?-i)","log(?i)fc(?-i)",
                "(?i)e(?-i)nrichment.(?i)s(?-i)core","^(?i)ES(?-i).*",
                "^(?i)S(?-i)tat*"
)
p_alias <- c("(?i)pv(?-i)al.*","(?i)p(?-i).(?i)v(?-i)al.*",
             "^(?i)P(?-i)"
)
q_alias <- c("(?i)qv(?-i)al.*","(?i)q(?-i).(?i)v(?-i)al.*", 
             "^(?i)q(?-i)",
             "(?i)FDR(?-i)", 
             "(?i)p(?-i)adj", 
             "(?i)a(?-i)dj.(?i)p(?-i).(?i)v(?-i)al"
)




# --------------- Initialize -------------------

# list filenames
ll <- c("Sample_Hibshman_lite.csv"
        # , "up_hif-1hypoxia_vs_normoxia.csv", "up_N2hypoxia_vs_normoxia.csv", "up_nhr-49hypoxia_vs_normoxia.csv"
        # , "t_efk1starved_vs_fed_vsN2_KEGG.csv", "t_N2starved_vs_fed_KEGG.csv"
        # , "z_efk1starved_vs_fed.csv", "z_N2starved_vs_fed.csv","z_zip2starved_vs_fed.csv"
        )
# list paths
pl <- paste0(getwd(),"/inc/", ll, sep = '')
# list dfs
gg <- lapply(pl, read.csv)
# remove any row with na anywhere
gg <- lapply(gg, remove_nas)


ll <- lapply(ll, tidy_filename, list())


tt <- list("logFC"
           # , "logFC", "logFC", "logFC"
           # , "ES", "ES"
           # , "logFC", "logFC"
           )


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
intros


# ===================================================== global.R END ============

# Run the application 
#shinyApp(ui = ui, server = server)
