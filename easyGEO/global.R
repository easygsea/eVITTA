library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyBS)
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
options(repos = BiocManager::repositories())

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

# tabulate outputs of list function (for platform selection)
# --------------------------------------------------------
tabulate <- function(object, FUN){
  do.call(rbind, lapply(object, FUN))
}

# find columns that have one value and return named list of those values
find_repeating_values <- function(df){
  df <- df[vapply(df, function(x) length(unique(x)) == 1, logical(1L))]
  as.list(df[1,])
}

# To transform named list to dataframe
# ---------------------------------------------------------
#   named_list_to_df(list, colnames)
# example:
#   named_list_to_df(gse_meta(), c("Field", "Value"))

named_list_to_df <- function(list, colnames){
  df <- data.frame(cbind(as.character(names(list)),
                         as.character(unname(unlist(list)))
  ))
  colnames(df) <- colnames
  df
}

# tidy df "field" columns
# ---------------------------------------------------------
tidy_field_col <- function(vector){
  # replace underscores
  vector <- unlist(lapply(vector, function(x){
    gsub("_"," ",x)
  }))
  # capitalize first word
  vector <- sub("(.)", "\\U\\1", vector, perl=TRUE)
  vector
}

# To extract all matched elements from named list
# --------------------------------------------------------
#   grep_multiple(to_match, grep_from, order=F)
# example:
#   grep_multiple(c("title", "study_type", "sample_count", "organism", "summary"), isolate(all_fields())$Field, order=T)
grep_multiple <- function(to_match, grep_from, order=F){
  if (order==F){
    matches <- unique(grep(paste(to_match,collapse="|"),
                           grep_from, value=TRUE))
  } else {
    matches <- unlist(lapply(to_match, function(x){
      grep(x, grep_from, value=TRUE)
    }))
  }

  matches
}

# transform characteristics column into named vector
# --------------------------------------------------------
transform_vector <- function(vector, sep=": "){
  unlist(lapply(vector, function(x){
    if(is.na(x)==F){

      ss <- strsplit(x, sep)[[1]]
      out <- ss[[2]]
      names(out) <- ss[[1]]


      return(out)
    }
  }))
}




# apply options to datatable. example: options = dt_options(80,F,F,T,T,T,10)
# To apply options to datatable. only works with ellipsis enabled
# ------------------------------------------------------------
#   dt_options (max_char, scrollX=F, scrollY=F, paging=T, searching=T, info=T, pageLength = 10, autoWidth=T)
# example:
#   renderDataTable({df}, plugins="ellipsis", options = dt_options(80,F,F,T,T,T,10))

dt_options <- function(max_char, scrollX=F, scrollY=F, paging=T, searching=T, info=T, pageLength = 10, autoWidth=T){
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



# To translate between sample names and GSE accessions (or other information in phenoData)
# ------------------------------------------------------------
#   translate_sample_names(original_vector, dict_df, output_type)
#
# example:
#   input = c("GSM3610107", "GSM3610108", "GSM3610109", "GSM3610110", "GSM3610111", "test")
# translate_sample_names(input,  rv$pdata[c("title", "geo_accession")],  "title")
# > "N2_AL_1", "hlh-30_AL_1", "N2_ARD_1", "hlh-30_ARD_1", "N2_AL_2", "test"
# anything not found is returned as is

translate_sample_names <- function(original_vector, dict_df, output_type){
  # try to match vector to every column in dict and get a score
  matches <- sort(unlist(lapply(dict_df, function(x){
    length(intersect(original_vector, x))
  })), decreasing = T)
  input_coln <- names(matches)[[1]] # this is the detected input column

  # translate according to dict df. if not found, preserve the original value
  output_vector <- unlist(lapply(original_vector, function(x){
    output_value <- dict_df[dict_df[[input_coln]]==x, output_type]
    if (identical(output_value, character(0))) {
      return (x)
    } else {
      return (output_value)
    }
  }))
  output_vector
}
# example input: "GSM3610107" "GSM3610108" "GSM3610109" "GSM3610110" "GSM3610111" "test"
# example output: "N2_AL_1"      "hlh-30_AL_1"  "N2_ARD_1"     "hlh-30_ARD_1" "N2_AL_2" "test"







# Function to call in place of dropdownMenu
# --------------------------------------------------------
dropdownMenuCustom <-     function (..., type = c("messages", "notifications", "tasks"),
                                    badgeStatus = "primary", icon = NULL, .list = NULL, customSentence)
{
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  if (is.null(icon)) {
    icon <- switch(type, messages = shiny::icon("envelope"),
                   notifications = shiny::icon("warning"), tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- span(class = paste0("label label-", badgeStatus),
                  numItems)
  }
  tags$li(
    class = dropdownClass,
    a(
      href = "#",
      class = "dropdown-toggle",
      `data-toggle` = "dropdown",
      icon,
      badge
    ),
    tags$ul(
      class = "dropdown-menu",
      tags$li(
        class = "header",
        customSentence(numItems, type)
      ),
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}
