library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(GEOquery)
library(dplyr)
library(shinyjs)
library(DT)
library(BiocManager)
options(repos = BiocManager::repositories())




# tabulate outputs of list function (for platform selection)
tabulate <- function(object, FUN){
  do.call(rbind, lapply(object, FUN))
}

# find columns that have one value and return named list of those values
find_repeating_values <- function(df){
  df <- df[vapply(df, function(x) length(unique(x)) == 1, logical(1L))]
  as.list(df[1,])
}

# transform named list to dataframe for display
named_list_to_df <- function(list, colnames){
  df <- data.frame(cbind(as.character(names(list)),
                         as.character(unname(unlist(list)))
  ))
  colnames(df) <- colnames
  df
}

# tidy df "field" columns
tidy_field_col <- function(vector){
  # replace underscores
  vector <- unlist(lapply(vector, function(x){
    gsub("_"," ",x)
  }))
  # capitalize first word
  vector <- sub("(.)", "\\U\\1", vector, perl=TRUE)
  vector
}

# grep multiple patterns (provided in a vector)
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
dt_options <- function(max_char=80, scrollX=F, scrollY=F, paging=T, searching=T, info=T, pageLength = 10){
  list(scrollX=scrollX, scrollY=scrollY, 
       paging=paging, searching=searching, info=info, pageLength = pageLength,
       columnDefs = list(
         list(
           targets = "_all",
           render = JS(paste0("$.fn.dataTable.render.ellipsis( ",max_char,", false )"))
         ))
  )
}