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



# translate vector of e.g. gsm ids, to sample names, etc, provided with the translation df.
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


