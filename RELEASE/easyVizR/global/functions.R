# ================================================= #
#                   Calculation                   ####
# ================================================= #

# calculate and append rank
append_rank = function(x){
  x$Rank <- -sign(x$Stat) * log10(x$PValue)
  return(x)}
# remove na rows
remove_nas = function(x){return (x[complete.cases(x), ])}



# ================================================= #
#                Numeric processing                 ####
# ================================================= #

# round down to an integer or decimal value
#----------------------------------------------
roundDown <- function(x,to=0.01)
{
  to*(x%/%to - as.logical(x%%to))+to
}

# function to generate a input scale from max value
#----------------------------------------------
generate_scale <- function(n, ivs=10){
  logscale = 10^floor(log10(n/ivs))
  step = roundDown(n/ivs,logscale)
  max=roundDown(n,step)
  seq(0,max,step)
}



# ================================================= #
#                Text processing                    ####
# ================================================= #


# tidy filenames and prevent duplicates
#--------------------------------------------
# duplicate filenames will be appended with (2)

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







# ================================================= #
#                 Matching functions                ####
# ================================================= #


# match string to list and return first match in list. if not found, return null
# (use this to detect and match column names)
#----------------------------------------------
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
#---------------------------------------------
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






# ================================================= #
#                Dataframe functions                ####
# ================================================= #

# MASTER DATAFRAME BUILDING
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

# SHARED DIMENSIONS
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


# get cols or colnames by class
#------------------------------------
# output_type: statnames = the statistic name only, (e.g. PValue)
# colnames = full names of all the fitting cols
# cols = Name column, plus all the fitting cols content
# example: get_cols_by_class(df, is.numeric, output_type="cols")

get_cols_by_class <- function(df, FUN, output_type="statnames"){
  # get all numeric columns
  num_cols <- lapply(seq_along(df), function(x){
    if (FUN(df[[x]])){
      colnames(df)[[x]]
    }
  })
  num_cols <- na.omit(unlist(num_cols))
  
  if (output_type=="statnames"){
    # get the strings before underscore and filter out the unique ones
    out <- unique(unlist(lapply(num_cols, function(x){
      strsplit(x, "_")[[1]][[1]]
    })))
    
  } else if (output_type=="colnames"){
    out <- num_cols
  } else if (output_type=="cols"){
    # get the Name column and the cols content
    out <- df[,unique(c("Name", num_cols))]
  }
  
  out
}





# ================================================= #
#                Dataframe FILTERING                ####
# ================================================= #

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



# apply cutoffs to a single dataset (USE THIS)
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


# filter column by sign. (USE THIS)
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



# ================================================= #
#                Data summarizing                   ####
# ================================================= #


# generate summary df for factor column (e.g. color)
# -------------------------------------------

summarize_factor_column <- function(df, colname="color"){
  # generate properties table
  summary <- c("total"=nrow(df), table(df[[colname]]))
  summary_df <- t(data.frame(as.list(summary)))
  summary_df <- data.frame(summary_df)
  # add genes to the table
  unicl <- unique(df[[colname]])
  gene_cats <- lapply(unicl, function(x){
    gl <- df[which(df[[colname]]==x),][["Name"]]
    paste(gl, collapse=" ")
  })
  gene_cats <- c(paste(df$Name, collapse=" "),gene_cats)
  summary_df$genes <- unlist(gene_cats)
  colnames(summary_df) <- c("n","Names")
  summary_df
}



# ================================================= #
#                CSS / HTML styling                 ####
# ================================================= #


# calculate dropdown ui width when containing multiple divs
# -------------------------------------------
# example: calc_dropdown_width(4, 225, 70, max_per_row=3)
calc_dropdown_width <- function(box_n, box_width, margin_width, max_per_row=3, unit="px"){
  if (box_n >= max_per_row){
    divs_per_row <- max_per_row
  } else {
    divs_per_row <- box_n
  }
  w <- (box_width * divs_per_row) + margin_width
  out <- paste0(w, unit)
  out
}



# LABELS WITH HELP BUTTONS 
#----------------------------------------------------------
# (need to wrap in HTML)
# example of use: label=HTML("Label here", add_help("id1", style="padding:1px 1px 1px 1px;") )
add_help <- function(id, color="#00c0ef", style=""){
  out <- paste0("<i class='fa fa-question-circle' 
                style = 'color:",color,";
                font-size:medium;",style,"' 
                id='",id,"'></i>")
  HTML(out)
}


# LABELS WITH CLICKABLE BS BUTTON 
#----------------------------------------------------------
# construct a label with a clickable help bs button
label_with_help_bttn <- function(label_text, bttn_id, bttn_status="info", bttn_style=""){
  p(style="margin-block-end: 2px;",
    label_text,
    tags$style(type = "text/css", paste0("#",bttn_id,"{display: inline-block;width: 17px;height: 17px;padding: 0;border-radius: 50%;vertical-align: text-top;margin-left: 3px;font-size: 10px;padding-top: 1px;",bttn_style,"}")),
    bsButton(bttn_id, label = "", icon = icon("question"), style = bttn_status, size = "extra-small"))
}



# Function to draw an info box to guide the user along the pipeline
#--------------------------------------------------------
# You can pass html string into msg, e.g. : guide_box("<strong>This is a bold message</strong>")
# default color is blue
# default width is 12 (maximum), must be is an integer value
# To make it appear on condition, call it in a conditional renderUI({})

guide_box <- function(msg, color="blue", width=12){
  box(
    title = NULL, background = color, solidHeader = TRUE, width=width,
    HTML(msg)
  )
}



# function to detect if box is collapsed (keep in global)
#-------------------------------------------------------
# need to call in ui  like so: collapseInput(inputId = "iscollapsebox1", boxId = "box1"),
# access the value using input$iscollapsebox1
collapseInput <- function(inputId, boxId) {
  tags$script(
    sprintf(
      "$('#%s').closest('.box').on('hidden.bs.collapse', function () {Shiny.onInputChange('%s', true);})",
      boxId, inputId
    ),
    sprintf(
      "$('#%s').closest('.box').on('shown.bs.collapse', function () {Shiny.onInputChange('%s', false);})",
      boxId, inputId
    )
  )
}
# ================================================= #
#                RRHO functions                ####
# ================================================= #

#This section is dedicated to functions related to RRHO, 
#which is a new feature we are plannnig to develop in Visr visualization tab


