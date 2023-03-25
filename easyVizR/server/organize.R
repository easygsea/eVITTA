
#======================================================================#
####                      ORGANIZE FILES                            ####
#======================================================================#

# specify the upload limits here (in mb)
batch_mb_limit <- 50
single_mb_limit <- 10
total_mb_limit <- 100

# calculate upload limits in bytes
batch_upload_limit <- batch_mb_limit*1024^2
single_upload_limit <- single_mb_limit*1024^2
total_upload_limit <- total_mb_limit*1024^2


# specify allowed characters
allowed_chars <- "[^(a-z0-9A-Z+><%\\s)|[:punct:]]"
var_allowed_chars <- "[^a-z0-9A-Z+><_\\-.]"

# file reading options
csvFileEncoding <- "UTF-8" # default encoding for reading csv
csvCheckNames <- T


tidyInDF <- function(in_df){
  # convert columns to appropriate classes
  in_df$Name <- as.character(in_df$Name)
  in_df$Stat <- as.numeric(in_df$Stat)
  in_df$PValue <- as.numeric(in_df$PValue)
  in_df$FDR <- as.numeric(in_df$FDR)

  # pre-filter rows
  in_df <- in_df[nchar(in_df$Name) != 0,] # delete rows with empty Name

  # process name column
  in_df$Name <- toupper(in_df$Name) # convert name to upper case
  if (any(duplicated(in_df$Name))){in_df$Name <- make.unique(in_df$Name)} # make names unique

  # process other columns
  in_df <- in_df %>% mutate_all(function(x) ifelse(is.nan(x) | is.infinite(x), NA, x)) # convert +-Inf to NA

  # post-filter columns
  in_df <- in_df[rowSums(is.na(in_df)) != ncol(in_df),] # delete rows that are entirely NA
  in_df <- in_df[complete.cases(in_df[,c("Name","Stat","PValue","FDR")]),] # remove rows containing any NA in essential cols
  # in_df <- remove_nas(in_df) # remove rows containing any NA

  in_df
}


tidyInDF <- function(in_df){
  # convert columns to appropriate classes
  in_df$Name <- as.character(in_df$Name)
  in_df$Stat <- as.numeric(in_df$Stat)
  in_df$PValue <- as.numeric(in_df$PValue)
  in_df$FDR <- as.numeric(in_df$FDR)

  # pre-filter rows
  in_df <- in_df[nchar(in_df$Name) != 0,] # delete rows with empty Name

  # process name column
  in_df$Name <- toupper(in_df$Name) # convert name to upper case
  if (any(duplicated(in_df$Name))){in_df$Name <- make.unique(in_df$Name)} # make names unique

  # process other columns
  in_df <- in_df %>% mutate_all(function(x) ifelse(is.nan(x) | is.infinite(x), NA, x)) # convert +-Inf to NA

  # post-filter columns
  in_df <- in_df[rowSums(is.na(in_df)) != ncol(in_df),] # delete rows that are entirely NA
  in_df <- in_df[complete.cases(in_df[,c("Name","Stat","PValue","FDR")]),] # remove rows containing any NA in essential cols
  # in_df <- remove_nas(in_df) # remove rows containing any NA

  in_df
}


tidyInDF <- function(in_df){
  # convert columns to appropriate classes
  in_df$Name <- as.character(in_df$Name)
  in_df$Stat <- as.numeric(in_df$Stat)
  in_df$PValue <- as.numeric(in_df$PValue)
  in_df$FDR <- as.numeric(in_df$FDR)

  # pre-filter rows
  in_df <- in_df[nchar(in_df$Name) != 0,] # delete rows with empty Name

  # process name column
  in_df$Name <- toupper(in_df$Name) # convert name to upper case
  if (any(duplicated(in_df$Name))){in_df$Name <- make.unique(in_df$Name)} # make names unique

  # process other columns
  in_df <- in_df %>% mutate_all(function(x) ifelse(is.nan(x) | is.infinite(x), NA, x)) # convert +-Inf to NA

  # post-filter columns
  in_df <- in_df[rowSums(is.na(in_df)) != ncol(in_df),] # delete rows that are entirely NA
  in_df <- in_df[complete.cases(in_df[,c("Name","Stat","PValue","FDR")]),] # remove rows containing any NA in essential cols
  # in_df <- remove_nas(in_df) # remove rows containing any NA

  in_df
}


tidyInDF <- function(in_df){
  # convert columns to appropriate classes
  in_df$Name <- as.character(in_df$Name)
  in_df$Stat <- as.numeric(in_df$Stat)
  in_df$PValue <- as.numeric(in_df$PValue)
  in_df$FDR <- as.numeric(in_df$FDR)

  # pre-filter rows
  in_df <- in_df[nchar(in_df$Name) != 0,] # delete rows with empty Name

  # process name column
  in_df$Name <- toupper(in_df$Name) # convert name to upper case
  if (any(duplicated(in_df$Name))){in_df$Name <- make.unique(in_df$Name)} # make names unique

  # process other columns
  in_df <- in_df %>% mutate_all(function(x) ifelse(is.nan(x) | is.infinite(x), NA, x)) # convert +-Inf to NA

  # post-filter columns
  in_df <- in_df[rowSums(is.na(in_df)) != ncol(in_df),] # delete rows that are entirely NA
  in_df <- in_df[complete.cases(in_df[,c("Name","Stat","PValue","FDR")]),] # remove rows containing any NA in essential cols
  # in_df <- remove_nas(in_df) # remove rows containing any NA

  in_df
}

tidyInDF <- function(in_df){
  # convert columns to appropriate classes
  in_df$Name <- as.character(in_df$Name)
  in_df$Stat <- as.numeric(in_df$Stat)
  in_df$PValue <- as.numeric(in_df$PValue)
  in_df$FDR <- as.numeric(in_df$FDR)

  # pre-filter rows
  in_df <- in_df[nchar(in_df$Name) != 0,] # delete rows with empty Name

  # process name column
  in_df$Name <- toupper(in_df$Name) # convert name to upper case
  if (any(duplicated(in_df$Name))){in_df$Name <- make.unique(in_df$Name)} # make names unique

  # process other columns
  in_df <- in_df %>% mutate_all(function(x) ifelse(is.nan(x) | is.infinite(x), NA, x)) # convert +-Inf to NA

  # post-filter columns
  in_df <- in_df[rowSums(is.na(in_df)) != ncol(in_df),] # delete rows that are entirely NA
  in_df <- in_df[complete.cases(in_df[,c("Name","Stat","PValue","FDR")]),] # remove rows containing any NA in essential cols
  # in_df <- remove_nas(in_df) # remove rows containing any NA

  in_df
}


#
#
# ####---------------------- From existing data --------------------------####
#
# output$add_subset_select <- renderUI({
#   selectInput(
#     inputId = "add_subset_select",
#     label = "Select parent dataset:",
#     choices = c("Select..."="", rv$ll),
#     selectize = FALSE) # show duplicate entries
# })
#
# output$add_subset_opt <- renderUI({
#   req(input$add_subset_select!="")
#   div(
#     column(3,
#            numericInput("add_subset_p", "P filter:", value = 0.05, min = 0, max = 1, step=0.001, width="100px")),
#     column(3,
#            numericInput("add_subset_q", "FDR filter:", value = 1, min = 0, max = 1, step=0.001, width="100px")),
#     column(3,
#            numericInput("add_subset_Stat", "|Stat| filter:", value = 0, min = 0, max = 5, step=0.1, width="100px")),
#     column(3,
#            radioGroupButtons("add_subset_sign",
#                              label = "Filter Stat by:",
#                              choices=c("All", "+", "-"),
#                              selected="All",size="s",direction = "horizontal"))
#   )
# })
#
# # constrain numericinput (???)
# observeEvent(input$add_subset_p, {
#   x <- input$add_subset_p
#   if (is.na(x)){
#     updateNumericInput(session, "add_subset_p", value = 0.05)
#   }
# })
#
# # render preview table
# output$add_subset_preview <- renderUI({
#   req(input$add_subset_select!="")
#   box(width=12, title = "Preview", solidHeader = F, collapsible = TRUE, status="info",
#       dataTableOutput("preview_tbl", width = "100%",height="100%")
#   )
# })
#
# output$preview_tbl <- DT::renderDataTable({
#   req(input$add_subset_select!="")
#
#   df <- rv$preview
#   # to round everything down to 3 decimals
#   df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
#   df
#
# }, options = list(scrollX=T,
#                   searching = FALSE, pageLength = 5))
#
#
# # input filename
# output$add_subset_name <- renderUI({
#   req(input$add_subset_select!="")
#   req(is.null(rv$preview_i)==F)
#   req(length(rv$ll) >= rv$preview_i)
#
#   if(is.null(input$add_subset_p)==F && is.null(input$add_subset_q)==F &&
#      is.null(input$add_subset_Stat)==F && is.null(input$add_subset_sign)==F){
#     tempname <- paste(rv$ll[[rv$preview_i]],
#                       input$add_subset_p, input$add_subset_q,
#                       input$add_subset_Stat, input$add_subset_sign,
#                       sep="_")
#   }
#   else{ tempname <- "" }
#
#   textInput(
#     inputId = "add_subset_name",
#     label = "Name this new dataset:",
#     value= tempname)
# })
#
#
# # observe and update trimmed table
# observe({
#   req(length(rv$gg)>0)
#   req(input$add_subset_select!="")
#   req(is.null(input$add_subset_p)==F && is.null(input$add_subset_q)==F &&
#         is.null(input$add_subset_Stat)==F && is.null(input$add_subset_sign)==F)
#
#   i <- match(input$add_subset_select, rv$ll)
#   req(is.null(rv$gg[[i]])==F)
#   df <- rv$gg[[i]]
#   df <- df[df$PValue <= input$add_subset_p,]
#   df <- df[df$FDR <= input$add_subset_q,]
#
#   if (input$add_subset_sign=="+"){
#     df <- df[df$Stat >= input$add_subset_Stat,]
#   }
#   else if (input$add_subset_sign=="-"){
#     df <- df[df$Stat <= -input$add_subset_Stat,]
#   }
#   else if (input$add_subset_sign=="All"){
#     df <- df[abs(df$Stat) >= input$add_subset_Stat,]
#   }
#
#   rv$preview_i <- i
#   rv$preview <- df
# })
#
#
#
# # upload / reset buttons
# output$add_subset_confirm <- renderUI({
#   req(input$add_subset_select!="")
#   req(input$add_subset_p >= 0 && input$add_subset_p <= 1)
#   req(input$add_subset_q >= 0 && input$add_subset_q <= 1)
#   req(input$add_subset_Stat >= 0)
#   req(nchar(input$add_subset_name)>0)
#
#   fluidRow(
#     box(width = 12, solidHeader = T,
#         splitLayout(
#           actionButton('as_submit', 'Confirm and add'),
#           actionButton('as_reset', 'Reset parameters')
#         )))
# })
#
# # when user presses reset
# observeEvent(input$as_reset, {
#   updateNumericInput(session, "add_subset_p", value = 0.05)
#   updateNumericInput(session, "add_subset_q", value = 1)
#   updateNumericInput(session, "add_subset_Stat", value = 0)
#   updateRadioGroupButtons(session, "add_subset_sign", selected = "All")
#
# })
#
# # when user presses submit
# observeEvent(input$as_submit, {
#   rv$gg <- c(rv$gg,list(rv$preview))
#   rv$ll <- c(rv$ll, input$add_subset_name)
#   rv$tt <- c(rv$ll, rv$tt[[rv$preview_i]])
#   updateSelectInput(session, "add_subset_select", selected="")
# })


####---------------------- Batch file upload --------------------------####





####================= multiple upload modal elements (show when file uploaded) =====================####

# select columns
output$batch_opt <- renderUI({

  req(rv$folder_upload_state == 'uploaded')
  req(length(rv$upload_batch_colscheme)>=4)

  # render default stat name
  statDefaultName <- firstmatch(stat_alias,rv$upload_batch_colscheme)
  if (is.null(statDefaultName)){ statDefaultName <- "Value" }

  div(
    HTML("<strong>Select columns corresponding to each element:</strong><br><br>"),
    fluidRow(
      column(3,
             radioButtons(
               inputId = "batch_gene_column",
               label = HTML(paste0(
                 "Name column:",
                 add_help("u_name_help", style="margin-left: 5px;"))
               ),
               choices = rv$upload_batch_colscheme,
               selected = firstmatch(gene_alias,rv$upload_batch_colscheme)
             ),
             bsTooltip("u_name_help",
                       "Gene, GeneName, Pathway, etc; an identifier.",
                       placement = "top"),
             radioButtons(
               inputId = "batch_p_column",
               label = HTML(paste0(
                 "PValue column:",
                 add_help("u_p_help", style="margin-left: 5px;"))
               ),
               choices = rv$upload_batch_colscheme,
               selected = firstmatch(p_alias,rv$upload_batch_colscheme)
             ),
             bsTooltip("u_p_help",
                       "PValue, pval, etc; the p-value.",
                       placement = "top"),
      ),
      column(3,
             radioButtons(
               inputId = "batch_Stat_column",
               label = HTML(paste0(
                 stat_replace("Stat column:"),
                 add_help("u_stat_help", style="margin-left: 5px;"))
               ),
               choices = rv$upload_batch_colscheme,
               selected = firstmatch(stat_alias,rv$upload_batch_colscheme)
             ),
             bsTooltip("u_stat_help",
                       "logFC, ES, etc; a main statistic that describes the magnitude of expression change.",
                       placement = "top"),
             radioButtons(
               inputId = "batch_q_column",
               label = HTML(paste0(
                 "FDR column:",
                 add_help("u_q_help", style="margin-left: 5px;"))
               ),
               choices = rv$upload_batch_colscheme,
               selected = firstmatch(q_alias,rv$upload_batch_colscheme)
             ),
             bsTooltip("u_q_help",
                       "FDR, q-value, padj, etc; a corrected p-value.",
                       placement = "top"),
      ),
      column(6,
             textInput(
               inputId = "batch_Stat_name",
               label = HTML(paste0(
                 stat_replace("Name the Stat column:"),
                 add_help("u_namestat_help", style="margin-left: 5px;"))
               ),
               value = statDefaultName,
             ),
             bsTooltip("u_namestat_help",
                       stat_replace("Tell us what kind of value is \"Stat\", e.g. logFC"),
                       placement = "top"),
             uiOutput("batch_additional_cols"),

      ),
    )
  )


})



# if there are other shared cols, also enable them to load those
output$batch_additional_cols <- renderUI({
  req(rv$folder_upload_state == 'uploaded')
  req(is.null(input$batch_gene_column)==F)
  req(is.null(input$batch_Stat_column)==F)
  req(is.null(input$batch_p_column)==F)
  req(is.null(input$batch_q_column)==F)
  req(length(rv$upload_batch_colscheme)>=4)

  j <- c(input$batch_gene_column, input$batch_Stat_column, input$batch_p_column, input$batch_q_column)

  additional_cols <- setdiff(rv$upload_batch_sharedcols,j)

  for(f in seq_along(additional_cols)){
    #delete the unrecognized character
    additional_cols[f] <- stringr::str_replace_all(additional_cols[f],"[^(a-z0-9A-Z%)|[:punct:]]", "")
  }

  # pickerInput(
  #   inputId = "batch_load_other_cols",
  #   label = "Load additional columns:",
  #   choices = additional_cols,
  #   selected = firstmatch(le_alias, additional_cols),
  #   options = list(
  #     `actions-box` = TRUE,
  #     size = 10,
  #     style = "btn-default",
  #     `selected-text-format` = "count > 0"
  #   ),
  #   multiple = TRUE
  # )

  multiInput(inputId = "batch_load_other_cols",
             label = "Load additional columns:",
             choices = additional_cols,
             selected = firstmatch(le_alias, additional_cols),
             width = "500px",
             options = list(
               enable_search = FALSE,
               non_selected_header = "Click to select:",
               selected_header = "Load these additional columns:")
  )
})


# render confirm and reset buttons
output$batch_buttons <- renderUI({


  fluidRow(
    div(style="display:inline-block;",
        uiOutput("batch_confirm_button")
    ),
    div(style="display:inline-block;",
        actionButton('g_reset', 'Reset upload')
    )
  )

})

output$batch_confirm_button <- renderUI({
  req(is.null(rv$folder_upload_state)==F)
  req(rv$folder_upload_state == 'uploaded')
  req(length(rv$upload_batch_colscheme)>=4)
  req(length(unique(c(input$batch_gene_column, input$batch_Stat_column, input$batch_p_column, input$batch_q_column)))>=4) # ensure columns input are unique
  req(length(rv$batch_failed)!=nrow(rv$batch_files)) # make sure not all files fail

  actionButton('batch_submit', 'Confirm and add', class = "btn-primary")
})


# ======================= feedback boxes ==========================
# first check if uploaded file list is ok
output$batch_feedback_1 <- renderUI({
  req(rv$folder_upload_state == 'uploaded')

  if (nrow(rv$batch_files)==0){ # catch no supported type file error
    box(
      title = NULL, background = "red", solidHeader = TRUE, width=12,
      paste0("WARNING: folder does not contain supported type files.")
    )
  }
  else if (length(rv$upload_batch_colscheme)<4){ # catch first file not ok error
    box(
      title = NULL, background = "red", solidHeader = TRUE, width=12,
      paste0("WARNING: not enough columns in the first file: ",rv$batch_files$name[[1]])
    )
  }
  else{
    inFiles <- rv$batch_files
    box(
      title = NULL, background = "green", solidHeader = TRUE, width=12,
      paste0("File uploaded: ",paste(inFiles$name, collapse=", "), " (",nrow(inFiles)," files)")
    )

  }
})

output$batch_feedback_2 <- renderUI({
  req(rv$folder_upload_state == 'uploaded')
  req(input$batch_gene_column)
  req(input$batch_Stat_column)
  req(input$batch_p_column)
  req(input$batch_q_column)

  cols <- c(input$batch_gene_column, input$batch_Stat_column, input$batch_p_column, input$batch_q_column)
  if (length(unique(cols))<4){
    box(
      title = NULL, background = "yellow", solidHeader = TRUE, width=12,
      "WARNING: Did you select a column twice?"
    )

  }
})

output$batch_feedback_3 <- renderUI({
  req(rv$folder_upload_state == 'uploaded')
  req(is.null(input$batch_gene_column)==F)
  req(is.null(input$batch_Stat_column)==F)
  req(is.null(input$batch_p_column)==F)
  req(is.null(input$batch_q_column)==F)
  req(length(rv$upload_batch_colscheme)>=4)
  req(length(unique(c(input$batch_gene_column, input$batch_Stat_column, input$batch_p_column, input$batch_q_column)))>=4)

  rv$batch_failed <- NULL
  failed=c()
  missingColumn = c()
  j <- c(input$batch_gene_column, input$batch_Stat_column, input$batch_p_column, input$batch_q_column)
  for (i in seq_along(rv$upload_batch_columns)) {
    if (all(j %in% rv$upload_batch_columns[[i]])){
      next
    }
    else{

      missingColumn <- c(missingColumn,list(j[!j %in% rv$upload_batch_columns[[i]]]))
      failed <- c(failed, rv$batch_files$name[[i]])

    }
  }
  if (is.null(failed)==F){
    rv$batch_failed <- failed
    #Add a for loop here to


    for (i in seq_along(failed)){
      ColumnsNeeded <- paste(missingColumn[i])
      #If missing column only has one element, then no need to trim the "c(" and ")" string from it



      if(length(missingColumn[[i]]) > 1){
        ColumnsNeeded <- substr(ColumnsNeeded, 3,nchar(ColumnsNeeded)-1)

      }
      else{
        ColumnsNeeded <- toString(dQuote(ColumnsNeeded))
      }

      failed[i] <- HTML(paste(failed[i], "misses column(s):", ColumnsNeeded),"<br/>")
    }

    box(
      title = NULL, background = "yellow", solidHeader = TRUE, width=12,
      #HTML(paste("Omitted these files because they don't have the corresponding columns:",paste(missingColumn, collapse=", "), " do not match in files: ",paste(failed, collapse="\n"), sep="<br/>"))
      HTML(paste("Omitted these files because they don't have the corresponding columns:", paste(failed, collapse="\n"), sep="<br/>"))
    )
  }
  else {
    box(
      title = NULL, background = "green", solidHeader = TRUE, width=12,
      "Files ok!"
    )
  }

})


# ======================= events ==========================

# when file is uploaded
observeEvent(input$fileIn, {
  # get files
  inFiles <- input$fileIn
  inFiles <- inFiles[grepl(".csv|.txt", inFiles$name),]

  #if(inFile$size >= 50*1024^2){
  if(sum(inFiles$size) >= batch_upload_limit){
    showModal(modalDialog(
      inputId = "size_reminder_modal1",
      div(
        paste0("The files you uploaded exceed ",batch_mb_limit,"MB, please modify it to proceed. Try to delete unneeded columns and
            only keep the columns that you are interested in.
            Then press \"Browse...\" to upload again. Thank you.")
        ,style="font-size:200%"),
      easyClose = TRUE,size="l"
      # , footer = modalButton("Close")
    ))
    shinyjs::reset("fileIn")
  } else if (sum(inFiles$size) + sum(rv$FileDF$size) >= total_upload_limit) {
    showModal(modalDialog(
      inputId = "size_reminder_modal2",
      div(
        paste0("You have exceeded your storage limit of ",total_mb_limit,"MB. Please delete the unneeded files.
            Then press \"Browse...\" to upload again. Thank you.")
        ,style="font-size:200%"),
      easyClose = TRUE,size="l"
      # , footer = modalButton("Close")
    ))
    shinyjs::reset("fileIn")
  }

  req((sum(inFiles$size) < batch_upload_limit)&&(sum(inFiles$size) + sum(rv$FileDF$size) < total_upload_limit))

  rv$batch_files <- inFiles

  allcols <- vector(mode="list")
  rv$columnCount <-vector(mode="list")
  # list columns for each file
  for (i in seq_along(inFiles$name)) {
    cols <- colnames(read.csv(inFiles$datapath[[i]],
                              fileEncoding = csvFileEncoding,
                              check.names = csvCheckNames,
                              nrows=1))
    allcols <- c(allcols, list(cols))


    #TODO: Add explanation if this works

    tempRow <- append(list(cols),1)
    #tempRow has two parts: the column scheme, and a count of that scheme. If the columnCount already contains such scheme, then the corresponding count + 1, if not create a new schme with count = 1
    if(tempRow[1] %in% rv$columnCount){
      #find the index of which the sheme is in
      index = which(rv$columnCount[] %in% tempRow[1]) + 1
      #add the corresponding count + 1
      rv$columnCount[[index]] = rv$columnCount[[index]] + 1
    }else{
      #if not contain, add such scheme to the list with a count value of 1
      rv$columnCount <- c(rv$columnCount,tempRow)
    }
  }
  #all even number indices are the counts, and we want to find max of them. The scheme they represent can be accessed by a simple offset
  countList <- rv$columnCount[c(FALSE, TRUE)]

  rv$upload_batch_columns <- allcols # get all col names as list

  #rv$upload_batch_colscheme <- allcols[[1]] # the standard now is the first file's columns. will change later
  rv$upload_batch_colscheme <- rv$columnCount[[which.max(countList) *2-1]]
  rv$upload_batch_sharedcols <- Reduce(intersect, allcols) # get all shared col names as vector
  # version1 remove invalid characters from the batch column names
  for(i in seq_along(rv$upload_batch_colscheme)){
    #delete the unrecognized character
    rv$upload_batch_colscheme[i] <- stringr::str_replace_all(rv$upload_batch_colscheme[i],"[^(a-z0-9A-Z%)|[:punct:]]", "")
  }
  for(i in seq_along(rv$upload_batch_columns[[1]])){
    #delete the unrecognized character
    rv$upload_batch_columns[[1]][i] <- stringr::str_replace_all(rv$upload_batch_columns[[1]][i],"[^(a-z0-9A-Z%)|[:punct:]]", "")
  }

  for(i2 in seq_along(rv$upload_batch_sharedcols)){
    #delete the unrecognized character
    rv$upload_batch_sharedcols[i2] <- stringr::str_replace_all(rv$upload_batch_sharedcols[i2],"[^(a-z0-9A-Z%)|[:punct:]]", "")
  }


  countList <- NULL
  rv$columnCount <- NULL

  rv$folder_upload_state <- 'uploaded'

  # show col selection modal
  showModal(modalDialog(
    title = "File upload",

    uiOutput("batch_feedback_1"),



    uiOutput("batch_opt"),

    uiOutput("batch_feedback_2"),
    uiOutput("batch_feedback_3"),

    easyClose = F, size="l",
    footer = uiOutput("batch_buttons")
  ))

})


# when user presses reset
observeEvent(input$g_reset, {
  rv$folder_upload_state <- 'reset'
  shinyjs::reset("fileIn")
  removeModal()
})



# upon submitting, add file to list of dataframes to select from
observeEvent(input$batch_submit, {

  # get files
  inFiles <- rv$batch_files

  # get rid of the dfs that dont fit criteria
  if(is.null(rv$batch_failed)==F){
    inFiles <- inFiles[ which( ! inFiles$name %in% rv$batch_failed) , ]
  }

  for (i in seq_along(inFiles$name)) {
    # print(i) # index of file
    # print(inFiles$name[[i]]) # name of file
    # print(inFiles$datapath[[i]]) # path of of file

    in_df <- read.csv(inFiles$datapath[[i]],
                      fileEncoding = csvFileEncoding,
                      check.names = csvCheckNames
                      )


    #Remove the invalid characters from the content
    show_reminder2 <- FALSE
    for(k in seq_along(colnames(in_df))){
      #detect and delete the unrecognized character Version1
      if(stringr::str_detect(colnames(in_df)[k], allowed_chars)){
        show_reminder2 <- TRUE
        colnames(in_df)[k]<- stringr::str_replace_all(colnames(in_df)[k],allowed_chars, "")
      }
      if(is.character(in_df[[k]])){
        if(any(stringr::str_detect(in_df[[k]], allowed_chars))){
          show_reminder2 <- TRUE
          in_df[[k]] <- stringr::str_replace_all(in_df[[k]],allowed_chars, "")
        }
      }
    }


    colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$batch_gene_column, "Name")
    colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$batch_Stat_column, "Stat")
    colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$batch_p_column, "PValue")
    colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$batch_q_column, "FDR")


    # tidy duplicate names
    if (any(duplicated(in_df$Name))){
      show_reminder_dup2 <- TRUE
    } else {show_reminder_dup2 <- F}

    # load only the essential columns (is it worth it to enable them to load more?)
    load_cols_list <- c(c("Name", "Stat", "PValue", "FDR"), unlist(input$batch_load_other_cols))

    # #rv$upload_columns <- colnames(read.csv(inFile$datapath, fileEncoding = "Latin1", check.names = F, nrows=1))
    # for(i in seq_along(load_cols_list)){
    #   #delete the unrecognized character
    #   load_cols_list[i] <- stringr::str_replace_all(load_cols_list[i],"[^(a-z0-9A-Z)|[:punct:]]", "")
    # }


    in_df <- in_df[,load_cols_list]
    in_df <- tidyInDF(in_df)

    newname <- tidy_filename(inFiles$name[[i]], rv$ll)

    # write in rv
    newname <- stringr::str_replace_all(newname,var_allowed_chars, "")
    rv$ll <- c(rv$ll, newname)
    rv$gg <- c(rv$gg, list(in_df))
    rv$tt <- c(rv$tt, input$batch_Stat_name)
    #Updated new value in rv: rv$FileDf
    rv$FileDF[nrow(rv$FileDF)+1,] <- c(inFiles$name[[i]], inFiles$size[[i]], inFiles$type[[i]],inFiles$datapath[[i]], newname)
  }
  rv$FileDF$size <- as.numeric(as.character(rv$FileDF$size))
  rv$folder_upload_state <- "reset"
  # shinyjs::reset("fileIn")
  removeModal()

  # # a modal that remind the user their file containin invalid characters Version 1
  # if(show_reminder2 == TRUE){
  #   showModal(modalDialog(
  #     inputId = "invalid_reminder_2 ",
  #     span("Unsupported characters are detected in your uploaded file(s) and will be removed for downstream analysis.", style = "font-size:200%"),
  #     easyClose = TRUE,size="l"
  #     , footer = modalButton("OK")
  #   ))
  # }
  # show_conditional_modal(show_reminder2, "invalid_reminder_2", "Unsupported characters are detected in your uploaded file(s) and will be removed for downstream analysis.")

  show_report_modal("batch_reminder2",
                    triggers = c(show_reminder2, show_reminder_dup2),
                    msgs = c(
                      "Invalid characters"="Unsupported characters are detected in your uploaded file(s) and will be removed for downstream analysis.",
                      "Duplicate names"="Your data contains duplicate names; these have been reformatted."
                      )
                    )


})













####---------------------- Single file upload --------------------------####



#  ============ help modal =============
observeEvent(input$upload_q,{
  showModal(modalDialog(
    inputId = "upload_q_md",
    title = "Upload data instructions",
    includeMarkdown(paste0(getwd(),"/inc/upload_q.md")),
    easyClose = TRUE,size="l",
    footer = modalButton("Dismiss")
  ))
})
observeEvent(input$upload_batch_q,{
  showModal(modalDialog(
    inputId = "upload_q_md",
    title = "Upload data instructions",
    includeMarkdown(paste0(getwd(),"/inc/upload_q.md")),
    easyClose = TRUE,size="l",
    footer = modalButton("Dismiss")
  ))
})



####================= single upload modal elements (show when file uploaded) =====================####

# select columns
output$upload_opt <- renderUI({
  req(rv$upload_state == 'uploaded')
  req(length(rv$upload_columns)>=4)

  # render default stat name
  statDefaultName <- firstmatch(stat_alias,rv$upload_columns)
  if (is.null(statDefaultName)){ statDefaultName <- "Value" }

  div(
    HTML("<strong>Select columns corresponding to each element:</strong><br><br>"),
    fluidRow(
      column(3,
             radioButtons(
               inputId = "gene_column",
               label = HTML(paste0(
                 "Name column:",
                 add_help("us_name_help", style="margin-left: 5px;"))
               ),
               choices = rv$upload_columns,
               selected = firstmatch(gene_alias,rv$upload_columns)
             ),
             bsTooltip("us_name_help",
                       "Gene, GeneName, Pathway, etc; an identifier.",
                       placement = "top"),
             radioButtons(
               inputId = "p_column",
               label = HTML(paste0(
                 "PValue column:",
                 add_help("us_p_help", style="margin-left: 5px;"))
               ),
               choices = rv$upload_columns,
               selected = firstmatch(p_alias,rv$upload_columns)
             ),
             bsTooltip("us_p_help",
                       "PValue, pval, etc; the p-value.",
                       placement = "top"),
      ),
      column(3,
             radioButtons(
               inputId = "Stat_column",
               label = HTML(paste0(
                 stat_replace("Stat column:"),
                 add_help("us_stat_help", style="margin-left: 5px;"))
               ),
               choices = rv$upload_columns,
               selected = firstmatch(stat_alias,rv$upload_columns)
             ),
             bsTooltip("us_stat_help",
                       "logFC, ES, etc; a main statistic that describes the magnitude of expression change.",
                       placement = "top"),
             radioButtons(
               inputId = "q_column",
               label = HTML(paste0(
                 "FDR column:",
                 add_help("us_q_help", style="margin-left: 5px;"))
               ),
               choices = rv$upload_columns,
               selected = firstmatch(q_alias,rv$upload_columns)
             ),
             bsTooltip("us_q_help",
                       "FDR, q-value, padj, etc; a corrected p-value.",
                       placement = "top"),


      ),
      column(6,
             textInput(
               inputId = "uploaded_file_name",
               label = "Name this dataset:",
               value= tidy_filename(input$file$name, rv$ll)),

             textInput(
               inputId = "Stat_name",
               label = HTML(paste0(
                 stat_replace("Name the Stat column:"),
                 add_help("us_namestat_help", style="margin-left: 5px;"))
               ),
               #value = itemmatched(stat_alias,rv$upload_columns))
               value = statDefaultName
             ),
             bsTooltip("us_namestat_help",
                       stat_replace("Tell us what kind of value is \"Stat\", i.e. logFC"),
                       placement = "top"),
             uiOutput("load_other_cols"),

      ),
    )
  )


})


# multiselect to load other columns
output$load_other_cols <- renderUI({
  req(rv$upload_state == 'uploaded')
  req(length(rv$upload_columns)>=4)

  other_cols <- setdiff(rv$upload_columns, c(input$gene_column, input$Stat_column,
                                             input$p_column,input$q_column))
  for(f in seq_along(other_cols)){
    #delete the unrecognized character
    other_cols[f] <- stringr::str_replace_all(other_cols[f],"[^(a-z0-9A-Z%)|[:punct:]]", "")
  }
  multiInput(inputId = "load_other_cols",
             label = "Load additional columns:",
             choices = other_cols,
             selected = firstmatch(le_alias, other_cols),
             width = "500px",
             options = list(
               enable_search = FALSE,
               non_selected_header = "Click to select:",
               selected_header = "Load these additional columns:")
  )
})

# upload / reset buttons
output$upload_buttons <- renderUI({


  fluidRow(
    div(style="display:inline-block;",
        uiOutput("confirm_button")
    ),
    div(style="display:inline-block;",
        actionButton('reset', 'Reset upload')
    )
  )
})

output$confirm_button <- renderUI({
  req(rv$upload_state == 'uploaded')
  req(length(rv$upload_columns)>=4)
  req(length(unique(c(input$gene_column, input$Stat_column, input$p_column, input$q_column)))==4)
  req(nchar(input$Stat_name)>0)
  actionButton('submit', 'Confirm and add' , class = "btn-primary")
})

# ======================= feedback boxes ==========================

# display uploaded file name
output$uploaded_file <- renderUI({
  if (is.null(rv$upload_state)){return (NULL)}
  else if (rv$upload_state == 'reset'){return (NULL)}
  else if (length(rv$upload_columns)<4){
    box(
      title = NULL, background = "yellow", solidHeader = TRUE, width=12,
      paste("WARNING: columns missing: ", input$file$name)
    )
  }
  else{
    box(
      title = NULL, background = "green", solidHeader = TRUE, width=12,
      paste("Uploaded file: ", input$file$name)
    )

  }
})

output$upload_feedback <- renderUI({
  req(rv$upload_state == 'uploaded')
  req(length(rv$upload_columns)>=4)

  cols <- c(input$gene_column, input$Stat_column, input$p_column, input$q_column)

  if (length(unique(cols))<4){
    box(
      title = NULL, background = "yellow", solidHeader = TRUE, width=12,
      "WARNING: Did you select a column twice?"
    )

  }
})


# ======================= events ==========================

# when file is uploaded, update state
observeEvent(input$file, {

  inFile <- input$file
  #Add a file size check and its modal, single upload is 10mb
  if(inFile$size >= single_upload_limit){
    #if(inFile$size >= 50){

    showModal(modalDialog(
      inputId = "size_reminder_modal3",
      # title = "The file size exceeds 50MB.",
      div(
        paste0("The file you uploaded exceeds ",single_mb_limit,"MB, please modify it to proceed. Try to delete unneeded columns and
            only keep the columns that you are interested in.
            Then press \"Browse...\" to upload it again. Thank you.")
        ,style="font-size:200%"),
      easyClose = TRUE,size="l"
      # , footer = modalButton("Close")
    ))
    shinyjs::reset("file")
  } else if (inFile$size + sum(rv$FileDF$size) > total_upload_limit){
    showModal(modalDialog(
      inputId = "size_reminder_modal4",
      # title = "The file size exceeds 50MB.",
      div(
        paste0("You have exceeded your storage limit of ",total_mb_limit,"MB. Please delete the unneeded files.
            Then press \"Browse...\" to upload again. Thank you.")
        ,style="font-size:200%"),
      easyClose = TRUE,size="l"
      # , footer = modalButton("Close")
    ))
    shinyjs::reset("file")
  }

  req((inFile$size < single_upload_limit) && (inFile$size + sum(rv$FileDF$size) <= total_upload_limit))

  # rv$upload_columns <- colnames(read.csv(inFile$datapath, nrows=1, encoding = 'UTF-8', stringsAsFactors=FALSE))

  # rv$upload_columns <- colnames(read.csv(inFile$datapath, nrows=1,local = locale(encoding = "latin1")))
  rv$upload_columns <- colnames(read.csv(inFile$datapath,
                                         fileEncoding = csvFileEncoding,
                                         check.names = csvCheckNames,
                                         nrows=1))

  for(i in seq_along(rv$upload_columns)){
    #delete the unrecognized character
    rv$upload_columns[i] <- stringr::str_replace_all(rv$upload_columns[i],"[^(a-z0-9A-Z%)|[:punct:]]", "")
  }

  rv$upload_state <- 'uploaded'

  # show col selection modal
  showModal(modalDialog(
    title = "File upload",
    uiOutput("uploaded_file"),
    uiOutput("upload_opt"),
    uiOutput("upload_feedback"),


    easyClose = F, size="l",
    footer = uiOutput("upload_buttons")
  ))
})
# when user presses reset, update state
observeEvent(input$reset, {
  rv$upload_state <- 'reset'
  shinyjs::reset("file")
  removeModal()
})

# upon submitting, add file to list of dataframes to select from
observeEvent(input$submit, {
  inFile <- input$file
  in_df <- read.csv(inFile$datapath,
                    fileEncoding = csvFileEncoding,
                    check.names = csvCheckNames
                    )

  # the for loop that loop through the file and remove invalid characters
  show_reminder <- FALSE
  for(i in seq_along(colnames(in_df))){
    #detect and delete the unrecognized character Version1
    if(stringr::str_detect(colnames(in_df)[i], allowed_chars)){
      show_reminder <- TRUE
      colnames(in_df)[i]<- stringr::str_replace_all(colnames(in_df)[i],allowed_chars, "")
    }
    if(is.character(in_df[[i]])){
      if(any(stringr::str_detect(in_df[[i]], allowed_chars))){
        show_reminder <- TRUE
        in_df[[i]] <- stringr::str_replace_all(in_df[[i]],allowed_chars, "")
      }
    }
  }

  # replace the important column names to prevent error later on
  colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$gene_column, "Name")
  colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$Stat_column, "Stat")
  colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$p_column, "PValue")
  colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$q_column, "FDR")
  load_cols_list <- c(c("Name", "Stat", "PValue", "FDR"),input$load_other_cols)

  # tidy duplicate names
  if (any(duplicated(in_df$Name))){
    show_reminder_dup1 <- TRUE
  } else {show_reminder_dup1 <- F}

  in_df <- in_df[,load_cols_list]
  in_df <- tidyInDF(in_df)

  newname <- input$uploaded_file_name

  # update rv
  newname <- stringr::str_replace_all(newname,var_allowed_chars, "")
  rv$gg <- c(rv$gg, list(in_df))
  rv$ll <- c(rv$ll, newname)
  rv$tt <- c(rv$tt, isolate(input$Stat_name))
  # update rv new value
  #rv$FileDF <- rbind(rv$FileDF,c(inFile$name, inFile$size, inFile$type,inFile$datapath , newname))
  rv$FileDF[nrow(rv$FileDF)+1,] <- c(inFile$name, inFile$size, inFile$type,inFile$datapath , newname)
  #This line is important becasue we need to have size column as numerics so that we can do calculation with total size later.
  rv$FileDF$size <- as.numeric(as.character(rv$FileDF$size))

  rv$upload_state <- "reset"

  # shinyjs::reset("file")
  removeModal()
  # if(show_reminder == TRUE){
  #   # a modal that reminds the user that their file contains invalid characters
  #   showModal(modalDialog(
  #     inputId = "invalid_reminder",
  #     span("IMPORTANT: Your file contains invalid characters. Please be aware of them. Thank you. ", style = "font-size:200%"),
  #     easyClose = TRUE,size="l"
  #     , footer = modalButton("OK")
  #   ))
  # }

  show_report_modal("single_reminder1",
                    triggers = c(show_reminder, show_reminder_dup1),
                    msgs = c(
                      "Invalid characters"="Unsupported characters are detected in your uploaded file(s) and will be removed for downstream analysis.",
                      "Duplicate names"="Your data contains duplicate names; these have been reformatted."
                    )
  )
})







####---------------------- delete file --------------------------####

#---------------------- events ---------------------------#

observeEvent(input$delete_deg_confirm, {

  to_delete_i <- which(rv$ll %in% input$delete_deg)

  # delete the items
  rv$ll <- rv$ll[-to_delete_i]
  rv$gg <- rv$gg[-to_delete_i]
  rv$tt <- rv$tt[-to_delete_i]

  #Added this one to update rv filedf when there is a delete, and after my test it works!
  rv$FileDF <-subset(rv$FileDF, !(rv$FileDF$tidiedName %in% input$delete_deg))
  #View(rv$FileDF)

})

#---------------------- Sidebar UI ---------------------------#

output$delete_deg <- renderUI({
  # #restoring RVs for the demo session
  if(!is.null(rv$demo) && rv$demo == "yes"){
    init_demo()
  }

  if(length(rv$ll) >= 1){
    div(
      tags$head(
        tags$style(".multi-wrapper {height: fit-content;}"),
        tags$style(".multi-wrapper .non-selected-wrapper, .multi-wrapper .selected-wrapper {height: 100%;}")
      ),
      div(
        multiInput(inputId = "delete_deg",
                   label = NULL,
                   choices = rv$ll,
                   width = "100%",
                   options = list(
                     enable_search = FALSE,
                     non_selected_header = "Loaded dataset(s):",
                     selected_header = "Delete dataset(s):")
        )
      )
    )
  } else {
    HTML("No data currently loaded.")
  }




})
output$delete_deg_confirm <- renderUI({
  req(length(input$delete_deg) >= 1)
  req(length(rv$ll) >= 1)
  actionButton("delete_deg_confirm", "Confirm and delete")
})


# ---------- when all is done, show guide box to next page ---------
output$guide_1a <- renderUI({
  if (length(rv$ll)>=2){ # 2 or more datasets loaded
    msg = "Navigate to <b>2. Select Filters</b> to proceed"
    guide_box("guide1", msg)
  } else {
    return(NULL)
  }
})
observeEvent(input$guide1,{
  updateTabItems(session, "tabs", "tab_filters")
})
