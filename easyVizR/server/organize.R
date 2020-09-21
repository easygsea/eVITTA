#======================================================================#
####                      ORGANIZE FILES                            ####
#======================================================================#
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





####================= multiple upload modal (show when file uploaded) =====================####

# show modal (experimental)
observeEvent(input$fileIn,{
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

# select columns
output$batch_opt <- renderUI({
  req(rv$folder_upload_state == 'uploaded')
  req(length(rv$upload_batch_colscheme)>=4)
  div(
    HTML("<strong>Select columns corresponding to each element:</strong><br><br>"),
    fluidRow(
      column(3,
             radioButtons(
               inputId = "batch_gene_column",
               label = "Gene column:",
               choices = rv$upload_batch_colscheme,
               selected = firstmatch(gene_alias,rv$upload_batch_colscheme)
             ),
             radioButtons(
               inputId = "batch_p_column",
               label = "P column:",
               choices = rv$upload_batch_colscheme,
               selected = firstmatch(p_alias,rv$upload_batch_colscheme)
             ),
      ),
      column(3,
             radioButtons(
               inputId = "batch_Stat_column",
               label = "Stat column:",
               choices = rv$upload_batch_colscheme,
               selected = firstmatch(stat_alias,rv$upload_batch_colscheme)
             ),
             radioButtons(
               inputId = "batch_q_column",
               label = "FDR column:",
               choices = rv$upload_batch_colscheme,
               selected = firstmatch(q_alias,rv$upload_batch_colscheme)
             )
      ),
      column(6,
             textInput(
               inputId = "batch_Stat_name",
               label = "Name the main statistic:",
               value = firstmatch(stat_alias,rv$upload_batch_colscheme),
             ),
             uiOutput("batch_additional_cols"),
             
      ),
    )
  )
  
  
})




# ======================= other modal elements ==========================

# if there are other shared cols, also enable them to load those
output$batch_additional_cols <- renderUI({
  req(rv$folder_upload_state == 'uploaded')
  req(is.null(input$batch_gene_column)==F)
  req(is.null(input$batch_Stat_column)==F)
  req(is.null(input$batch_p_column)==F)
  req(is.null(input$batch_q_column)==F)
  req(length(rv$upload_batch_colscheme)>=4)
  
  j <- c(input$batch_gene_column, input$batch_Stat_column, input$batch_p_column, input$batch_q_column)
  # print(j)
  # print(rv$upload_batch_sharedcols)
  additional_cols <- setdiff(rv$upload_batch_sharedcols,j)
  
  
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
  
  # print(rv$folder_upload_state)
  # print(rv$upload_batch_colscheme)
  # print(length(unique(c(input$batch_gene_column, input$batch_Stat_column, input$batch_p_column, input$batch_q_column))))
  # print(rv$batch_failed)
  # print(rv$batch_files)

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
  j <- c(input$batch_gene_column, input$batch_Stat_column, input$batch_p_column, input$batch_q_column)
  for (i in seq_along(rv$upload_batch_columns)) {
    if (all(j %in% rv$upload_batch_columns[[i]])){
      next
    }
    else{
      failed <- c(failed, rv$batch_files$name[[i]])
    }
  }
  if (is.null(failed)==F){
    rv$batch_failed <- failed
    box(
      title = NULL, background = "yellow", solidHeader = TRUE, width=12,
      paste0("Omitted because columns do not match: ",paste(failed, collapse=", "))
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
  rv$batch_files <- inFiles
  
  allcols <- vector(mode="list")
  # list columns for each file
  for (i in seq_along(inFiles$name)) {
    cols <- colnames(read.csv(inFiles$datapath[[i]],nrows=1))
    allcols <- c(allcols, list(cols))
  }
  
  rv$upload_batch_columns <- allcols # get all col names as list
  rv$upload_batch_colscheme <- allcols[[1]] # the standard now is the first file's columns. will change later
  rv$upload_batch_sharedcols <- Reduce(intersect, allcols) # get all shared col names as vector
  
  rv$folder_upload_state <- 'uploaded'
  
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
    
    in_df <- read.csv(inFiles$datapath[[i]])
    colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$batch_gene_column, "Name")
    colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$batch_Stat_column, "Stat")
    colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$batch_p_column, "PValue")
    colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$batch_q_column, "FDR")
    
    # load only the essential columns (is it worth it to enable them to load more?)
    load_cols_list <- c(c("Name", "Stat", "PValue", "FDR"), unlist(input$batch_load_other_cols))
    # print(load_cols_list)
    in_df <- in_df[,load_cols_list]
    in_df$Name <- toupper(in_df$Name)
    
    newname <- tidy_filename(inFiles$name[[i]], rv$ll)
    
    
    # write in rv
    rv$ll <- c(rv$ll, newname)
    rv$gg <- c(rv$gg, list(in_df))
    rv$tt <- c(rv$tt, input$batch_Stat_name)
  }
  
  rv$folder_upload_state <- "reset"
  # shinyjs::reset("fileIn")
  removeModal()
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



####================= single upload modal (show when file uploaded) =====================####

# show modal (experimental)
observeEvent(input$file,{
  showModal(modalDialog(
    title = "File upload",
    uiOutput("uploaded_file"),
    uiOutput("upload_opt"),
    uiOutput("upload_feedback"),
    
    
    easyClose = F, size="l",
    footer = uiOutput("upload_buttons")
  ))

})  

# select columns
output$upload_opt <- renderUI({
  req(rv$upload_state == 'uploaded')
  req(length(rv$upload_columns)>=4)
  div(
    HTML("<strong>Select columns corresponding to each element:</strong><br><br>"),
    fluidRow(
      column(3,
             radioButtons(
               inputId = "gene_column",
               label = "Gene column:",
               choices = rv$upload_columns,
               selected = firstmatch(gene_alias,rv$upload_columns) 
             ),
             radioButtons(
               inputId = "p_column",
               label = "P column:",
               choices = rv$upload_columns,
               selected = firstmatch(p_alias,rv$upload_columns)
             ),
      ),
      column(3,
             radioButtons(
               inputId = "Stat_column",
               label = "Stat column:",
               choices = rv$upload_columns,
               selected = firstmatch(stat_alias,rv$upload_columns)
             ),
             radioButtons(
               inputId = "q_column",
               label = "FDR column:",
               choices = rv$upload_columns,
               selected = firstmatch(q_alias,rv$upload_columns)
             )
             
        
      ),
      column(6,
             textInput(
               inputId = "uploaded_file_name",
               label = "Name this dataset:",
               value= tidy_filename(input$file$name, rv$ll)),
             
             textInput(
               inputId = "Stat_name",
               label = "Name the main statistic:",
               #value = itemmatched(stat_alias,rv$upload_columns))
               value = firstmatch(stat_alias,rv$upload_columns)
               ),
             uiOutput("load_other_cols"),
      
      ),
    )
  )
  
  
})


# ======================= other modal elements ==========================

# multiselect to load other columns
output$load_other_cols <- renderUI({
  req(rv$upload_state == 'uploaded')
  req(length(rv$upload_columns)>=4)
  
  other_cols <- setdiff(rv$upload_columns, c(input$gene_column, input$Stat_column,
                                             input$p_column,input$q_column))
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
  # print(cols)
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
  rv$upload_columns <- colnames(read.csv(inFile$datapath, nrows=1))
  rv$upload_state <- 'uploaded'
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
  in_df <- read.csv(inFile$datapath)
  
  # replace the important column names to prevent error later on
  colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$gene_column, "Name")
  colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$Stat_column, "Stat")
  colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$p_column, "PValue")
  colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$q_column, "FDR")
  load_cols_list <- c(c("Name", "Stat", "PValue", "FDR"),input$load_other_cols)
  #print(load_cols_list)
  
  
  in_df <- in_df[,load_cols_list]
  in_df <- remove_nas(in_df)
  
  
  # set data type
  in_df$Name <- as.character(in_df$Name) # convert name column to character
  in_df$Name <- toupper(in_df$Name)
  # in_df[load_cols_list[-1]] <- sapply(in_df[load_cols_list[-1]],as.numeric) # convert all but name to numeric
  
  newname <- input$uploaded_file_name
  
  # update rv
  rv$gg <- c(rv$gg, list(in_df))
  rv$ll <- c(rv$ll, newname)
  rv$tt <- c(rv$tt, isolate(input$Stat_name))
  
  rv$upload_state <- "reset"
  
  # shinyjs::reset("file")
  removeModal()
})







####---------------------- delete file --------------------------####

#---------------------- events ---------------------------#

observeEvent(input$delete_deg_confirm, {
  #print(input$delete_deg)
  to_delete_i <- which(rv$ll %in% input$delete_deg)
  #print(to_delete_i)
  # delete the items
  rv$ll <- rv$ll[-to_delete_i]
  rv$gg <- rv$gg[-to_delete_i]
  rv$tt <- rv$tt[-to_delete_i]
})

#---------------------- Sidebar UI ---------------------------#

output$delete_deg <- renderUI({
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

