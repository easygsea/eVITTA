# # DEMO SESSION CODE -------------------------------------------------------
# library(later)
# # the modal to remind the user it is a demo session
# observe({
#   init_demo()
#   showModal(modalDialog(title = tags$h3("Welcome to our easyGEO demo session"),
#                         tags$h4("Explore the sample output that performs interactively in the same way as real output.")
#                         ,br()
#                         ,tags$h4("Click OK to follow the intro tour."),
#                         size = "m",
#                         easyClose = FALSE
#                         ,footer = actionButton("welcome_modal",label = "OK")))
# 
# })
# # when the user closed the modal, start rintrojs
# observeEvent(input$welcome_modal, {
#   removeModal()
#   rv$demo_yes <- "yes"
#   call_introjs(rbind(intros$E_pre,intros$E_post,intros$E_post_with_summary_ui))
#   print(input$menu1)
# 
# })
# 
# # start rintrojs when users switch tabs
# observeEvent(input$menu1,{
#   if(input$menu1 == "tab3"){
#     later::later(~call_introjs(intros$D_post), delay = 2)
#   } else if(input$menu1 == "tab2"){
#     later(~call_introjs(intros$F_post),2)
#   } else if(input$menu1 == "tab4"){
#     later(~call_introjs(rbind(intros$R_post,intros$R_post_deg)),2)
#   } else if(input$menu1 == "tab5"){
#     later(~call_introjs(intros$V_volcano), 0.1)
#   } else {
# 
#   }
# })
# # when user select different plots, triggering different introjs
# observeEvent(input$visDEG, {
#   if(input$visDEG == "heatmap"){
#     later(~call_introjs(intros$V_heatmap), 0.1)
#   } else if(input$visDEG == "gene"){
#     later(~call_introjs(intros$V_explore_violin), 0.1)
#   } else {
# 
#   }
# })
# # when user click the boxplot next to the violin plot, trigger an introjs
# observeEvent(input$a_type, {
#   if(input$a_type == "box"){
#     call_introjs(intros$V_explore)
#   }
# })
# # END-----------------------------------------------------------------------------


# get the current run_mode ------------------------------------------------
observeEvent(input$selected_mode, {
  # req(input$menu1 == "tab1")
  rv$run_mode = input$selected_mode
})
# UI for the retrieval by GSE mode
output$ui_tab1 <- renderUI({
  if(input$selected_mode == "auto"){
  fluidRow(
    column(4,
           
           box(title=span(HTML("<b>1.1.</b>"),icon("search"), "Input GEO accession"), width = 12, solidHeader=F, status = "primary",
               uiOutput("geo_accession_ui"),
               
           ),
           
           box(title=span(HTML("<b>1.2.</b>"),icon("hdd"),"Select Platform"), width = 12, solidHeader=F, status = "primary",
               uiOutput("geo_platform_ui")
           ),
           
           column(12,align="center",
                  uiOutput("guide_1a")
                  
           )
           
           
    ),
    
    column(8,
           
           tabBox(
             title = NULL, width = 12,
             id = "summary", height = "250px",
             tabPanel("Summary",
                      
                      uiOutput("gse_summary_ui")
                      
             ),
             tabPanel("Study info",
                      
                      DT::dataTableOutput("gse_meta_df")
             ),
             tabPanel("Experiment info",
                      
                      DT::dataTableOutput("gsm_meta_df")
             )
           )
           
           
           
    )
    
    
    
    
  )}else{ uiOutput("ui_manual")}                  
  
})
# UI for the manually uploading data mode
output$ui_manual <- renderUI({
  # req(input$selected_mode == "manual")
  fluidRow(
    column(6,
             box(title=span(HTML("<b>1.1.</b>"),icon("upload"),"Upload Data Matrix"), width = 12, solidHeader=F, status = "primary", 
                 id = "manual_data_matrix",
                 
                 div(
                   fileInput("data_matrix_file",
                             label = p("Upload tidied data matrix (CSV/TSV format):",
                                       tags$style(type = "text/css", "#manual_help {display: inline-block;width: 20px;height: 20px;padding: 0;border-radius: 50%;vertical-align: baseline;}"),
                                       bsButton("manual_help", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                             accept = c(
                               "text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv",".tsv",".txt",".tab")
                   ),
                    downloadLink("data_matrix_sample", "Download Sample Data Matrix", style = "position:relative; bottom: 20px")
                   ,
                   bsTooltip("manual_help", "Click to learn more", placement = "top")
                 )
               )
           ),
    column(6,
           box(title=span(HTML("<b>1.2.</b>"),icon("upload"),"Upload Design Matrix"), width = 12, solidHeader=F, status = "primary", 
               id = "manual_design_matrix",
               
               div(
                 fileInput("design_matrix_file",
                           label = p("Upload tidied design matrix (CSV/TSV format):",
                                     tags$style(type = "text/css", "#manual_help_2 {display: inline-block;width: 20px;height: 20px;padding: 0;border-radius: 50%;vertical-align: baseline;}"),
                                     bsButton("manual_help_2", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv",".tsv",".txt",".tab")
                 ), 
                 downloadLink("design_matrix_sample", "Download Sample Design Matrix", style = "position:relative; bottom: 20px")
                 ,
                 bsTooltip("manual_help_2", "Click to learn more", placement = "top")
               )
           ),
           column(12,align="center",
                  uiOutput("guide_1a_manual")
                  
           ))
  )
})
output$data_matrix_example = DT::renderDataTable(
  df <- read_tsv(paste0(getwd(), "/server/data_matrix.tsv")) %>%
    remove_rownames() %>%
    column_to_rownames("X1")
)

output$design_matrix_example = DT::renderDataTable(
  df <- read_csv(paste0(getwd(), "/server/design_matrix.csv")) %>%
    remove_rownames() %>%
    column_to_rownames("X1")
)

# the help page for data matrix
observeEvent(input$manual_help,{
  showModal(modalDialog(
    inputId = "file_help_1",
    title = h1("Note on uploading data matrix"),
    h3('1) the data should be in comma- or tab-delimited format (csv, tsv, tab, txt)'),
    br(),
    h3('2) the first row of the matrix should be sample names; must match the sample names in the design matrix'),
    br(),
    h3("3) the first column of the matrix should be gene names; no duplicates are allowed"),
    br(),
    h3("For example,"),
    br(),
    DT::dataTableOutput("data_matrix_example"),
    #includeMarkdown(rmarkdown::render(paste0(getwd(),"/server/data_matrix_page.Rmd"))),
    #includeHTML(paste0(getwd(),"/server/data_matrix_page.html")),
    easyClose = TRUE,size="l",
    footer = modalButton("OK")
  ))
})
# the help page for design matrix
observeEvent(input$manual_help_2,{
  showModal(modalDialog(
    inputId = "file_help_2",
    title = h1("Note on uploading design matrix"),
    h3("1) the data should be in comma- or tab-delimited format (csv, tsv, tab, txt)"),
    br(),
    h3("2) the first row of the matrix should be sample attributes (e.g. strain names, experimental conditions, patient groups); no duplicates are allowed,"),
    br(),
    h3("3) the first column of the matrix should be sample names; must match the sample names in the data matrix,"),
    br(),
    h3("For example,"),
    br(),
    DT::dataTableOutput("design_matrix_example"),
    # HTML(markdown::markdownToHTML(knit('server/design_matrix_page.Rmd', quiet = TRUE))),
    # includeHTML(paste0(getwd(),"/server/design_matrix_page.html")),
    easyClose = TRUE,size="l",
    footer = modalButton("OK")
  ))
})

# when the data matrix is uploaded
observeEvent(input$data_matrix_file, {
  # generate data matrix
  inFile <- input$data_matrix_file
  read_data_matrix(inFile = inFile)
 
})

# when the design matrix is uploaded, rv$fddf changed from NULL to a data frame
observeEvent(input$design_matrix_file, {
  # read the design matrix into rv$fddf
  inFile <- input$design_matrix_file
  read_design_matrix(inFile)
})

# the buttons of the modal after data matrix uploaded
output$matrix_buttons <- renderUI({
  fluidRow(
    div(style="display:inline-block;",
        uiOutput("dm_confirm_button")
    ),
    div(style="display:inline-block;",
        actionButton('dm_reset', 'Reset upload')
    )
  )   
})
# the buttons of the modal after design matrix uploaded
output$design_matrix_buttons <- renderUI({
  fluidRow(
    div(style="display:inline-block;",
        bsButton("design_matrix_confirm", "Confirm and upload", style = "primary")
    ),
    div(style="display:inline-block;",
        actionButton('design_matrix_reset', 'Reset upload')
    )
  )   
})

output$dm_confirm_button <- renderUI({
  bsButton("dm_confirm", "Confirm and upload", style = "primary")
})

# when user presses reset
observeEvent(input$dm_reset, {
  # rv$folder_upload_state <- 'reset'
  shinyjs::reset("data_matrix_file")
  # reset the existing sample names
  rv$dmdf_samples <- NULL
  removeModal()
})
observeEvent(input$design_matrix_reset, {
  shinyjs::reset("design_matrix_file")
  # reset the existing sample names
  rv$fddf_samples <- NULL
  rv$fddf <- NULL
  removeModal()
})
# when user clicks confirm and upload button
observeEvent(input$dm_confirm, {
  #initialize rv$dmdf
  rv$dmdf <- rv$indf
  rv$samples <- rv$dmdf_samples
  removeModal()
})
observeEvent(input$design_matrix_confirm, {

  #initialize rv$fddf
  rv$fddf_o <- rv$fddf
  removeModal()
})

# the function that read the design matrix
read_design_matrix <- function(inFile){
  
  # the modal that appears when the file user upload exceeds 10MB, Version1
  if(inFile$size >= 10*1024^2){
    showModal(modalDialog(
      inputId = "size_reminder_modal",
      # title = "The file size exceeds 100MB.",
      div("The file you uploaded exceeds 10MB, please modify it to proceed. Try to delete unneeded columns and 
            only keep the columns that you are interested in. 
            Then press \"Browse...\" to upload it again. Thank you.",style="font-size:200%"),
      easyClose = TRUE,size="l"
      , footer = modalButton("OK")
    ))
  }
  # check if the file (name) extensions are correct; if not, reset the file upload
  if(!tools::file_ext(inFile$name) %in% c(
    "text/csv",
    "text/comma-separated-values,text/plain",
    "csv","tsv","txt","tab")){
    shinyjs::reset("design_matrix_file")
    shinyalert("We only accept files that are .csv,.tsv,.txt,.tab; 
                   please check your file and upload the file with the correct file (name) extensions .")
  }
  req(tools::file_ext(inFile$name) %in% c(
    "text/csv",
    "text/comma-separated-values,text/plain",
    "csv","tsv","txt","tab"))
  
  #added try() here because there could be an error reading the file
  indf <- try(read.csv(inFile$datapath, header=T, 
                       colClasses=c("character")))
  
  # read in TAB delimited
  if(is.null(ncol(indf)) || ncol(indf)==1){
    indf <- try(read.table(inFile$datapath, sep="\t",header=T, 
                           colClasses=c("character")))
  }
  print(head(indf))
  # read in space delimited
  if(is.null(ncol(indf)) || ncol(indf)==1){
    indf <- try(read.table(inFile$datapath, sep=" ",header=T, 
                           colClasses=c("character")))
  }
  
  if(inherits(indf, "try-error")) { 
    ErrorMessage <- conditionMessage(attr(indf, "condition"))  # the error message
    #show a modal dialog if there is an error reading files causing crash
    showModal(modalDialog( 
      title = "Your input file has a wrong format",
      HTML(paste0("Having trouble loading your file:<br>",
                  ErrorMessage,"<br>",
                  "Please revise your input file according to our notes and reupload the file.")),
      size = "l",
      easyClose = TRUE
      ,footer = modalButton("OK")
    ))
  }
  
  
  req(!inherits(indf, "try-error")) #require to be no error to proceed the following codes
  
  # check the number of columns of the matrix
  if(ncol(indf) < 2){
    shinyalert("You uploaded a file with < 2 columns, that is not an accepted format. Please click the help button for accepted file formats.")
    shinyjs::reset("design_matrix_file")
  }
  
  req(ncol(indf) >= 2)
  
  # This part removes duplicate rows of indf
  DuplicateCheck <- as.data.frame(indf[,1], drop=FALSE) #extract first column of indf and check if there is 
  DuplicateCheck <- duplicated(DuplicateCheck)
  indf<-indf[!DuplicateCheck, ] #remove duplicate rows
  DuplicateRows <- which(DuplicateCheck == TRUE, arr.ind=TRUE)
  if(length(DuplicateRows) > 0){ # if there are duplicate rows
    showModal(modalDialog( 
      title = "Warning: The file you uploaded contains duplicate gene names",
      HTML(paste0("Only the first duplicate(s) will be kept.<br>",
                  "If this is not what you intend, please double check your input file and re-upload.")),
      size = "l",
      easyClose = TRUE
      ,footer = modalButton("OK")
    ))
  }
  ###
  
  indf_coln <- colnames(indf)
  
  #print(indf_coln)
  #print(validUTF8(indf_coln))
  #print(prod(validUTF8(indf_coln)))
  #adding a IF statement to ensure that the characters in the column names are encoded correctly,
  #which means there is no invalid characters inside the column names
  if(prod(validUTF8(indf_coln))){
    whether_contains_invalid <- FALSE
  }
  else{
    #delete the column names that contain invalid characters
    #indf_coln <- indf_coln[validUTF8(indf_coln)]
    whether_contains_invalid <- TRUE
    for(i in seq_along(indf_coln)){
      #delete the unrecognized character
      indf_coln[i] <- stringr::str_replace_all(indf_coln[i],"[^(a-z0-9A-Z)|[:punct:]]", "")
      # print(indf_coln[i])
    }
  }
  rv$fddf_samples <- indf[[1]]
  rownames(indf) <- indf[[1]]
  indf[ ,1] <- NULL
  rv$fddf <- indf
  
  row_text <- ifelse(length(rownames(indf))>5, rownames(indf)[1:5], rownames(indf))
  col_text <- ifelse(length(colnames(indf))>5, colnames(indf)[1:5], colnames(indf))
  # display a modal that briefly describes the design matrix
  showModal(modalDialog(
    title = div("Design Matrix Upload",style = "font-size:170%"),
    span(HTML("The uploaded file contains these <b>Samples:</b> "),
         if(length(rownames(indf))>5){
          paste0(glue_collapse(rownames(indf)[1:5], sep = ", ", last = " and "), "...")
          } else {
            glue_collapse(rownames(indf), sep = ", ", last = " and ") 
         }
         , "(",
         length(rownames(indf)), HTML("in total), and  these <b>attributes:</b>"),
         if(length(colnames(indf)) > 5){
           paste(glue_collapse(colnames(indf)[1:5], sep = ", ", last = " and "),"...")
         } else {
            glue_collapse(colnames(indf), sep = ", ", last = " and ")
         }
         , "(",
         length(colnames(indf)), "in total).",br(), uiOutput("design_matrix_sample_comparison"),
         uiOutput("design_matrix_warning"),ul_txt,
         style = "font-size:130%"),
    easyClose = F,
    size = "l",
    footer = uiOutput("design_matrix_buttons")
  ))
  
}
# the warning that appears when the uploaded design matrix contains pure number variable
output$design_matrix_warning <- renderUI({
  df <- rv$fddf
  number_of_numeric = 0

  for(i in seq_along(df)){
    if(all(!grepl("\\D", df[[i]]))){
      number_of_numeric = number_of_numeric + 1
      break
    }
  }
  if(number_of_numeric > 0){
    HTML("<br><p style = 'color:red;'><b>The matrix you have upload contains pure numeric values. It's probably a data matrix.
          Please double check and reupload if necessary.</b></p>")
  }
})
# the warning that appears when the uploaded data matrix contains
# variable that is not 0 or 'NA'; '\\D' in regular expression means non-digit.
output$data_matrix_warning <- renderUI({
  df <- rv$indf[ , -1]
  number_of_nonnumeric = 0
  withProgress(message = 'Processing data matrix', value = 1,{
      for(i in seq_along(df)){
        if(any(grepl("\\D", df[[i]])) && !anyNA(df[[i]])){
          number_of_nonnumeric = number_of_nonnumeric + 1
          break
        }
      # for(j in seq_along(df[[i]])){
      #   if(grepl("\\D", df[[i]][j]) && df[[i]][j] != 'NA'){
      #   }
      # }
    }
  })
  
  if(number_of_nonnumeric > 0){
    HTML("<br><p style='color:red'>The matrix you have uploaded contains non-numeric values. It's probably a design matrix. 
          Please double check and reupload if necessary.</p>")
  }else{
    
  }
})

output$sample_comparison <- renderUI({
  # req(!is.null(rv$fddf_samples))
  number_of_matches <- 0
  overlapped_vector <- intersect(rv$fddf_samples, rv$dmdf_samples)
  different_vector <- setdiff(rv$fddf_samples, rv$dmdf_samples)
  
  number_of_matches = length(overlapped_vector)
  print(number_of_matches)
  
  if(length(different_vector) > 0) {
    if(length(different_vector) > 5){
      txt_a <- different_vector[1:5]
    } else {
      txt_a <- different_vector
    }
    
    txt <- paste0(". Specifically, ", glue_collapse(txt_a, sep = ", ", last = " and "),
                 " (", length(different_vector), " in total) are found in the design matrix (uploaded in the right panel), but missing in the uploaded data matrix.")
  } else{
    txt <- "."
  }
  
  if(!is.null(rv$fddf_samples) && !is.null(rv$dmdf_samples)){
    HTML(paste("<br><p style = 'color:green;'>", number_of_matches, 
               "samples are found in both data matrix and design matrix",
               txt, "</p>"))
  } #else if(is.null(rv$fddf_samples)) {
  #   HTML(paste("<br>In addition, you could click <b>Reset upload</b> and 
  #              upload your design matrix on the right panel first, to view the overlapped samples of these two matrix."))
  #   } else {
  #   
  # }
})
# when data matrix is uploaded first, compare the design matrix with the data matrix
output$design_matrix_sample_comparison <- renderUI({
  # req(!is.null(rv$fddf_samples))
  number_of_matches <- 0
  overlapped_vector <- intersect(rv$dmdf_samples, rv$fddf_samples)
  different_vector <- setdiff(rv$dmdf_samples, rv$fddf_samples)
  
  if(length(different_vector) > 0) {
    if(length(different_vector) > 5){
      txt_a <- different_vector[1:5]
    } else {
      txt_a <- different_vector
    }
    txt <- paste0(". Specifically, ", glue_collapse(txt_a, sep = ", ", last = " and "),
                  "... (", length(different_vector), " in total) are detected in the data matrix (uploaded on the left), but missing in the uploaded design matrix.")
    
  } else{
    txt <- "."
  }
  
  number_of_matches = length(overlapped_vector)
  if(!is.null(rv$fddf_samples) && !is.null(rv$dmdf_samples)){
    HTML(paste("<br><p style = 'color:green;'>", number_of_matches, 
               "samples are found in both data matrix and design matrix",
               txt, "</p>"))
  }
})

# the function for the link to download the data matrix and design matrix
output$data_matrix_sample <- downloadHandler(
  filename = "data_matrix_sample.tsv",
  content = function(file){
    file.copy("data_matrix_sample.tsv", file)
  }
)
output$design_matrix_sample <- downloadHandler(
  filename = "design_matrix_sample.csv",
  content = function(file){
    file.copy("design_matrix_sample.csv", file)
  }
)



# --------------- search GEO accession ---------------
# currently checks if input exists
# todo: check if input format is correct (GSEXXXXXX)

output$geo_accession_ui <- renderUI({
  if(rv$demo == "yes"){
    init_demo()
  }
  
  div(
    textInput(
      inputId = "geo_accession",
      label = p("GSE Accession Number:"
                   ,tags$style(type = "text/css", "#q1 {display: inline-block;width: 16px;height: 16px;padding: 0 0 0 1px;border-radius: 50%;vertical-align: baseline;font-size:10px;}"),
                   bsButton("q1", label = "", icon = icon("question"), style = "info", size = "extra-small")),
      value = rv$demo_acc,
      placeholder = "GSE147507",
      width = "100%"
    ),
    bsTooltip("q1", HTML("Begins with GSE. <i>Click</i> to load an <u>example GEO accession number</u>")
              , placement = "top")

    ,"Note: only RNA-seq and single_channel microarray datasets are currently accepted.", br(), br(),

    uiOutput("search_geo")

  )
})

# update text box when q1 actionbutton clicked
observeEvent(input$q1,{
  updateTextInput(session,
    inputId = "geo_accession",
    value = "GSE147507"
  )
})

output$search_geo <- renderUI({
  req(nchar(input$geo_accession)>0)
  # if (nchar(input$geo_accession)>0){
    actionButton("search_geo", "Search")
  # } else {
  #   "Enter a valid GEO accession number."
  # }

})

# --------------- get GEO matrix ---------------
# this takes several mins for larger datasets; need more feedback?

observeEvent(input$search_geo, {

  rv$gse_all <- NULL
  rv$geo_accession <- NULL
  rv$platforms <- NULL
  rv$plat_id <- NULL
  rv$deg <- NULL
  rv$identifiers_df <- NULL
  rv$identifiers <- NULL
  
  rv$getgeo_mode <- T; rv$gpl_type = list(); rv$gpl_count = list(); rv$gsmlist = list()
  rv$organism <- NULL

  withProgress(message = 'Getting data. Please wait a minute...', value = 1, {

    rv$geo_accession <- isolate(input$geo_accession) %>% toupper(.)
    
    if(startsWith(rv$geo_accession,"GSE")){gse_y <- "yes"}else{gse_y <- "no"}
    
    if(gse_y == "no"){
      showModal(modalDialog(
        title = "Wrong input format",
        HTML("Please enter a valid NCBI GEO accession number that <b>begins with GSE</b>."),
        size = "l",
        easyClose = TRUE
        ,footer = modalButton("OK")
      ))
    }
    
    req(gse_y == "yes")
    
    #It appears that this is where load happens and we need to test if we things are unexpected when loading
    rv$gse_all <- try(getGEO(input$geo_accession, GSEMatrix=T))

    if(inherits(rv$gse_all, "try-error")) {
      ErrorMessage <- conditionMessage(attr(rv$gse_all, "condition"))  # the error message
      print(ErrorMessage)
      #Depending on what we entered, different types of errors could occur:

      if(ErrorMessage == "object 'destfile' not found"){
          DisplayText <- paste0("Your input: \"",rv$geo_accession,"\"<br><br>",
                                "Unable to process ",rv$geo_accession," from NCBI server due to internet connection issues."
                                ,"<br><br>Please re-click <b>Search</b> to retrieve again."
                                ,"<br><br>If you keep seeing this error message, please email eVITTA team at evitta@cmmt.ubc.ca"
                                ,"<br><br>Thank you for your support."
                                ,"<br><br>"
                                ,ErrorMessage)
          
          showModal(modalDialog(
            title = "Data fetching error",
            HTML(DisplayText),
            size = "l",
            easyClose = TRUE
            ,footer = modalButton("OK")
          ))
        }
      else if(ErrorMessage == "HTTP error 404."){
        DisplayText <- paste0("Your input: \"",rv$geo_accession,"\"<br><br>",
                              "Unable to retrieve ",rv$geo_accession," from NCBI server.<br><br>Please enter a valid and published GSE accession number and try again.")
        showModal(modalDialog(
          title = "Data fetching error",
          HTML(DisplayText),
          size = "l",
          easyClose = TRUE
          ,footer = modalButton("OK")
        ))
      }
      else if(ErrorMessage == "HTTP error 400."){
        DisplayText <- paste0("Your input: \"",rv$geo_accession,"\"<br><br>",
                              "You probably entered some upexpected character(s) such as a space.<br><br>Please double check your input and try again.")
        showModal(modalDialog(
          title = "Data fetching error",
          HTML(DisplayText),
          size = "l",
          easyClose = TRUE
          ,footer = modalButton("OK")
        ))
      }
      else {
        # switch to matrix == F
        rv$gse_all <- try(getGEO(input$geo_accession, GSEMatrix=F))
        
        if(inherits(rv$gse_all, "try-error")) {
          alink <- paste0("https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=",rv$geo_accession)
          # DisplayText <- paste0("Failed to get data from server. Please double check your query and try again", "<br>", ErrorMessage)
          DisplayText <- paste0("Unable to parse ",rv$geo_accession," data. Error message:<br>"
                                , ErrorMessage
                                , "<br><br>GEO sequence submission procedures are designed to encourage provision of <a target='_blank' href='http://fged.org/projects/minseqe/'>MINSEQE</a> elements."
                                , " This dataset was likely not meeting the standards."
                                , "<br><br>Please download the study's gene expression data matrix and design matrix from <a target='_blank' href='",alink,"'><b>",alink,"</b></a>, process them into desirable formats ("
                                ,"<a href='https://tau.cmmt.ubc.ca/bu/data_matrix.png' target='_blank'>data matrix</a> and <a href='https://tau.cmmt.ubc.ca/bu/design_matrix.png' target='_blank'>design matrix</a>"
                                ,"), and use <b>Manual uploads</b> mode to continue."
                                )
          showModal(modalDialog(
            title = "Data parsing error",
            HTML(DisplayText),
            size = "l",
            easyClose = TRUE
            ,footer = confirm_and_reset_buttons("parsing_error_confirm","parsing_error_reset")
          ))
        }else{
          rv$getgeo_mode <- F
        }
      }
    }

    req(!inherits(rv$gse_all, "try-error"))
    
    if(rv$getgeo_mode){
      rv$platforms <- tabulate(rv$gse_all, annotation)
      rv$gpl_summary <- summarize_gpl(rv$gse_all)
    }else{
      rv$platforms <- names(GPLList(rv$gse_all))
      rv$gpl_summary <- summarize_gpl_F(rv$gse_all)
    }

    # initialize gpl selection choices
    choices <- lapply(rv$gpl_summary, function(x){
      HTML(paste0(x[["ID"]],": ", x[["Organism"]], " (", x[["Samples"]]," samples)" ))
    })
    
    rv$gpl_choices <- invert_vector(choices)
  })

})

# --------------- select the desired platform ----------------
# Sometimes multiple platforms are in the GSE, thus the GSE object is a list with length >1
# we need to make people choose one

# select GEO platform, with tooltips that summarize the GPL
output$geo_platform_ui <- renderUI({
  req(is.null(rv$gse_all)==F)
  req(length(rv$platforms)>0)
  req(length(rv$gpl_summary)>0)
  req(length(rv$gpl_choices)>0)

  div(id="select_plat",
    radioButtons(
      inputId = "plat",
      label = HTML("Platforms available:",add_help("q2")),
      # choices = rv$platforms
      choices = rv$gpl_choices
    ),
    bsTooltip("q2",HTML("GEO platforms represent a diverse range of technologies, molecule types, and annotation conventions.<br><br>Select the platform you are interested in and <i>click</i> <b>Select to proceed</b>")
              ,placement = "top"),
    uiOutput("gpl_tooltips"),
    uiOutput("study_type_feedback"),
    uiOutput("select_geo_platform")

  )

})

# generate tooltips for gpl selections
observe({
  req(length(rv$gpl_summary)>0)
  req(length(rv$gpl_choices)>0)

  # tooltip content
  tooltips <- unlist(unname(lapply(rv$gpl_summary, function(x){
    paste0("<strong>", names(x),"</strong>: ", x, collapse="<br>")
  })))

  # generate tooltip
  for (i in 1:length(rv$gpl_choices)){
    rv$gpl_tooltips[[i]] <- radioTooltip(id = "plat",
                                     choice = rv$gpl_choices[[i]],
                                     title = tooltips[[i]],
                                     placement = "right", trigger = "hover")
  }
})

output$gpl_tooltips <- renderUI({
  req(length(rv$gpl_tooltips)>0)
  rv$gpl_tooltips
})



# detect study type during platform selection
study_type <- reactive({
  req(is.null(rv$gse_all)==F)
  req(length(rv$platforms)>0)
  req(is.null(input$plat)==F)
  
  if(rv$getgeo_mode){
    gse_temp <- rv$gse_all[[match(input$plat, rv$platforms)]]
    # print(gse_temp)
    
    req(is.null(gse_temp)==F)
    
    # get channel count
    gsm_meta_temp <- find_repeating_values(pData(phenoData(gse_temp)))
    channel_count <- gsm_meta_temp$channel_count
    
    # get study type
    gse_meta_temp <- notes(experimentData(gse_temp))
    type <- gse_meta_temp$type
    return(list("type" = type, "channel_count" = channel_count))
  }else{
    return(list("type" = rv$gpl_type[[match(input$plat, rv$platforms)]], "channel_count" = rv$gpl_count[[match(input$plat, rv$platforms)]]))
  }


  

})

output$study_type_feedback <- renderUI({
  req(is.null(rv$gse_all)==F)
  req(length(rv$platforms)>0)
  req(is.null(input$plat)==F)

  study_type <- study_type()$type
  channel_count <- study_type()$channel_count

  errors = 0
  msgs = vector()
  if (study_type %in% accepted_study_types == F){
    errors = errors +1
    msgs <- c(msgs,
              paste0("<strong>CAUTION: </strong>You have selected a study of type: <strong>",study_type, "</strong>, which is not currently supported. <br>
                                Please continue at your own risk.")
    )
    box_color = "yellow"
  }
  
  if(study_type == "Expression profiling by array"){rv$microarray <- "yes"}else{rv$microarray <- "no"}

  # making sure the channel_count is not null to run the function
  if(is.null(channel_count) == F){
    if (channel_count != 1) {
      errors = errors +1
      msgs <- c(msgs,
                paste0("<strong>CAUTION: </strong>Dataset has <strong>",channel_count, "</strong> channels, which is not currently supported.<br>
                                  Only data in the first channel will be read.")
      )
      box_color = "yellow"
    }
  }
  msg <- paste(msgs, collapse="<br>")
  if(errors>0){
    fluidRow(
      box(
        title = NULL, background = box_color, solidHeader = TRUE, width=12,
        HTML(msg)
      )
    )
  } else {return(NULL)}


})

output$select_geo_platform <- renderUI({
  # req(
  #     # study_type()$type %in% accepted_study_types &
  #         study_type()$channel_count == 1)

  actionButton("geo_platform", "Select to proceed")
})


observeEvent(input$geo_platform, {

  plat <- isolate(input$plat)
  rv$plat_id <- match(plat, rv$platforms)
  print(plat)
  withProgress(message = 'Loading data...', value = 1, {
    if(rv$getgeo_mode){
      # initialize the count matrix (even if it's empty) with first row = Name
      exprs <- exprs(gse())
    }else{
      rv$gsmlist <- Filter(function(gsm) {Meta(gsm)$platform_id==plat},GSMList(rv$gse_all))
      # get the probeset ordering
      gpllist <- GPLList(rv$gse_all)
      gpl <- gpllist[[names(gpllist)==plat]]
      probesets <- Table(gpl)$ID
      # make the data matrix from the VALUE columns from each GSM
      # being careful to match the order of the probesets in the platform
      # with those in the GSMs
      exprs <- do.call('cbind',lapply(rv$gsmlist,function(x) {
        tab <- Table(x)
        mymatch <- match(probesets,tab$ID_REF)
        return(tab$VALUE[mymatch])
      }))
      
      if(!is.null(exprs)){
        if (nrow(exprs) >0){
          exprs <- apply(exprs,2,function(x) {as.numeric(as.character(x))})
          rownames(exprs) <- probesets
          colnames(exprs) <- names(rv$gsmlist)
        }
      }else{
        gsms <- names(rv$gsmlist)
        exprs <- data.frame(matrix(vector(), 0, length(gsms),
                                   dimnames=list(c(), c(gsms))),
                            stringsAsFactors=F)
      }
    }
    
    if (nrow(exprs) >0){ # put gene names onto first column
      dmdf <- cbind(Name = rownames(exprs), exprs)
    } else {
      dmdf <- cbind(Name = character(0), exprs)
    }
    
    rownames(dmdf) <- c() # remove rownames
    rv$dmdf <- dmdf
    rv$dmdf_o <- dmdf

    incProgress(0.5)
    
    if(rv$getgeo_mode){
      # initialize samples list
      rv$all_samples <- sampleNames(gse()) # this one stays the same
      rv$samples <- sampleNames(gse()) # this one will change
      
      # initialize pdata
      rv$pdata <- pData(phenoData(gse()))
    }else{
      rv$all_samples <- rv$samples <- names(rv$gsmlist)
      rv$pdata <- rbindlist(lapply(rv$gsmlist, function(x) as.data.frame(t(Meta(x)), stringsAsFactors=FALSE)), fill=T) %>%
        as.data.frame()
      rownames(rv$pdata) <- names(rv$gsmlist)
    }
    
    
    # initialize fddf
    rv$fddf <- design_df() # initially unfiltered, will update when filter

        
    incProgress(0.2)
    
    # initialize gene identifiers
    if(rv$microarray == "yes"){
      # GPL probes' info
      # gset <- rv$gse_all[[match(plat, rv$platforms)]]
      
      # currently only support GSEMatrix == T
      if(rv$getgeo_mode){
        df <- pData(featureData(gse()))
      }else{
        df <- Table(gpl)
      }

      df <- df %>%
        dplyr::select(any_of(c(matches(accepted_gene_identifiers, ignore.case = T),ends_with("_id", ignore.case = T))))
  
      
      rv$identifiers <- colnames(df)
      rv$identifiers_df <- df
    }
    incProgress(0.3)
  
  })

})

# ---------- when all is done, show guide box to next page ---------
output$guide_1a <- renderUI({
  if (is.null(rv$plat_id)==F){ # user already selected a platform
    msg = "Navigate to <b>2. Data matrix</b> to proceed"
    guide_box("guide1", msg)
  } else {
    return(NULL)
  }
})
output$guide_1a_manual <- renderUI({
  if (is.null(rv$fddf)==F && (is.null(rv$dmdf)==F)){ # user already selected a platform
    msg = "Navigate to <b>2. Data matrix</b> to proceed"
    guide_box("guide1_manual", msg)
  } else {
    return(NULL)
  }
})

observeEvent(input$guide1,{
  updateTabItems(session, "menu1", "tab3")
})
observeEvent(input$guide1_manual,{
  updateTabItems(session, "menu1", "tab3")
})



# --------------- progress info box ---------------

output$progress_1 <- renderUI({

  progress_box(id="infobox_1", prompt="To-dos:",
               msg=c("1. Search a valid GSE number", "2. Select a platform", "3. Read the study information"),
               condition=c(!is.null(rv$gse_all), !is.null(rv$plat_id), !is.null(rv$plat_id)),
               bttn_id="next_p1"
               )

})

observeEvent(input$next_p1, {
  updateTabItems(session, "menu1", "tab3")
})

# the functions of the buttons redirect to Manual upload, and reset upload
observeEvent(input$parsing_error_confirm, {
  removeModal()
  confirm_and_jump()
})
observeEvent(input$parsing_error_reset, {
  removeModal()
  # shinyjs::reset("geo_accession")
})