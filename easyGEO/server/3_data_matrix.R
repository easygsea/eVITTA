# --------------- overall data matrix tab UI ---------------
output$ui_dm <- renderUI({
  if(is.null(rv$plat_id)){
    panel_null()
  }else{
    fluidRow(
      column(4,
             
             box(title=span(icon("download"),"Get data matrix"), width = 12, solidHeader=F, status = "primary", 
                 id = "download_matrix",
                 
                 uiOutput("data_matrix_ui")
                 
             ),
             
             uiOutput("upload_matrix_ui")
             
      ),
      column(8,
             
             box(title=span(icon("table"),"Processed data matrix"), width = 12, solidHeader=F, status = "primary", 
                 id = "data_matrix",
                 
                 fluidRow(
                   column(12,
                          box(title=NULL, width = 6, solidHeader=T, status="primary",
                              radioGroupButtons(
                                inputId = "dmdf_show_coln",
                                label = "Show column names as:", 
                                choices = c("GEO accession", "Sample name"),
                                selected = "GEO accession"
                              ),
                              
                          ),
                          
                          uiOutput("dmdf_filter_ui"),
                          
                          DT::dataTableOutput("data_matrix_df")
                          
                   )
                   
                   
                 )
                 
             )
             
      )
    )
    
  }
})


# --------------- detect data source and provide download ---------------

# Data matrix source is stored in rv$sup_source
# "table" = in gse as datatable
# "gse_sup" = in gse as supplementary
# "gsm_sup" = in gsm as supplementary (usually when this happens, gse_sup is also provided)
# "none" = none of the above



output$data_matrix_ui <- renderUI({
    # check supplementary data in GSE
  gse_sup <- unlist(gse_meta_df()[grep("supplementary_file", gse_meta_df()$Field),"Value"])
  print(gse_sup)
  gse_sup[gse_sup=="NONE"] <- NA # convert NONE to NA
  print(gse_sup)
  gse_sup <- gse_sup[is.na(gse_sup)==F] # delete NA
  print(gse_sup)
  if(!identical(gse_sup, character(0))){
    gse_sup <- strsplit(gse_sup, "\n")[[1]]
  }
  print(gse_sup)
  
  # check supplementary data in GSMs
  
  gsm_sup <- unlist(gsm_meta_df()[grep("supplementary_file", gsm_meta_df()$Field),"Value"])
  gsm_sup[gsm_sup=="NONE"] <- NA # convert NONE to NA
  gsm_sup <- gsm_sup[is.na(gsm_sup)==F] # delete NA
  gsm_sup <- unlist(gsm_sup)
  print(gsm_sup)
  
  
  # detect where the data is
  if (nrow(exprs(gse()))>0){
    source = "table"
    text <- "<strong>Detected data source: <br>as preloaded datatable.</strong> 
                        <br>Datatable will be shown on the right."
    where <- "table here"
  } else if (length(gse_sup) > 0){
    source = "gse_sup"
    text <- "<strong>Detected data source: <br>as supplementary files in series record.</strong> 
                        <br>Please download the processed data matching the currently selected platform."
    where <- paste0("GSE supplementary (",length(gse_sup),"): ", paste(gse_sup, collapse=", "))
    rv$suplist <- gse_sup
  } else if (length(gsm_sup) > 0){
    source = "gsm_sup"
    text <- "<strong>Detected data source: <br>as supplementary files in samples record.</strong>
                        <br>Please download the processed data matching the currently selected platform."
    where <- paste0("GSM supplementary (",length(gsm_sup),"): ", paste(gsm_sup, collapse=", "))
    rv$suplist <- gsm_sup
  } else {
    source = "none"
    text <- "<strong>Data source not detected!</strong>
                        <br>Please download the processed data matrix from the GEO page or the associated paper."
    where <- ""
  }
  
  # write the detected data source into rv
  rv$sup_source <- source
  
  # output report and url links to the user
  div(
    HTML(text), br(), br(),
    # where,
    uiOutput("sup_links")
  )
  
})


# generates url links
output$sup_links <- renderUI({
  req(rv$sup_source == "gse_sup" | rv$sup_source == "gsm_sup" )
  
  o_list <- lapply(1:length(rv$suplist), function(i){
    path = rv$suplist[[i]]
    ftp = dirname(path)
    # ftp_id = paste0("ftp",i)
    ftpa_id = paste0("ftpa",i)
    
    output[[ftpa_id]] <- downloadHandler(
      filename <- function() {
        basename(path)
      },
      content <- function(file) {
        rv$ftpy = basename(path)
        o_file = paste0(getwd(),"/www/tmp/",basename(path))
        if(! file.exists(o_file)){
          # curl::curl_fetch_disk(path, o_file)
          download.file(path, o_file, method="wget")
        }
        file.copy(o_file, file)
      }
    )
    
    div(style="display: inline-block;vertical-align:top; width: 100%;word-break: break-word;",
                     wellPanel(basename(path), br(),
                                       # conditionalPanel(
                                       #   "false", downloadButton(ftp_id, "Download")
                                       # ),
                                       downloadLink(ftpa_id,"Download")
                                       # a("Download", href=path), " / ",
                                       # a("FTP Folder", href=ftp),
                     ))
  })
  
  do.call(tagList, o_list)
})    
    
# ------------ observe rv$ftpy, render reminder modal -------------
observeEvent(rv$ftpy,{
  showModal(modalDialog(
    title = h2(paste0("Successfully downloaded ",rv$ftpy)),
    div(style="font-size:200%",
      tags$strong("Please decompress and check your downloaded data:"),
      br(),br(),
      tags$li("If they are raw/normalized counts, please tidy them up and upload the right format according to our instructions below."),
      br(),
      tags$li(HTML("If they are analyzed, proceed to <a href='http://tau.cmmt.ubc.ca/eVITTA/easyGSEA/' target='_blank'><b>easyGSEA</b></a> for enrichment analysis or <a href='http://tau.cmmt.ubc.ca/eVITTA/easyVizR/' target='_blank'><b>easyVizR</b></a> for multiple comparisons.")),
    ),
    size = "l", easyClose = T, footer = modalButton("Got it!")
    
  ))
})  
 


# --------------- upload tidied matrix (show conditionally) ---------------


#     Full Count matrix is stored in rv$dmdf  (aka. data matrix df)
#     
#     DYNAMICS:
#         - rv$dmdf is initialized from exprs(gse()) when user selects platform. 
#     - if data is in supplementary, rv$dmdf will be initially empty (0 rows). 
#     when users upload data, the app will populate rv$dmdf by matching column names (thus column order doesn't have to be the same). 
# 	gene names will be populated into the "Name"column. 
#     - if data is in the gse to begin with (e.g. GSE137355), it will be initialized into rv$dmdf upon platform selection.
#     
#     STRUCTURE:
#     - First column is "Name", with all the gene names. (no case coercion atm).
#     - data in the rest of the columns
#     - internally, column names must be GSM accession numbers. this can be easily converted to sample names by translate_sample_names() function. 


output$upload_matrix_ui <- renderUI({
  req(rv$sup_source == "gse_sup" | rv$sup_source == "gsm_sup" | rv$sup_source == "none")
  
  box(title=span(icon("upload"),"Upload tidied matrix"), width = 12, solidHeader=F, status = "primary", 
      id = "upload_matrix",
      
      div(
        fileInput("file",
                  # help button added Edtion 1
                label = p("Upload tidied data matrix (CSV/TSV format):",
                          tags$style(type = "text/css", "#file_help {display: inline-block;width: 20px;height: 20px;padding: 0;border-radius: 50%;vertical-align: baseline;}"),
                          bsButton("file_help", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                # "Upload tidied data matrix (CSV/TSV format):",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",".tsv",".txt",".tab")
      ), 
      bsTooltip("file_help", "Click to learn more", placement = "top")
      )
      # tags$hr(style="border-color: grey;"),
      # 
      # strong("Note on uploading data matrix:"),br(),
      # "1) the data should be in comma- or tab-delimited format (csv, tsv, tab, txt),",br(),
      # "2) first row of matrix should be sample names; must match either the GEO accession or sample names,",br(),
      # "3) first column of matrix should be gene names; no duplicates are allowed,", br(),
      # "4) IMPORTANT FOR EXCEL USERS: to prevent excel auto-formatting gene names into dates, 
      #                when importing data into excel, select 'do not detect data types'."
      # 
      
      )
  
})
# help button page added Edition 2
observeEvent(input$file_help,{
  showModal(modalDialog(
    inputId = "file_help_html",
    #title = "Ranked list file format (*.rnk)",
     includeHTML(paste0(getwd(),"/server/help_button_page.html")),
    # dataTableOutput('example_data1'),
    # includeMarkdown(paste0(getwd(),"/inc/rnk_explaination.md")),
    # includeMarkdown(knitr::knit(paste0(getwd(),"/inc/rnk_explaination.Rmd"),quiet=T)),
    easyClose = TRUE,size="l",
    footer = modalButton("Close")
  ))
})

output$example3 <- renderTable({(example_data3 <- read.csv(paste0(getwd(),"/server/easyGEO_example1.rnk"),header = TRUE, sep = "\t"))},escape = FALSE)


# when file is uploaded, update the stored count matrix
observeEvent(input$file, {
  inFile <- input$file
  
  # the modal that appears when the file user upload exceeds 50MB, Version1
  if(inFile$size >= 50*1024^2){
    showModal(modalDialog(
      inputId = "size_reminder_modal",
      # title = "The file size exceeds 50MB.",
      div("The file you uploaded exceeds 50MB, please modify it to proceed. Try to delete unneeded columns and 
            only keep the columns that you are interested in. 
            Then press \"Browse...\" to upload it again. Thank you.",style="font-size:200%"),
      easyClose = TRUE,size="l"
      # , footer = modalButton("Close")
    ))
  }
  
  #added try() here because there could be an error reading the file
  indf <- try(read.csv(inFile$datapath, header=F, 
                   colClasses=c("character")))
  
  # read in TAB delimited
  if(ncol(indf)==1){
    indf <- try(read.table(inFile$datapath, sep="\t",header=F, 
                       colClasses=c("character")))
  }
  
  # read in space delimited
  if(ncol(indf)==1){
    indf <- try(read.table(inFile$datapath, sep=" ",header=F, 
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
    ))
    }
  

  req(!inherits(indf, "try-error")) #require to be no error to proceed the following codes
  
  # This part removes duplicate rows of indf
  DuplicateCheck <- as.data.frame(indf[,1], drop=FALSE) #extract first column of indf and check if there is 
  DuplicateCheck <- duplicated(DuplicateCheck)
  indf<-indf[!DuplicateCheck, ] #remove duplicate rows
  DuplicateRows <- which(DuplicateCheck == TRUE, arr.ind=TRUE)
  if(length(DuplicateRows) > 0){ # if there are duplicate rows
    showModal(modalDialog( 
      title = "Warning: Your input file contains duplicate gene names",
      HTML(paste0("Duplicate entries were omitted.<br>",
                  "If this is not what you intend, please double check and try again.")),
      size = "l",
      easyClose = TRUE
    ))
  }
  ###
  
  indf_coln <- unname(unlist(indf[1,])) # colnames = first row
  print(indf_coln)
  indf_rown <- unname(unlist(indf[,1])) # rownames = first col
  print(head(indf_rown))
  indf <- indf[-1,-1]
  # print(head(indf))
  
  # try to convert the indf headers into gsm format. matching is done in upper case.
  translation_df <- rv$pdata[c("title", "geo_accession")]
  translation_df$title <- toupper(translation_df$title) # convert the translation df to upper case as well
  indf_coln <- translate_sample_names(toupper(indf_coln),  # translating from (upper case)
                                      translation_df,  # translation df
                                      "geo_accession") # translating to
  colnames(indf) <- indf_coln[-1]
  
  # print(head(indf))
  
  # then, match each column to rv$dmdf and update the values into it
  rvdf <- rv$dmdf
  dmdf <- data.frame(matrix(NA, nrow = nrow(indf), ncol = ncol(rvdf))) # initialize empty df
  dmdf <- data.frame(lapply(colnames(rvdf), function(x){ # update values into dmdf (leaves NA if not found)
    if (x %in% colnames(indf)){
      indf[[x]]
    } else {
      return (rep(NA, nrow(indf)))
      }
  }))
  
  matched_cols <- intersect(colnames(rvdf)[-1], colnames(indf)) # a vector of GSM id for matched cols
  unmatched_cols <- setdiff(colnames(indf), matched_cols) # vector of uploaded colnames that cannot find a match
  print(matched_cols)
  #print(length(matched_cols))
  print(unmatched_cols)
  #print(length(unmatched_cols))
  
 
  
  
  colnames(dmdf) <- colnames(rvdf)
  # print(head(dmdf))
  # check if all non-name cols contain any values.
  has_values <- any(unlist(lapply(dmdf[-1], function(x) any(complete.cases(x)))))
  if (has_values == T){
    dmdf$Name <- indf_rown[-1] # if has value, update first row as gene names (!!! beware of excel auto date formatting)
  } else {
    dmdf <- dmdf[0,] # if all nas, delete all rows
  }
  rownames(dmdf) <- c() # clear row names
  
  print(head(dmdf))
  
  rv$dmdf <- dmdf

  #the modal that reminds user that there are unmatched columns exist is added, Vesion 1
  #declare some variables
    unmatched_cols_length_one_character <- glue_collapse(unmatched_cols, sep = ", ", last = " and ")
    length_matched <- length(matched_cols)
    length_unmatched <- length(unmatched_cols)
 
     #the code of the modal
    if(length_unmatched > 0){showModal(modalDialog(
      inputId = "column_match_modal",
      # title = "See below for your column information",
      span("IMPORTANT: ", style = "font-size:200%"),
      span(length_matched,style = "font-size:200%"),
      span(" columns successfully read; ",style = "font-size:200%"),
      span(length_unmatched,style = "font-size:200%"),
      span(" columns omitted because column name does not match existing sample names (",style = "font-size:200%"),
      span(unmatched_cols_length_one_character,style = "font-size:200%"),
      span("). Please check your file, column names as well as the platform, and then try again.",style = "font-size:200%"),
      #2 columns successfully read; 2 columns omitted because column name does not match existing sample names (C, D). Please check your file and try again.
      easyClose = TRUE,size="l"
      # , footer = modalButton("Close")
    ))}
  })

# --------------- show data matrix df ---------------

output$data_matrix_df <- DT::renderDataTable({
  req(is.null(rv$gse_all)==F)
  req(is.null(rv$plat_id)==F)
  req(is.null(rv$dmdf)==F)
  req(nchar(input$dmdf_filter))
  
  df <- rv$dmdf
  
  # filter according to stored sample list
  if (input$dmdf_filter == "Filtered"){
    df <- filtered_data_showdf()
  }
  
  # translate GSM column names to sample names on display
  if (input$dmdf_show_coln == "Sample name"){
    
    colnames(df) <- translate_sample_names(colnames(df),  # translating from
                                           rv$pdata[c("title", "geo_accession")],  # translation df
                                           "title") # translating to
  }
  
  
  df
  
}, plugins="ellipsis",
options=dt_options(30, scrollX=T)
)

# select whether to filter
output$dmdf_filter_ui <- renderUI({
  req(length(rv$all_samples)>0)
  
  fm <- paste0("Full matrix (",length(rv$all_samples),")")
  fl <- paste0("Filtered (",length(rv$samples),")")
  choices <- c("Full matrix", "Filtered")
  names(choices) <- c(fm, fl)
  box(title=NULL, width = 6, solidHeader=T, status="primary",
      radioGroupButtons(
        inputId = "dmdf_filter",
        label = "Filtering options:",
        choices = choices,
        selected = "Filtered"
      ),
  )
})

# filter count matrix by selected samples (DISPLAY ONLY)
filtered_data_showdf <- reactive({
  req(is.null(rv$dmdf)==F)
  req(length(rv$samples)>0)
  
  dmdf <- rv$dmdf
  dmdf <- dmdf[,c("Name", rv$samples)]
  dmdf
  
})


# filter count matrix by selected samples (USE FOR ANALYSIS)
filtered_data_df <- reactive({
  req(is.null(rv$dmdf)==F)
  req(length(rv$samples)>0)
  
  dmdf <- rv$dmdf
  dmdf <- dmdf[,c("Name", rv$samples)]
  dmdf[dmdf==""]<-NA # replace empty string with na
  dmdf <- dmdf[complete.cases(dmdf),] # get rid of na values
  dmdf
  
})
