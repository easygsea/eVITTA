# --------------- detect data source and provide download ---------------

# Data matrix source is stored in rv$sup_source
# "table" = in gse as datatable
# "gse_sup" = in gse as supplementary
# "gsm_sup" = in gsm as supplementary (usually when this happens, gse_sup is also provided)
# "none" = none of the above



output$data_matrix_ui <- renderUI({
  
  # check supplementary data in GSE
  gse_sup <- unlist(gse_meta_df()[grep("supplementary_file", gse_meta_df()$Field),"Value"])
  gse_sup[gse_sup=="NONE"] <- NA # convert NONE to NA
  gse_sup <- gse_sup[is.na(gse_sup)==F] # delete NA
  gse_sup <- strsplit(gse_sup, "\n")[[1]]
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
  req(rv$sup_source == "gse_sup" | rv$sup_source == "gsm_sup" | rv$sup_source == "none")
  
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
    
    div(style="display: inline-block;vertical-align:top; width: 100%;",
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
    
# observe rv$ftpy, render successful modal
observeEvent(rv$ftpy,{
  showModal(modalDialog(
    title = rv$ftpy,
    "Successfully downloaded.",
    easyClose = T,
    footer = modalButton("Got it!")
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
  
  tabBox(
    title = NULL, width = 12,
    id = "upload_matrix",
    tabPanel("Upload tidied matrix", 
             
             fileInput("file", "Upload tidied data matrix (CSV/TSV format):",
                       accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",".tsv",".txt",".tab")
             ), 
             
             tags$hr(style="border-color: grey;"),
             
             strong("Note on uploading data matrix:"),br(),
             "1) the data should be in comma- or tab-delimited format (csv, tsv, tab, txt),",br(),
             "2) first row of matrix should be sample names; must match either the GEO accession or sample names,",br(),
             "3) first column of matrix should be gene names; no duplicates are allowed,", br(),
             "4) IMPORTANT FOR EXCEL USERS: to prevent excel auto-formatting gene names into dates, 
                     when importing data into excel, select 'do not detect data types'."
             
    )
  )
})

# when file is uploaded, update the stored count matrix
observeEvent(input$file, {
  inFile <- input$file
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
  # print(indf_coln)
  indf_rown <- unname(unlist(indf[,1])) # rownames = first col
  # print(head(indf_rown))
  indf <- indf[-1,-1]
  # print(head(indf))
  
  # try to convert the indf headers into gsm format
  indf_coln <- translate_sample_names(indf_coln,  # translating from
                                      rv$pdata[c("title", "geo_accession")],  # translation df
                                      "geo_accession") # translating to
  colnames(indf) <- indf_coln[-1]
  
  # print(head(indf))
  
  # then, match each column to rv$dmdf and update the values into it
  rvdf <- rv$dmdf
  dmdf <- data.frame(matrix(NA, nrow = nrow(indf), ncol = ncol(rvdf))) # initialize empty df
  dmdf <- data.frame(lapply(colnames(rvdf), function(x){ # update values into dmdf (leaves NA if not found)
    if (x %in% colnames(indf)){
      dmdf[[x]] <- indf[[x]]
    } else {return (rep(NA, nrow(indf)))}
  }))
  
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
    df <- filtered_data_df()
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

# filter count matrix by selected samples
filtered_data_df <- reactive({
  req(is.null(rv$dmdf)==F)
  req(length(rv$samples)>0)
  
  rv$dmdf[,c("Name", rv$samples)] 
})
