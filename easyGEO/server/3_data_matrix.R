# --------------- overall data matrix tab UI ---------------
output$ui_dm <- renderUI({
  if(is.null(rv$plat_id) && rv$run_mode == "auto"){
    panel_null()
  } else if(rv$run_mode == "manual" && is.null(rv$dmdf)){
    panel_null(text = "Data available upon sucessfully uploading your gene expression (data) matrix.")
  } else {
    fluidRow(
      column(4,
             if(rv$run_mode == "auto"){
              box(title=span(HTML("<b>2.1.</b>"),icon("download"),"Get data matrix"), width = 12, solidHeader=F, status = "primary", 
                 id = "download_matrix",
                 
                 uiOutput("data_matrix_ui")
                 
              )} else {
                box(title=span("Data matrix"), width = 12, solidHeader=F, status = "primary", 
                    # id = "view_data_matrix",
                    
                    HTML("Review your uploaded data on the right.")
                    
                )
              },
             
             uiOutput("upload_matrix_ui"),
             
             uiOutput("guide_box2")
             
      ),
      column(8,
             
             box(title=span(icon("table"),"Review processed data matrix", uiOutput("download_dmdf_button")), width = 12, solidHeader=F, status = "primary", 
                 id = "data_matrix",
                 
                 fluidRow(
                   column(12,
                          if(rv$run_mode == "auto")
                          box(title=NULL, width = 6, solidHeader=T, status="primary",
                              radioGroupButtons(
                                inputId = "dmdf_show_coln",
                                label = "Show column names as:", 
                                choices = c("GEO accession", "Sample name"),
                                selected = "GEO accession"
                              ),
                              
                          ),
                          
                          uiOutput("dmdf_filter_ui"),
                          
                            DT::dataTableOutput("data_matrix_df"),
                          # display text to remind the users about the column matching errors
                            if(rv$column_match_error == TRUE){
                              fluidRow(
                                column(
                                  12,
                                  HTML("<p style = 'color:red'> <strong>Your data matrix cannot be filtered using the criteria in 3.Filter/review design matrix: Samples indicated in your design matrix are missing in the provided data matrix. 
                                   Please adjust the filters on the next tab, or check and reupload your data</strong></p>")
                                  
                                )
                                                                
                              )
                            }

                   )
                   
                   
                 )

                 
             )
             
      )
    )
    
  }
})

# the button to download rv$dmdf, the data matrix
output$download_dmdf_button <- renderUI({
  req(!is.null(rv$dmdf))
  req(nrow(rv$dmdf) >= 1)

  div(
    id = "download_dm", style = "display: inline-block; position: absolute; right: 0px; top: 4px",
    downloadBttn(
      size = "md", style = "unite",
      outputId = "download_dmdf", label = NULL
    ),
    bsTooltip("download_dm", "Download the data matrix."
              ,placement = "top")

  )
})

output$download_dmdf <- downloadHandler(
  filename = function(){paste0(rv$geo_accession,"_data_matrix.csv")},
  content = function(file){
    # translate GSM column names to sample names on display
    if (input$dmdf_show_coln == "Sample name" 
        && rv$run_mode == "auto"){
      # the downloaded df that with sample names
      df_download <- rv$dmdf
      colnames(df_download) <- translate_sample_names(colnames(df_download),  # translating from
                                             rv$pdata[c("title", "geo_accession")],  # translation df
                                             "title") # translating to
      fwrite(df_download, file)
    } else {
        fwrite(rv$dmdf, file)
      }
  }
)

# --------------- detect data source and provide download ---------------

# Data matrix source is stored in rv$sup_source
# "table" = in gse as datatable
# "gse_sup" = in gse as supplementary
# "gsm_sup" = in gsm as supplementary (usually when this happens, gse_sup is also provided)
# "none" = none of the above



output$data_matrix_ui <- renderUI({
    # check supplementary data in GSE
  gse_sup <- unlist(gse_meta()[grep("supplementary_file", names(gse_meta()))])
  gse_sup[gse_sup=="NONE"] <- NA # convert NONE to NA
  gse_sup <- gse_sup[is.na(gse_sup)==F] # delete NA
  # print(gse_sup)
  if(rv$getgeo_mode){
    if(!identical(gse_sup, character(0))){
      gse_sup <- strsplit(gse_sup, "\n")[[1]]
    }
  }
  # print(gse_sup)
  
  # check supplementary data in GSMs
  
  gsm_sup <- unlist(gsm_meta_df()[grep("supplementary_file", gsm_meta_df()$Field),"Value"])
  gsm_sup[gsm_sup=="NONE"] <- NA # convert NONE to NA
  gsm_sup <- gsm_sup[is.na(gsm_sup)==F] # delete NA
  gsm_sup <- unlist(gsm_sup)
  # print(gsm_sup)
  
  # detect where the data is
  if (rv$expr_nrow>0){
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
    # select identifier, if microarray
    uiOutput("select_identifier"),
    # where,
    uiOutput("sup_links")
  )
  
})

# generate selection of gene identifiers, symbol as the default
output$select_identifier <- renderUI({
  req(rv$identifiers)
  
  div(
    selectizeInput("identifier",HTML("Select the gene identifier:",add_help("identifier_q"))
                   ,choices = c("Default",rv$identifiers)
                   ,selected = rv$identifiers[grepl("symbol",rv$identifiers,ignore.case = T)][1]
    )
    ,bsTooltip("identifier_q",HTML("Probe IDs auto-converted to HNGC symbols (if available) on default")
               ,placement = "top")
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
          download.file(path, o_file, method="libcurl")
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
  # hlink = paste0(getwd(),"/inc/uploading_GEO_data_matrix.png")
  showModal(modalDialog(
    title = h2(paste0("Successfully downloaded ",rv$ftpy)),
    div(style="font-size:200%",
      tags$strong("Please decompress and check the downloaded file(s):"),
      br(),br(),
      tags$li(HTML("If downloaded data files contain raw/normalized counts (e.g. CPM, FPKM, RPKM), please tidy them up according to our <a href='https://tau.cmmt.ubc.ca/bu/uploading_GEO_data_matrix.png' target='_blank'><b>instructions</b></a>. Then upload the processed file in step <b>2.2. Upload tidied matrix</b>.")),
      br(),
      tags$li(HTML("If downloaded data are analyzed data (e.g. logFC, p value, FDR), you can proceed directly to <a href='http://tau.cmmt.ubc.ca/eVITTA/easyGSEA/' target='_blank'><b>easyGSEA</b></a> for enrichment analysis or <a href='http://tau.cmmt.ubc.ca/eVITTA/easyVizR/' target='_blank'><b>easyVizR</b></a> for multiple comparisons.")),
      
    ),
    size = "l", easyClose = T, footer = modalButton("OK")
    
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
  req(rv$run_mode == "auto")
  
  box(title=span(HTML("<b>2.2.</b>"),icon("upload"),"Upload tidied matrix"), width = 12, solidHeader=F, status = "primary", 
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
     includeHTML(paste0(getwd(),"/inc/help_button_page.html")),
    # dataTableOutput('example_data1'),
    # includeMarkdown(paste0(getwd(),"/inc/rnk_explaination.md")),
    # includeMarkdown(knitr::knit(paste0(getwd(),"/inc/rnk_explaination.Rmd"),quiet=T)),
    easyClose = TRUE,size="l",
    footer = modalButton("OK")
  ))
})
# the data matrix example table
output$example3 <- renderTable({(example_data3 <- read.csv(paste0(getwd(),"/inc/easyGEO_example1.rnk"),header = TRUE, sep = "\t"))},escape = FALSE)

# the function that read the data matrix in the both uploading mode
read_data_matrix <- function(inFile){
  withProgress(message = 'Reading data matrix. This might take a while ...', value = 1, {
  # the modal that appears when the file user upload exceeds 50MB, Version1
  if(inFile$size >= 100*1024^2){
    showModal(modalDialog(
      inputId = "size_reminder_modal",
      # title = "The file size exceeds 100MB.",
      div("The file you uploaded exceeds 100MB, please modify it to proceed. Try to delete unneeded columns and 
            only keep the columns that you are interested in. 
            Then press \"Browse...\" to upload it again. Thank you.",style="font-size:200%"),
      easyClose = TRUE,size="l"
      , footer = modalButton("OK")
    ))
  }
  # check the file name extensions to make sure the file has the correct format
  ext_check <- ""
  if(!tools::file_ext(inFile$name) %in% c(
    "text/csv",
    "text/comma-separated-values,text/plain",
    "csv","tsv","txt","tab")){
    ext_check <- "no"
    shinyjs::reset("data_matrix_file")
    shinyalert("We only accept files that are .csv,.tsv,.txt,.tab; 
                   please check your file and upload the file with the correct file (name) extensions .")
  }
  
  req(ext_check != "no")
  
  #added try() here because there could be an error reading the file
  indf <- try(read.csv(inFile$datapath, header=F, 
                       colClasses=c("character")))
  
  # read in TAB delimited
  if(is.null(ncol(indf)) ||ncol(indf)==1){
    indf <- try(read.table(inFile$datapath, sep="\t",header=F, 
                           colClasses=c("character")))
  }
  # print(head(indf))
  # read in space delimited
  if(is.null(ncol(indf)) ||ncol(indf)==1){
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
                  "Please revise your input file according to our notes and reupload the file. <br>
                  Click <b>Reset upload </b> to upload your tidied file again, or click <b>Confirm</b> to jump to the manual upload mode. ")),
      size = "l",
      easyClose = TRUE
      ,footer = fluidRow(
        div(style = "display:inline-block;",
            actionButton("wrong_format_reset", "Reset upload")
        ),
        div(style = "display:inline-block;",
            actionButton("wrong_format_proceed","OK")  
        )
      ) 
    ))
  }
  
  
  req(!inherits(indf, "try-error")) #require to be no error to proceed the following codes
  
  # check the number of columns of the matrix
  if(ncol(indf) < 2){
      shinyalert("You uploaded a file with < 2 columns, that is not an accepted format. Please click the help button for accepted file formats.")
      shinyjs::reset("data_matrix_file")
  }
  
  req(ncol(indf) > 1)
  # This part removes duplicate rows of indf
  DuplicateCheck <- as.data.frame(indf[,1], drop=FALSE) #extract first column of indf and check if there is 
  DuplicateCheck <- duplicated(DuplicateCheck)
  indf<-indf[!DuplicateCheck, ] #remove duplicate rows
  DuplicateRows <- which(DuplicateCheck == TRUE, arr.ind=TRUE)
  if(length(DuplicateRows) > 0){ # if there are duplicate rows
    showModal(modalDialog( 
      title = "Warning: The file you uploaded contains duplicate gene names",
      HTML(paste0("Only the first duplicate(s) will be kept.<br>",
                  "If this is not what you intend, please double check your input file and re-upload.
                  <br>Click <b>OK</b> to proceed, or click <b>Reset upload</b> to upload your file again. ")),
      size = "l",
      easyClose = TRUE
      ,footer = fluidRow(
        div(style = "display:inline-block;",
            actionButton("duplicated_reset", "Reset upload")
        ),
        div(style = "display:inline-block;",
            actionButton("duplicated_proceed","OK")  
        )
      )
    ))
  }
  ###
  
  indf_coln <- unname(unlist(indf[1,])) # colnames = first row
  indf_rown <- unname(unlist(indf[,1])) # rownames = first col
  # print(head(indf_rown))
  indf <- indf[-1,-1]
  # print(head(indf))
  
  # try to convert the indf headers into gsm format. matching is done in upper case.
  translation_df <- rv$pdata[c("title", "geo_accession")]
  translation_df$title <- toupper(translation_df$title) # convert the translation df to upper case as well
  
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
  # print(indf_coln)
  # if(prod(validUTF8(indf_coln))){indf_coln <- translate_sample_names(toupper(indf_coln),  # translating from (upper case)
  #                                     translation_df,  # translation df
  if(rv$run_mode == "auto"){
    indf_coln <- translate_sample_names(toupper(indf_coln),  # translating from (upper case)
                                     translation_df,  # translation df
                                     "geo_accession") # translating to
 
  }  
    colnames(indf) <- indf_coln[-1]
  # print("a small indicator here")
  # print(head(indf))
  
  if(rv$run_mode == "auto"){
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
   }
  if(rv$run_mode == "manual"){
    # perform data transformation to create rv$indf, which is assigned to rv$dmdf later when the user confirms
    indf <- cbind(Name = indf_rown[-1], indf)
    rownames(indf) <- c()
    rvdf <- indf
    # the vector to store the sample names to compare with the sample names from the design matrix
    rv$dmdf_samples <- indf_coln[-1]
    
    rv$indf <- indf
    
    # the modal that briefly show the data inside the file that the user uploads
    showModal(modalDialog(
      title = div("Data Matrix Upload",style = "font-size:170%"),
      span(HTML("The uploaded file contains these <b>Genes:</b> "),
           # glue_collapse(indf$Name[1:10], sep = ", ", last = " and "), "... (",
           if(length(indf$Name) > 5){
             paste(glue_collapse(indf$Name[1:5], sep = ", ", last = " and "),"...")
           } else {
             glue_collapse(indf$Name, sep = ", ", last = " and ")
           }
           , "(",
           length(indf$Name), HTML(" in total), and  these <b>Samples:</b>"),
           if(length(colnames(indf)) > 6){
             paste(glue_collapse(colnames(indf)[2:6], sep = ", ", last = " and "),"...")
           } else {
             glue_collapse(colnames(indf), sep = ", ", last = " and ")
           }
           , "(",
           length(colnames(indf))-1, " in total).",br(),uiOutput("sample_comparison"),
           uiOutput("data_matrix_warning"), ul_txt,
           style = "font-size:130%"),
      easyClose = F,
      size = "l",
      footer = uiOutput("matrix_buttons")
    ))
    
  }

  
  if(rv$run_mode == "auto"){
    matched_cols <- intersect(colnames(rvdf)[-1], colnames(indf)) # a vector of GSM id for matched cols
  } else {
    matched_cols <- intersect(colnames(rvdf), colnames(indf)) # a vector of GSM id for matched cols
  }
  unmatched_cols <- setdiff(colnames(indf), matched_cols) # vector of uploaded colnames that cannot find a match
  #print(length(matched_cols))
  #print(length(unmatched_cols))
  
  
  if(rv$run_mode == "auto"){
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
    
    # print(head(dmdf))
    
    rv$dmdf <- dmdf
  }

  })
  
  # -------------- potential terms in DEG file, so as to tell users it's already analyzed files -------------
  deg_colnames <- c("logfc","fc","log2_fold_change"
                    ,"p","pval","pvalue","p.value","p_value"
                    ,"fdr","padj","adj.p.val","q_value")
  
  #Check if the users file containg invalid characters, if yes, display a specific modal
  #without running the code that process the columns Version 1
  #if(prod(validUTF8(indf_coln))){
  if(!whether_contains_invalid){
    # Check to see if there is some column names matched the potential terms in DEG file
    match_deg_cols <- intersect(tolower(deg_colnames),tolower(unmatched_cols))
    match_deg_cols_one_character <- glue_collapse(match_deg_cols, sep = ", ", last = " and ")
    # print(match_deg_cols)
    # print(length(match_deg_cols))
    
    #the modal that reminds user that there are unmatched columns exist is added, Vesion 1
    #declare some variables
    unmatched_cols_length_one_character <- glue_collapse(unmatched_cols, sep = ", ", last = " and ")
    length_matched <- length(matched_cols)
    length_unmatched <- length(unmatched_cols)
    
    #the code of the modal
    #check if there is any unmatched columns
    if(length_unmatched > 0){
      #check to see if the unmatched columns contains the DEG analysis term, such as logfc, pvalue
      if(length(match_deg_cols) > 0){
        
        #the modal that remind the user that the uploaded file has already been analysed 
        #because the column names match the DEG analysis names such as pvalue and logFC
        showModal(modalDialog(
          inputId = "match_DEG_modal",
          # title = "See below for your column information",
          span("IMPORTANT: ", style = "font-size:200%"),
          span(length_matched,style = "font-size:200%"),
          span(" columns successfully read; ",style = "font-size:200%"),
          span("your file's column names contain: ",style = "font-size:200%"),
          span(match_deg_cols_one_character, style = "font-size:200%"),
          span(". Your file probably has already been processed and analyzed. Please Check it again;
               if it has been processed, proceed to ",style = "font-size:200%"),
          HTML("<a href='http://tau.cmmt.ubc.ca/eVITTA/easyGSEA/' target='_blank' style = 'font-size:200%'><u><b>easyGSEA</b></u></a>"),
          span(" or ", style = "font-size:200%"),
          HTML("<a href='http://tau.cmmt.ubc.ca/eVITTA/easyVizR/' target='_blank' style = 'font-size:200%'><u><b>easyVizR</b></u></a>"),
          span(" for further analysis. Thank you.", style = "font-size:200%"),
          easyClose = TRUE,size="l"
          , footer = modalButton("OK")
        ))
      }
      else{
        
        #the modal indicated unrecognizable columns
        showModal(modalDialog(
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
          , footer = modalButton("OK")
        ))}
    }
  }else{
    #the modal that reminds the user their file contains invalid characters Version 1
    showModal(modalDialog(
      inputId = "invalid_character_modal",
      span("IMPORTANT: Unrecognized characters are detected in the uploaded file and removed for downstream analysis. Please check your uploaded file's column names. Convert them into corresponding GSM IDs if possible. Thank you.", style = "font-size:200%"),
      easyClose = TRUE, size = "l"
      , footer = modalButton("OK")
      
    ))}
  
}

# when file is uploaded, update the stored count matrix
observeEvent(input$file, {
  inFile <- input$file
  read_data_matrix(inFile = inFile)
  
  # # the modal that appears when the file user upload exceeds 50MB, Version1
  # if(inFile$size >= 100*1024^2){
  #   showModal(modalDialog(
  #     inputId = "size_reminder_modal",
  #     # title = "The file size exceeds 100MB.",
  #     div("The file you uploaded exceeds 100MB, please modify it to proceed. Try to delete unneeded columns and 
  #           only keep the columns that you are interested in. 
  #           Then press \"Browse...\" to upload it again. Thank you.",style="font-size:200%"),
  #     easyClose = TRUE,size="l"
  #     , footer = modalButton("OK")
  #   ))
  # }
  # 
  # #added try() here because there could be an error reading the file
  # indf <- try(read.csv(inFile$datapath, header=F, 
  #                  colClasses=c("character")))
  # 
  # # read in TAB delimited
  # if(ncol(indf)==1){
  #   indf <- try(read.table(inFile$datapath, sep="\t",header=F, 
  #                      colClasses=c("character")))
  # }
  # 
  # # read in space delimited
  # if(ncol(indf)==1){
  #   indf <- try(read.table(inFile$datapath, sep=" ",header=F, 
  #                      colClasses=c("character")))
  # }
  # 
  # if(inherits(indf, "try-error")) {        
  #   ErrorMessage <- conditionMessage(attr(indf, "condition"))  # the error message
  #   #show a modal dialog if there is an error reading files causing crash
  #   showModal(modalDialog( 
  #   title = "Your input file has a wrong format",
  #   HTML(paste0("Having trouble loading your file:<br>",
  #     ErrorMessage,"<br>",
  #     "Please revise your input file according to our notes and reupload the file.")),
  #   size = "l",
  #   easyClose = TRUE
  #   ,footer = modalButton("OK")
  #   ))
  #   }
  # 
  # 
  # req(!inherits(indf, "try-error")) #require to be no error to proceed the following codes
  # 
  # # This part removes duplicate rows of indf
  # DuplicateCheck <- as.data.frame(indf[,1], drop=FALSE) #extract first column of indf and check if there is 
  # DuplicateCheck <- duplicated(DuplicateCheck)
  # indf<-indf[!DuplicateCheck, ] #remove duplicate rows
  # DuplicateRows <- which(DuplicateCheck == TRUE, arr.ind=TRUE)
  # if(length(DuplicateRows) > 0){ # if there are duplicate rows
  #   showModal(modalDialog( 
  #     title = "Warning: The file you uploaded contains duplicate gene names",
  #     HTML(paste0("Only the first duplicate(s) will be kept.<br>",
  #                 "If this is not what you intend, please double check your input file and re-upload.")),
  #     size = "l",
  #     easyClose = TRUE
  #     ,footer = modalButton("OK")
  #   ))
  # }
  # ###
  # 
  # indf_coln <- unname(unlist(indf[1,])) # colnames = first row
  # indf_rown <- unname(unlist(indf[,1])) # rownames = first col
  # print(head(indf_rown))
  # indf <- indf[-1,-1]
  # # print(head(indf))
  # 
  # # try to convert the indf headers into gsm format. matching is done in upper case.
  # translation_df <- rv$pdata[c("title", "geo_accession")]
  # translation_df$title <- toupper(translation_df$title) # convert the translation df to upper case as well
  # 
  # #print(indf_coln)
  # #print(validUTF8(indf_coln))
  # #print(prod(validUTF8(indf_coln)))
  # #adding a IF statement to ensure that the characters in the column names are encoded correctly,
  # #which means there is no invalid characters inside the column names
  # if(prod(validUTF8(indf_coln))){
  #   whether_contains_invalid <- FALSE
  # }
  # else{
  #   #delete the column names that contain invalid characters
  #   #indf_coln <- indf_coln[validUTF8(indf_coln)]
  #   whether_contains_invalid <- TRUE
  #   for(i in seq_along(indf_coln)){
  #     #delete the unrecognized character
  #     indf_coln[i] <- stringr::str_replace_all(indf_coln[i],"[^(a-z0-9A-Z)|[:punct:]]", "")
  #     # print(indf_coln[i])
  #   }
  # }
  # print(indf_coln)
  # # if(prod(validUTF8(indf_coln))){indf_coln <- translate_sample_names(toupper(indf_coln),  # translating from (upper case)
  # #                                     translation_df,  # translation df
  # #   
  # indf_coln <- translate_sample_names(toupper(indf_coln),  # translating from (upper case)
  #                                     translation_df,  # translation df
  #                                     "geo_accession") # translating to
  # #print(indf_coln)
  # 
  # colnames(indf) <- indf_coln[-1]
  # #print("a small indicator here")
  # # print(head(indf))
  # 
  # # then, match each column to rv$dmdf and update the values into it
  # rvdf <- rv$dmdf
  # dmdf <- data.frame(matrix(NA, nrow = nrow(indf), ncol = ncol(rvdf))) # initialize empty df
  # dmdf <- data.frame(lapply(colnames(rvdf), function(x){ # update values into dmdf (leaves NA if not found)
  #   if (x %in% colnames(indf)){
  #     indf[[x]]
  #   } else {
  #     return (rep(NA, nrow(indf)))
  #     }
  # }))
  # 
  # matched_cols <- intersect(colnames(rvdf)[-1], colnames(indf)) # a vector of GSM id for matched cols
  # unmatched_cols <- setdiff(colnames(indf), matched_cols) # vector of uploaded colnames that cannot find a match
  # print(matched_cols)
  # #print(length(matched_cols))
  # print(unmatched_cols)
  # #print(length(unmatched_cols))
  # 
  # 
  # 
  # colnames(dmdf) <- colnames(rvdf)
  # # print(head(dmdf))
  # # check if all non-name cols contain any values.
  # has_values <- any(unlist(lapply(dmdf[-1], function(x) any(complete.cases(x)))))
  # if (has_values == T){
  #   dmdf$Name <- indf_rown[-1] # if has value, update first row as gene names (!!! beware of excel auto date formatting)
  # } else {
  #   dmdf <- dmdf[0,] # if all nas, delete all rows
  # }
  # rownames(dmdf) <- c() # clear row names
  # 
  # print(head(dmdf))
  # 
  # rv$dmdf <- dmdf
  # 
  # 
  # # -------------- potential terms in DEG file, so as to tell users it's already analyzed files -------------
  # deg_colnames <- c("logfc","fc","log2_fold_change"
  #                   ,"p","pval","pvalue","p.value","p_value"
  #                   ,"fdr","padj","adj.p.val","q_value")
  # 
  # #Check if the users file containg invalid characters, if yes, display a specific modal
  # #without running the code that process the columns Version 1
  # #if(prod(validUTF8(indf_coln))){
  # if(!whether_contains_invalid){
  # # Check to see if there is some column names matched the potential terms in DEG file
  # match_deg_cols <- intersect(tolower(deg_colnames),tolower(unmatched_cols))
  # match_deg_cols_one_character <- glue_collapse(match_deg_cols, sep = ", ", last = " and ")
  # print(match_deg_cols)
  # print(length(match_deg_cols))
  # 
  # #the modal that reminds user that there are unmatched columns exist is added, Vesion 1
  # #declare some variables
  #   unmatched_cols_length_one_character <- glue_collapse(unmatched_cols, sep = ", ", last = " and ")
  #   length_matched <- length(matched_cols)
  #   length_unmatched <- length(unmatched_cols)
  # 
  #    #the code of the modal
  #   #check if there is any unmatched columns
  #   if(length_unmatched > 0){
  #   #check to see if the unmatched columns contains the DEG analysis term, such as logfc, pvalue
  #     if(length(match_deg_cols) > 0){
  #       
  #       #the modal that remind the user that the uploaded file has already been analysed 
  #       #because the column names match the DEG analysis names such as pvalue and logFC
  #       showModal(modalDialog(
  #         inputId = "match_DEG_modal",
  #         # title = "See below for your column information",
  #         span("IMPORTANT: ", style = "font-size:200%"),
  #         span(length_matched,style = "font-size:200%"),
  #         span(" columns successfully read; ",style = "font-size:200%"),
  #         span("your file's column names contain: ",style = "font-size:200%"),
  #         span(match_deg_cols_one_character, style = "font-size:200%"),
  #         span(". Your file probably has already been processed and analyzed. Please Check it again;
  #              if it has been processed, proceed to ",style = "font-size:200%"),
  #         HTML("<a href='http://tau.cmmt.ubc.ca/eVITTA/easyGSEA/' target='_blank' style = 'font-size:200%'><u><b>easyGSEA</b></u></a>"),
  #         span(" or ", style = "font-size:200%"),
  #         HTML("<a href='http://tau.cmmt.ubc.ca/eVITTA/easyVizR/' target='_blank' style = 'font-size:200%'><u><b>easyVizR</b></u></a>"),
  #         span(" for further analysis. Thank you.", style = "font-size:200%"),
  #         easyClose = TRUE,size="l"
  #         , footer = modalButton("OK")
  #       ))
  #     }
  #     else{
  #       
  #       #the modal indicated unrecognizable columns
  #       showModal(modalDialog(
  #         inputId = "column_match_modal",
  #         # title = "See below for your column information",
  #         span("IMPORTANT: ", style = "font-size:200%"),
  #         span(length_matched,style = "font-size:200%"),
  #         span(" columns successfully read; ",style = "font-size:200%"),
  #         span(length_unmatched,style = "font-size:200%"),
  #         span(" columns omitted because column name does not match existing sample names (",style = "font-size:200%"),
  #         span(unmatched_cols_length_one_character,style = "font-size:200%"),
  #         span("). Please check your file, column names as well as the platform, and then try again.",style = "font-size:200%"),
  #         #2 columns successfully read; 2 columns omitted because column name does not match existing sample names (C, D). Please check your file and try again.
  #         easyClose = TRUE,size="l"
  #         , footer = modalButton("OK")
  #       ))}
  #   }
  # }else{
  #   #the modal that reminds the user their file contains invalid characters Version 1
  #   showModal(modalDialog(
  #     inputId = "invalid_character_modal",
  #     span("IMPORTANT: Unrecognized characters are detected in the uploaded file and removed for downstream analysis. Please check your uploaded file's column names. Convert them into corresponding GSM IDs if possible. Thank you.", style = "font-size:200%"),
  #     easyClose = TRUE, size = "l"
  #     , footer = modalButton("OK")
  #     
  #   ))}
  })

# --------------- show data matrix df ---------------

output$data_matrix_df <- DT::renderDataTable({
  if(rv$demo_save == "yes" ){
    if(rv$run_mode == "auto")
      saveRDS(rv$samples, file = "rvs/samples.rds")
    else
      saveRDS(rv$samples, file = "rvs2/samples.rds")
  }
  req(is.null(rv$gse_all)==F || rv$run_mode == "manual")
  req(is.null(rv$plat_id)==F || rv$run_mode == "manual")
  req(is.null(rv$dmdf)==F)
  req(nchar(input$dmdf_filter))
  
  df <- rv$dmdf
  
  # filter according to stored sample list
  if (input$dmdf_filter == "Filtered"){
    if(rv$column_match_error == FALSE){
      df <- filtered_data_showdf()
    } else {
      # when there is match column error, return a null df
      df <- NULL
    }
  }
  
  # translate GSM column names to sample names on display
  if (input$dmdf_show_coln == "Sample name" 
        && rv$run_mode == "auto"){

      colnames(df) <- translate_sample_names(colnames(df),  # translating from
                                           rv$pdata[c("title", "geo_accession")],  # translation df
                                           "title") # translating to
  }
  
  # print(head(df))
  df
  
}, plugins="ellipsis",
options=dt_options(30, scrollX=T)
)

# select whether to filter
output$dmdf_filter_ui <- renderUI({
  req(length(rv$all_samples)>0 || !is.null(rv$dmdf_samples))

  if(rv$run_mode == "manual"){
    fm <- paste0("Full matrix (",length(rv$dmdf_samples),")")
    fl <- paste0("Filtered (",length(rv$samples),")")
  } else {
    fm <- paste0("Full matrix (",length(rv$all_samples),")")
    fl <- paste0("Filtered (",length(rv$samples),")")
  }
  
  choices <- c("Full matrix", "Filtered")
  names(choices) <- c(fm, fl)
  box(title=NULL, width = 6, solidHeader=T, status="primary",
      radioGroupButtons(
        inputId = "dmdf_filter",
        label = "Filtering options:",
        choices = choices,
        selected = "Filtered"
        #the next line is for demo section Version 1
        #selected = "Full matrix"
      ),
  )
})

# filter count matrix by selected samples (DISPLAY ONLY)
filtered_data_showdf <- reactive({
  
  req(is.null(rv$dmdf)==F)
  req(nrow(rv$dmdf)>0)
  req(length(rv$samples)>0)
  
  dmdf <- rv$dmdf

  if(rv$run_mode == "auto"){
    dmdf <- dmdf[,c("Name", rv$samples)]
  } else {
        # check if there are samples that at in design matrix but not in data matrix
    if(length(setdiff(rv$samples, rv$dmdf_samples))>0){
      
    }else{
      dmdf <- dmdf[,c("Name", rv$samples)]
    }
  }

  dmdf
  
})


# filter count matrix by selected samples (USE FOR ANALYSIS)
filtered_data_df <- reactive({
  req(is.null(rv$dmdf)==F)
  req(nrow(rv$dmdf)>0)
  req(length(rv$samples)>0)

  dmdf <- rv$dmdf
  # check if there are samples that exist in design matrix but not in data matrix
  # if yes, there is a column match error while displaying the filter data matrix
  if(rv$run_mode == "auto"){
    dmdf <- dmdf[,c("Name", rv$samples)]
  } else {
    if(length(setdiff(rv$samples, rv$dmdf_samples))>0){
      rv$column_match_error = TRUE
    }else{
      dmdf <- dmdf[,c("Name", rv$samples)]
      rv$column_match_error = FALSE
    }
  }
  dmdf[dmdf==""]<-NA # replace empty string with na
  dmdf <- dmdf[complete.cases(dmdf),] # get rid of na values
  
  dmdf
})

# --------- change identifiers according to GPL info --------------
observeEvent(input$identifier,{
  dmdf <- rv$dmdf_o
  
  # rename dmdf gene identifiers according to selection
  identifier = isolate(input$identifier)
  if(identifier != "Default"){
    genes <- rownames(rv$identifiers_df)
    df <- rv$identifiers_df[,identifier,drop=F] %>%
      dplyr::mutate(NameN = genes) %>%
      dplyr::filter(complete.cases(.)) %>%
      dplyr::filter(.data[[identifier]] != "")

    df <- df[!duplicated(df[[identifier]]),]
    
    # dmdf <- dmdf[dmdf[,"Name"] %in% df[["NameN"]],]
    dmdf <- dmdf[match(df[["NameN"]],dmdf[,"Name"]),] %>%
      as.data.frame()
    dmdf$Name <- df[[identifier]]
  }
  
  rv$dmdf <- dmdf
})

# ---------- guide box to 3. design matrix page -------
output$guide_box2 <- renderUI({
  if(rv$demo == ""){
    req(filtered_data_df())
  }
  
  column(12,
         guide_box("guide2", "Navigate to <b>3. Filter/review design matrix</b> to proceed"),
         br()
  )
})

observeEvent(input$guide2,{
  updateTabItems(session, "menu1", "tab2")
})
# ------------- the confirm and reset buttons function added ---------------------
# observeEvent(input$wrong_format_confirm, {
#   confirm_and_jump()
# })
observeEvent(input$wrong_format_reset, {
  shinyjs::reset("file")
  removeModal()
})
observeEvent(input$duplicated_reset, {
  shinyjs::reset("file")
  removeModal()
})
observeEvent(input$duplicated_proceed, {
  removeModal()
})
observeEvent(input$wrong_format_proceed, {
  removeModal()
})