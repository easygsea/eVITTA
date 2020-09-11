# --------------- search GEO accession ---------------
# currently checks if input exists
# todo: check if input format is correct (GSEXXXXXX)

output$geo_accession_ui <- renderUI({
  div(
    textInput(
      inputId = "geo_accession",
      label = "GSE Accession Number:", 
      value = "GSE147507",
      placeholder = "GSE137355",
      width = "100%"
    ),
    "Note: only RNA-seq and single_channel microarray datasets are currently accepted.", br(), br(),
    
    uiOutput("search_geo")
    
  )
})

output$search_geo <- renderUI({
  if (nchar(input$geo_accession)>0){
    actionButton("search_geo", "Search")
  } else {
    "Enter a valid GEO accession number."
  }
  
})

# --------------- get GEO matrix ---------------
# this takes several mins for larger datasets; need more feedback?

observeEvent(input$search_geo, {
  
  rv$gse_all <- NULL
  rv$geo_accession <- NULL
  rv$platforms <- NULL
  rv$plat_id <- NULL
  
  withProgress(message = 'Getting data. Please wait a minute...', value = 1, {
    
    rv$geo_accession <- isolate(input$geo_accession)
    rv$gse_all <- getGEO(input$geo_accession, GSEMatrix=T) 
    
    rv$platforms <- tabulate(rv$gse_all, annotation)
    
    
  })
  
})

# --------------- select the desired platform ----------------
# Sometimes multiple platforms are in the GSE, thus the GSE object is a list with length >1
# we need to make people choose one

output$geo_platform_ui <- renderUI({
  req(is.null(rv$gse_all)==F)
  req(length(rv$platforms)>0)
  
  div(
    radioButtons(
      inputId = "plat",
      label = "Platforms available:",
      choices = rv$platforms
    ),
    uiOutput("study_type_feedback"),
    uiOutput("select_geo_platform")
    
  )
  
})

# detect study type during platform selection
study_type <- reactive({
  req(is.null(rv$gse_all)==F)
  req(length(rv$platforms)>0)
  req(is.null(input$plat)==F)
  
  gse_temp <- rv$gse_all[[match(input$plat, rv$platforms) ]]
  print(gse_temp)
  
  # get channel count
  gsm_meta_temp <- find_repeating_values(pData(phenoData(gse_temp)))
  channel_count <- gsm_meta_temp$channel_count
  
  # get study type
  gse_meta_temp <- notes(experimentData(gse_temp))
  type <- gse_meta_temp$type
  
  return(list("type" = type, "channel_count" = channel_count))
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
  if (channel_count != 1) {
    errors = errors +1
    msgs <- c(msgs, 
              paste0("<strong>CAUTION: </strong>Dataset has <strong>",channel_count, "</strong> channels, which is not currently supported.<br>
                                Only data in the first channel will be read.")
    )
    box_color = "yellow"
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
  
  # initialize the count matrix (even if it's empty) with first row = Name
  exprs <- exprs(gse()) 
  if (nrow(exprs) >0){ # put gene names onto first column
    dmdf <- cbind(Name = rownames(exprs), exprs) 
  } else {
    dmdf <- cbind(Name = character(0), exprs)
  }
  rownames(dmdf) <- c() # remove rownames
  rv$dmdf <- dmdf
  
  
  # initialize samples list
  rv$all_samples <- sampleNames(gse()) # this one stays the same
  rv$samples <- sampleNames(gse()) # this one will change
  
  # initialize pdata
  rv$pdata <- pData(phenoData(gse()))
  
  # initialize fddf
  rv$fddf <- design_df() # initially unfiltered, will update when filter
  
})

