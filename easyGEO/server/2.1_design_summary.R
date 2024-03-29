# --------------- overall design tab UI -----------------
output$ui_design <- renderUI({
  if(is.null(rv$plat_id) && rv$run_mode == "auto"){
    panel_null()
  }else if(is.null(rv$fddf_o) && rv$run_mode == "manual"){
    panel_null(text = "Data available upon successfully uploading your design matrix.")
  }else{
    div(
      column(12,
             guide_box("guide3", "Navigate to <b>4. Run DE analysis</b> to proceed"),
             br()
      ),
      fluidRow(
        column(8,
               
               
               box(title=span(HTML("<b>3.1.</b>"),icon("pencil-ruler"),HTML("Filter data if needed")), width = 12, solidHeader=F, status = "primary", 
                   uiOutput("filter_design_ui"),
               ),
               # tabBox(
               #   title = NULL, width = 12,
               #   id = "filter",
               #   
               #   tabPanel("Filter Data", 
               #            
               #            uiOutput("filter_design_ui"),
               #   ),
               #   
               #   tabPanel("Study Design Summary",
               #            
               #            uiOutput("design_summary_ui")
               #            
               #   )
               # ),
               
               
               
               
               # box(title=span(icon("microscope"),"Design Matrix"), width = 12, solidHeader=F, status = "primary", 
               #     id = "filtered design matrix",
               #     
               #     fluidRow(
               #       column(12,
               #              box(title=NULL, width = 6, solidHeader=T, status="primary",
               #                  radioGroupButtons(
               #                    inputId = "fddf_show_rown",
               #                    label = "Show column names as:", 
               #                    choices = c("GEO accession", "Sample name"),
               #                    selected = "Sample name"
               #                  )
               #              ),
               #              
               #              DT::dataTableOutput("filtered_design_df")
               #              
               #       )
               #       
               #     )
               #     
               # )
               
        ),
        column(4,
               valueBoxOutput("design_variables", width=12),
               valueBoxOutput("design_samples", width=12),
               
               # tabBox(
               #     title = NULL, width = 12,
               #     id = "design_vis",
               #     
               #     
               #     tabPanel("Visualization", 
               #              
               #              "Some categorical heatmap/ sunburst visualization here"
               #     )
               # ),
               
        )
        
      ),
      fluidRow(
        column(12,
               tabBox(
                 title = uiOutput("download_fddf_button"), width = 12,
                 
                 tabPanel(span(HTML("<b>3.2.</b>"),icon("microscope"),HTML("Review design matrix")),
                          
                          fluidRow(
                            column(12,
                                   if(rv$run_mode == "auto"){
                                     box(title=NULL, width = 6, solidHeader=T, status="primary",
                                         radioGroupButtons(
                                           inputId = "fddf_show_rown",
                                           label = "Show row names as:", 
                                           choices = c("GEO accession", "Sample name"),
                                           selected = "Sample name"
                                         )
                                     )
                                    },
                                   
                                   DT::dataTableOutput("filtered_design_df")
                                   
                            )
                            
                          )
                 ),
                 
                 tabPanel(span(HTML("<b>3.3.</b>"),icon("list"),HTML("Review design summary")),
                          
                          uiOutput("design_summary_ui")
                          
                 )
               ),
        )
      )
    )
    
  }
})

# the button to download rv$fddf, the design matrix
output$download_fddf_button <- renderUI({
  req(!is.null(rv$fddf))
  req(nrow(rv$fddf) >= 1)

  div(
    id = "download_fd", style = "display: inline-block; position: absolute; right: 0px; top: 4px;",
    downloadBttn(
      size = "md", style = "unite",
      outputId = "download_fddf", label = NULL
    ),
    bsTooltip("download_fd", "Download the design matrix"
              ,placement = "top")
  )
})

output$download_fddf <- downloadHandler(
  filename = function(){paste0(rv$geo_accession,"_design_matrix.csv")},
  content = function(file){
    if (input$fddf_show_rown == "Sample name" 
        && rv$run_mode == "auto"){
      # the downloaded df that with sample names
      df_download <- rv$fddf
      rownames(df_download) <- translate_sample_names(rownames(df_download),  # translating from
                                                      rv$pdata[c("title", "geo_accession")],  # translation df
                                                      "title") # translating to
      fwrite(df_download, file, row.names = T)
    } else {
      fwrite(rv$fddf, file, row.names = T)
    }
  }
)

# ----------- guide box to 4. run DE page ---------
observeEvent(input$guide3,{
  updateTabItems(session, "menu1", "tab4")
})

# get full design matrix table -----------#

design_df <- reactive({
  if(rv$run_mode == "auto"){
    req(!is.null(rv$pdata))
    # req(is.null(rv$gse_all)==F)
    # req(is.null(gse())==F)
    # req(is.null(rv$plat_id)==F)
    
    if (study_type()$channel_count == 1){ 
      charKeywords = c("characteristics")
      
      # detect how many char columns there are; if only 1, try to parse differently
      gse_chars <- data.frame(t(data.frame(rv$pdata) %>% dplyr::select(contains(charKeywords))))
      detected_var_num <- nrow(gse_chars) #pData(phenoData(gse()))
      # print(paste0("detected characteristics columns: ", detected_var_num))
      if (detected_var_num>1){
        char_list <- extract_char_list(oneline_guard=F, keyword=charKeywords) #gse(), 
      } else {
        char_list <- extract_char_list(oneline_guard=T, keyword=charKeywords) #gse(), 
      }
      
      char_mat <- char_mat_from_list(char_list)
    } else { 
      # retrieve design df directly for multi-channel datasets
      charKeywords = c("characteristics","source_name","label_ch", "growth_protocol")
      gse_chars <- data.frame(rv$pdata) %>% dplyr::select(contains(charKeywords))
      rowNa <- rownames(gse_chars)
      gse_chars <- as.data.frame(lapply(gse_chars, function(x){
        x <- as.factor(unlist(x))
        x
      }))
      rownames(gse_chars) <- rowNa
      char_mat <- gse_chars
    }

  } else{
    char_mat <- rv$fddf_o
  }
  # print(head(char_mat))
  char_mat
  
  # # tidy characteristics
  # char_list <- data.frame(t(data.frame(pData(phenoData(gse()))) %>% dplyr::select(contains("characteristics"))))
  # char_list[char_list==""] <- NA
  # char_list <- as.list(char_list)
  # # print(char_list)
  # 
  # # map list of characters into dataframe format (those not found = NA)
  # char_list <- lapply(char_list, function(x){
  #   transform_vector(x, ": ")
  # })
  # # print(char_list)
  # 
  # 
  # # char_list
  # chars <- names(table(unlist(lapply(char_list, names))))
  # # chars
  # ls <- lapply(char_list,function(x){
  #   xx<- rep(NA, length(chars))
  #   names(xx) <- chars
  #   xx[names(x)] <- x
  #   xx
  # })
  # # ls
  # char_mat <- data.frame(t(data.frame(ls)))
  # 
  # # # get rid of single factor columns # well... we still need them to show user
  # # to_keep <- function(x) any(is.numeric(x), length(unique(x)) > 1)
  # # char_mat <- Filter(to_keep, char_mat)
  # 
  # 
  # # fill NAs with string?? (optional)
  # char_mat[is.na(char_mat)] <- "N/A"
  # char_mat[char_mat=="NA"] <- "N/A"
  # 
  # # convert cols type. currently, all is converted to factor
  # # in the future: integers >> numeric, char >> factor
  # char_mat[] <- lapply(char_mat, function(x) {
  #   # if(is.integer(x) | is.numeric(x)) {
  #   #     as.numeric(x) 
  #   # } else {
  #   as.factor(x)
  #   # }
  # })
  # char_mat 
})


# summarize design -----------#

# variable summary (is a named list of named vectors in form of $var level:freq)
var_summary <- reactive({
  if(rv$run_mode == "manual"){
    req(!is.null(rv$fddf_o))
  }
  char_mat <- design_df()
  
  # get named list of named vectors
  var_summary <- vector(mode="list", length=ncol(char_mat))
  for (i in 1: length(colnames(char_mat))){
    var_summary[[i]] <- table(char_mat[[colnames(char_mat)[[i]]]])
  }
  names(var_summary) <- colnames(char_mat)
  var_summary
})

# --------------- textual summary for design ---------------

# construct the text summary from variable summary
design_summary <- reactive({
  req(nrow(rv$fddf)>0)
  
  # var_summary <- var_summary()
  
  fddf <- rv$fddf
  var_summary <- lapply(fddf, table)
  
  # get text
  textt <- vector(mode="list", length=length(var_summary))
  for (i in 1: length(var_summary)){
    textt[[i]] <- paste0(
      "<strong>", names(var_summary)[[i]], ":</strong><br>", 
      paste(
        paste(names(var_summary[[i]]), " (", var_summary[[i]], ")", sep="")
        , collapse=", ")
    )
  }
  paste(textt, collapse="<br><br>")
})


output$design_summary_ui <- renderUI({
  if(rv$run_mode == "auto"){
    req(is.null(rv$gse_all)==F)
    req(is.null(rv$plat_id)==F)
  
  div(
    strong(paste0("Study design for ", rv$platform, ": ")), br(),br(),
    
    
    shiny::HTML(design_summary())
    
    
  )} else {
    div(shiny::HTML(design_summary()))
  }
})






# show summary valueboxes
output$design_variables <- renderValueBox({
  # if(rv$run_mode == "auto"){
    req(is.null(design_df())==F)
  
  
  valueBox(
    paste0(ncol(design_df()), " variables"), 
    paste0(paste(colnames(design_df()), collapse=", ")), 
    icon = icon("microscope"),
    color = "blue"
  )
})
output$design_samples <- renderValueBox({
  req(is.null(filtered_design_df())==F)
  # if(rv$run_mode == "auto") {
    req(is.null(design_df())==F)
  
  
  selected <- nrow(filtered_design_df())
  total <- nrow(design_df())
  valueBox(
    paste0(selected, "/", total, " samples"), 
    HTML(paste0("Selected: ", selected, " samples <br>", 
                "Total: ", total, " samples")), 
    icon = icon("seedling"),
    color = "purple"
  )
})