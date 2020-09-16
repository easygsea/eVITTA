# --------------- overall design tab UI -----------------
output$ui_design <- renderUI({
  if(is.null(rv$plat_id)){
    panel_null()
  }else{
    fluidRow(
      
      column(8,
             
             
             tabBox(
               title = NULL, width = 12,
               id = "filter",
               
               tabPanel("Filter Data", 
                        
                        uiOutput("filter_design_ui"),
               ),
               
               tabPanel("Study Design Summary",
                        
                        uiOutput("design_summary_ui")
                        
               )
             ),
             box(title=span(icon("microscope"),"Filtered Design Matrix"), width = 12, solidHeader=F, status = "primary", 
                 id = "filtered design matrix",
                 
                 fluidRow(
                   column(12,
                          box(title=NULL, width = 6, solidHeader=T, status="primary",
                              radioGroupButtons(
                                inputId = "fddf_show_rown",
                                label = "Show column names as:", 
                                choices = c("GEO accession", "Sample name"),
                                selected = "GEO accession"
                              )
                          ),
                          
                          DT::dataTableOutput("filtered_design_df")
                          
                   )
                   
                 )
                 
             )
             
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
      
    )
  }
})

# get full design matrix table -----------#

design_df <- reactive({
  req(is.null(rv$gse_all)==F)
  req(is.null(gse())==F)
  req(is.null(rv$plat_id)==F)
  
  # tidy characteristics
  char_list <- data.frame(t(data.frame(pData(phenoData(gse()))) %>% dplyr::select(contains("characteristics"))))
  char_list[char_list==""] <- NA
  char_list <- as.list(char_list)
  # print(char_list)
  
  # map list of characters into dataframe format (those not found = NA)
  char_list <- lapply(char_list, function(x){
    transform_vector(x, ": ")
  })
  # print(char_list)
  
  
  # char_list
  chars <- names(table(unlist(lapply(char_list, names))))
  # chars
  ls <- lapply(char_list,function(x){
    xx<- rep(NA, length(chars))
    names(xx) <- chars
    xx[names(x)] <- x
    xx
  })
  # ls
  char_mat <- data.frame(t(data.frame(ls)))
  
  # # get rid of single factor columns # well... we still need them to show user
  # to_keep <- function(x) any(is.numeric(x), length(unique(x)) > 1)
  # char_mat <- Filter(to_keep, char_mat)
  
  
  # fill NAs with string?? (optional)
  char_mat[is.na(char_mat)] <- "N/A"
  char_mat[char_mat=="NA"] <- "N/A"
  
  # convert cols type. currently, all is converted to factor
  # in the future: integers >> numeric, char >> factor
  char_mat[] <- lapply(char_mat, function(x) {
    # if(is.integer(x) | is.numeric(x)) {
    #     as.numeric(x) 
    # } else {
    as.factor(x)
    # }
  })
  char_mat 
})


# summarize design -----------#

# variable summary (is a named list of named vectors in form of $var level:freq)
var_summary <- reactive({
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
  
  var_summary <- var_summary()
  
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
  req(is.null(rv$gse_all)==F)
  req(is.null(rv$plat_id)==F)
  
  div(
    strong(paste0("Study design for ", annotation(gse()), ": ")), br(),br(),
    
    
    shiny::HTML(design_summary())
    
    
  )
})






# show summary valueboxes
output$design_variables <- renderValueBox({
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