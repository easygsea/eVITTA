output$confirm_matrix_ui <- renderUI({
  
  div(
    uiOutput("confirm_matrix_feedback"),
    radioButtons(
      inputId = "data_type",
      label = "Type of data provided:",
      choices = c("Raw counts"="raw", "Normalized counts"="normalized"),
      inline=T
    )
  )
  
})

output$confirm_matrix_feedback <- renderUI({
  req(is.null(rv$dmdf)==F)
  
  errors = 0
  msg = vector()
  
  # check if count matrix is empty
  if (nrow(rv$dmdf)==0){
    errors <- errors + 1
    msg = c(msg, "<strong>Data Matrix is empty. </strong><br>
            Please upload data matrix in the Data Matrix page.")
    
  } else {
    
    # check if columns are present in dmdf for the selected samples
    notfound <- setdiff(rv$samples, intersect(colnames(rv$dmdf),rv$samples))
    if (length(notfound)>0){
      errors <- errors + 1
      msg = c(msg, paste0("<strong>Selected samples are missing from data matrix: </strong><br>
            ", paste(notfound, collapse=", ")))
      
    } else {
      
      # check if the selected samples all have data in the count matrix
      checkdf <- rv$dmdf[,rv$samples]
      indx = apply(checkdf, 2, function(x) any(is.na(x)))
      has_nas <- names(indx[indx==T])
      if (length(has_nas)>0){
        errors <- errors + 1
        msg = c(msg, paste0("<strong>Data is incomplete for selected samples: </strong><br>
                            ", paste(has_nas, collapse=", ")))
        
      }
      
    }
  }
  
  
  # what to show
  if(errors > 0){ 
    box_color = "red"
    msg <- paste(msg, collapse="<br><br>")
    rv$matrix_ready = F
    
  } else {
    box_color = "green"
    msg <- "<strong>Data matrix ok!</strong><br>
            Count data is complete for selected samples."
    rv$matrix_ready = T
  }
  
  fluidRow(
    box(
      title = NULL, background = box_color, solidHeader = TRUE, width=12,
      HTML(msg)
    )
  )
  
  
})

