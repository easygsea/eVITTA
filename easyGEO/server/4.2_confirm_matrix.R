output$confirm_matrix_ui <- renderUI({
  
  div(
    uiOutput("confirm_matrix_feedback"),
    radioButtons(
      inputId = "data_type",
      label = "Select the type of data provided by the authors:",
      choices = c("Raw counts"="raw", "Normalized counts"="normalized"),
      inline=T
    )
  )
  
})

output$confirm_matrix_feedback <- renderUI({
  req(is.null(rv$dmdf)==F)
  
  errors = 0
  msg = vector()
  dmdf <- rv$dmdf
  
  # check if count matrix is empty
  if (nrow(dmdf)==0){
    errors <- errors + 1
    msg = c(msg, "<strong>Data matrix is empty. </strong><br>
            Please upload data matrix in the <b>2. Data Matrix</b> page.")
    
  } else {
        
    # check if columns are present in dmdf for the selected samples
    notfound <- setdiff(rv$samples, intersect(colnames(dmdf),rv$samples))
    if (length(notfound)>0){
      errors <- errors + 1
      
      # titles of not found samples
      samples_title = translate_sample_names(notfound,rv$pdata[c("title", "geo_accession")],  "title")
      
      if(length(notfound)==1){d_msg = "it"}else{d_msg = "them"}
      
      msg = c(msg, paste0("<strong>Selected samples are missing from data matrix: </strong><br>
            ", paste(notfound, collapse=", "), " (", paste(samples_title, collapse=", ") ,")"))
      
    } else {
      dmdf[dmdf==""]<-NA # replace empty string with na
      # check if the selected samples all have data in the count matrix
      checkdf <- dmdf[,rv$samples,drop=FALSE]
      # print(head(checkdf))
      # check which cols have some nas
      indxx <- apply(checkdf, 2, function(x) {any(is.na(x))})
      # print(indxx)
      has_nas <- names(indxx[indxx==T])
      # print(has_nas)
      # check which col has all nas
      indx = apply(checkdf, 2, function(x) {all(is.na(x))})
      all_nas <- names(indx[indx==T])
      # print(all_nas)
      if (length(all_nas)>0){
        errors <- errors + 1
        
        # titles of all na samples
        samples_title = translate_sample_names(all_nas,rv$pdata[c("title", "geo_accession")],  "title")

        msg = c(msg, paste0("<strong>Data are missing for selected samples: </strong><br>
                            ", paste(all_nas, collapse=", "), " (", paste(samples_title, collapse=", ") ,")"
                            ,"<br><br><b>Please check if your uploaded data matrix is in correct format and is complete.</b>"))
        
      } else if (length(has_nas)>0){
        # titles of has na samples
        samples_title = translate_sample_names(has_nas,rv$pdata[c("title", "geo_accession")],  "title")
        
        if(length(has_nas)==1){d_msg = "it"}else{d_msg = "them"}
        
        msg = c(msg, paste0("<strong>Missing value(s) found in selected samples: </strong><br>
                            ", paste(has_nas, collapse=", "), " (", paste(samples_title, collapse=", ") ,")",
                            "<br><strong>Values missing in rows 
                            ", paste(rownames(checkdf[!complete.cases(checkdf),]), collapse=", ")
                            ,".<br><br><b>These rows will be omitted.</b>"))
        
        # check if the final dmdf has any rows left
        dmdf <- dmdf[complete.cases(dmdf),] # get rid of na values
        if (nrow(dmdf)==0) {
          errors <- errors+1
          msg = c(msg, paste0("<strong>No complete rows in data matrix. </strong>"))
        }
        
      }
      
    }
  }
  
  
  
  # what to show
  if(errors > 0){ 
    box_color = "red"
    msg <- paste(msg, collapse="<br><br>")
    rv$matrix_ready = F
  } else if (length(has_nas)>0){
    box_color = "yellow"
    msg <- paste(msg, collapse="<br><br>")
    rv$matrix_ready = T
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

