# --------------- overall run tab UI ---------------
output$ui_run <- renderUI({
  if(is.null(rv$plat_id) && rv$run_mode == "auto"){
    panel_null()
  }else if((is.null(rv$fddf_o) || is.null(rv$dmdf)) && rv$run_mode == "manual"){
    panel_null(text = "Data available upon successfully uploading your data matrix and design matrix.")
  }else{
    fluidRow(
      column(5,
             box(
               title=span(HTML("<b>4.1.</b>"),icon("check-square"),HTML("Confirm data matrix")), width = 12, status = "primary",
               id = "sp",
               # tabPanel(
               # span(icon("clipboard-check"),"Check if data matrix is ready"),
               uiOutput("confirm_matrix_ui")
               # )
             ),
             br(),
             box(
               title = span(HTML("<b>4.2.</b>"),icon("mixer"),HTML("Make contrast")), width = 12, status = "primary",
               id = "ui_select_box",
               radioGroupButtons(
                 inputId = "ui_select",
                 # label = "Select plot type",
                 choiceNames = c("By design matrix", "Manual selection"),
                 choiceValues = c("sp","coerce"),
                 selected = "sp",
                 checkIcon = list(
                   yes = icon("check-square"),
                   no = icon("square-o")
                 ),
                 # status = "primary",
                 direction = "horizontal"
               ),
               # bsTooltip("ui_select", HTML("<b>By design matrix</b> is applicable when the authors have uploaded their study design in full.<br><br><b>Manual selection</b> is for any combination of samples. You may manually select samples in the control and the experimental groups.")
               #           ,placement = "right"),
               
               # tabPanel(
               #   value = "sp",
               #   span(icon("check-square"),"By design matrix"),
               # HTML(paste0("By design matrix",add_help(
               #   "fine_q"
               # )))
               # ), 
               # bsTooltip("fine_q", "Applicable when design matrix is provided by authors and complete","top"),
               uiOutput("select_params_ui"),
               # ),
               # tabPanel(
               # value = "coerce",
               # span(icon("mixer"),"Manual"),
               # HTML(paste0("Manual selection",add_help(
               #   "coerce_q"
               # )))
               # ),
               # bsTooltip("coerce_q", "For any combination of samples","top"),
               # HTML("<b>Note:</b> \"Manual\" is for any combination of samples. You may manually select samples in the control and the experimental groups. 
               #       Select the comparisons you're interested in and run DEG analysis."),
               # hr(),
               
               uiOutput("coerce_ui")
               
               # )
             )
             
      ),
      column(7,
             column(
               width = 12,
               uiOutput("confirm_run"),
             ),
             
             fluidRow(
               column(
                 width = 12,
                 uiOutput("run_deg_ui")
                 
               )
               
             )
             
      )
    )
  }
})

output$confirm_matrix_ui <- renderUI({
  
  div(
    uiOutput("confirm_matrix_feedback"),
    radioButtons(
      inputId = "data_type",
      label = HTML("Select the type of expression data provided by the authors:",add_help("q_dtype")),
      choices = c("Raw counts"="raw", "Normalized counts"="normalized"),
      selected = "normalized",
      inline=T
    ),
    bsTooltip("q_dtype",HTML("<b>Microarray</b>: select normalized counts<br><br><b>RNA-seq</b>: select according to the data provided by the authors")
              ,placement = "right")
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
      if(rv$run_mode == "auto"){
        samples_title = translate_sample_names(notfound,rv$pdata[c("title", "geo_accession")],  "title")
      } else {
        samples_title = notfound
      }
      
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
        if(rv$run_mode == "auto"){
          samples_title = translate_sample_names(all_nas,rv$pdata[c("title", "geo_accession")],  "title")
        } else {
          samples_title = all_nas
        }
        msg = c(msg, paste0("<strong>Data are missing for selected samples: </strong><br>
                            ", paste(all_nas, collapse=", "), " (", paste(samples_title, collapse=", ") ,")"
                            ,"<br><br><b>Please check if your uploaded data matrix is in correct format and is complete.</b>"))
        
      } else if (length(has_nas)>0){
        # titles of has na samples
        if(rv$run_mode == "auto"){
          samples_title = translate_sample_names(has_nas,rv$pdata[c("title", "geo_accession")],  "title")
        } else {
          samples_title = has_nas
        }
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

