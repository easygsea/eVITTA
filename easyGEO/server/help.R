# trigger the corresponding intro tour while pressing the help button
observeEvent(input$help_1_button, {
  if(rv$demo == "yes"){
    call_introjs(rbind(intros$E_pre,intros$E_post,intros$E_post_with_summary_ui))
  }else{
    if (is.null(rv$geo_accession)==T){
      call_introjs(intros$E_pre)
    } else {
      # check to see if the user have pressed the select to proceed button, 
      # if yes, then we call the function with highlighting summary ui
      if(is.null(rv$dmdf)){
        call_introjs(intros$E_post)
      } else{
        call_introjs(intros$E_post_with_summary_ui)
      }
    }
  }
  
})


# trigger the corresponding intro tour on the data matrix tab
observeEvent(input$help_3_button, {
  req(is.null(rv$fddf)==F)
  # if (is.null(rv$fddf)==T){
  #   call_introjs(intros$D_pre)
  # } else {
    call_introjs(intros$D_post)
  # }
})

# trigger the corresponding help tour on the filter matrix tab
observeEvent(input$help_2_button, {
  # if (is.null(rv$dmdf)){
  #   call_introjs(intros$F_pre)
  # } else {
    call_introjs(intros$F_post)
  # }
})

# trigger the corresponding help tour on the Run DEG tab
observeEvent(input$help_4_button, {
  #check if we have run the deg analysis
  if(is.null(rv$deg)){
    if (is.null(rv$matrix_ready)==T || rv$matrix_ready == F){
      call_introjs(intros$R_pre)
    } else {
      call_introjs(intros$R_post)
    }
  } else {
    call_introjs(intros$R_post_deg)
  }
  
})

# trigger the corresponding help tour on the visualization tab
observeEvent(input$help_5_button, {
  # if (is.null(rv$deg)){
  #   call_introjs(intros$V_pre)
  # } else {
    call_introjs(intros$V_post)
  # }
})


#introjs help button on the first tab
output$floating_button_1 <- renderUI({
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_1_button", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
})

#introjs button on the filtered matrix tab
output$floating_button_2 <- renderUI({
  req(is.null(rv$fddf)==F)
  
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_2_button", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
})

#introjs help button on the data matrix tab
output$floating_button_3 <- renderUI({
  req(is.null(rv$fddf)==F)
  
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_3_button", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
})

#introjs help button on the fourth tab
output$floating_button_4 <- renderUI({
  req(is.null(rv$fddf)==F)
  
      div(style="margin-top:10px",
          actionBttn(
            inputId = "help_4_button", label=NULL, 
            icon = icon("question"), style="material-circle", color="primary", size="lg"
          )
      )
})

#introjs help button on the third tab
output$floating_button_5 <- renderUI({
  req(is.null(rv$deg)==F)
  
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_5_button", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
})

# the function that call rintrojs
call_introjs <- function(file_name) {
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = file_name)
  )
}