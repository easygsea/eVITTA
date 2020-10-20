# trigger the corresponding intro tour while pressing the help button
observeEvent(input$help_1_pre, {
  req(input$menu1 == "tab1")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$E_pre)
  )
})
observeEvent(input$help_1_post, {
  req(input$menu1 == "tab1")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$E_post)
  )
})

# trigger the corresponding intro tour on the data matrix tab
observeEvent(input$help_3_pre, {
  req(input$menu1 == "tab3")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$D_pre)
  )
})
observeEvent(input$help_3_post, {
  req(input$menu1 == "tab3")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$D_post)
  )
})

# trigger the corresponding help tour on the filter matrix tab
observeEvent(input$help_2_pre, {
  req(input$menu1 == "tab2")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$F_pre)
  )
})
observeEvent(input$help_2_post, {
  req(input$menu1 == "tab2")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$F_post)
  )
})

# trigger the corresponding help tour on the Run DEG tab
observeEvent(input$help_4_pre, {
  req(input$menu1 == "tab4")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$R_pre)
  )
})
observeEvent(input$help_4_post, {
  req(input$menu1 == "tab4")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$R_post)
  )
})
# intro tour after running deg analysis
observeEvent(input$help_4_post_deg, {
  req(input$menu1 == "tab4")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$R_post_deg)
  )
})

# trigger the corresponding help tour on the visualization tab
observeEvent(input$help_5_pre, {
  req(input$menu1 == "tab5")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$V_pre)
  )
})
observeEvent(input$help_5_post, {
  req(input$menu1 == "tab5")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$V_post)
  )
})

#introjs help button on the first tab
output$floating_button_1 <- renderUI({
  if (is.null(rv$geo_accession)==T){
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_1_pre", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  } else {
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_1_post", label=NULL,
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  }
})

#introjs button on the filtered matrix tab
output$floating_button_2 <- renderUI({
  if (is.null(rv$dmdf)){
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_2_pre", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  } else {
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_2_post", label=NULL,
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  }
})

#introjs help button on the data matrix tab
output$floating_button_3 <- renderUI({
  if (is.null(rv$fddf)==T){
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_3_pre", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  } else {
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_3_post", label=NULL,
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  }
})

#introjs help button on the fourth tab
output$floating_button_4 <- renderUI({
  if(is.null(rv$deg)){
    if (is.null(rv$matrix_ready)==T || rv$matrix_ready == F){
      div(style="margin-top:10px",
          actionBttn(
            inputId = "help_4_pre", label=NULL, 
            icon = icon("question"), style="material-circle", color="primary", size="lg"
          )
      )
    } else {
      div(style="margin-top:10px",
          actionBttn(
            inputId = "help_4_post", label=NULL,
            icon = icon("question"), style="material-circle", color="primary", size="lg"
          )
      )
    }
  } else{
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_4_post_deg", label=NULL,
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  }
  
})

#introjs help button on the third tab
output$floating_button_5 <- renderUI({
  if (is.null(rv$deg)){
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_5_pre", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  } else {
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_5_post", label=NULL,
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  }
})