output$floating_button_tab1 <- renderUI({
  if (is.null(rv$data_head_o)==T){
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_tab1_pre", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  } else {
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_tab1_post", label=NULL,
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  }
})

output$floating_button_tab2 <- renderUI({
  if (is.null(rv$data_head_o)==T){
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_tab2_pre", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  } else {
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_tab2_post", label=NULL,
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  }
})

output$floating_button_tab3 <- renderUI({
  if (is.null(rv$data_head_o)==T){
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_tab3_pre", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  } else {
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_tab3_post", label=NULL,
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  }
})

output$floating_button_tab4 <- renderUI({
  if (is.null(rv$data_head_o)==T){
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_tab4_pre", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  } else {
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_tab4_post", label=NULL,
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  }
})
