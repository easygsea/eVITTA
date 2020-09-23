
####---------------------- HELP ---------------------------####
# 
# an intro tour is written conditionally for each page pre- and post- 
# clicking "visualize".
#
# upload = organize files page
# x = single dataset page
# n = multiple datasets page
# 
# each page may have multiple intro tours according to conditions, 
# e.g. before and after clicking visualize, or individual subtabs.
# these are named 0 (pre-visualize), 1, 2, 3 (subtabs)...
#
# intro text are loaded from tab-delimited txt in /intro.
# 

# ----------------- organize files tab
observeEvent(input$help_organize, {
  req(input$tabs == "tab1")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$upload)
  )
})

# ----------------- filters tab
# before selecting datasets
observeEvent(input$help_f_pre, {
  req(input$tabs == "tab_filters")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$f0)
  )
})

# after selecting datasets
observeEvent(input$help_f_post, {
  req(input$tabs == "tab_filters")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$f1)
  )
})

# ----------------- intersection tab
# before selecting datasets
observeEvent(input$help_i_pre, {
  req(input$tabs == "tab_ins")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$i0)
  )
})

# after selecting datasets
observeEvent(input$help_i_post, {
  req(input$tabs == "tab_ins")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$i1)
  )
})

# ----------------- n visualization tab
# before selecting datasets
observeEvent(input$help_n_pre, {
  # print(input$tabs)
  req(input$tabs == "tab3")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$n0)
  )
})

# after selecting datasets
observeEvent(input$help_n_post, {
  # print(input$tabs)
  req(input$tabs == "tab3")
  
  if (input$n_ui_showpanel == "Heatmap"){
    # updateRadioGroupButtons(session, inputId = "n_ui_showpanel", selected="Intersection")
    rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                              steps = intros$n2)
    )
  } else if (input$n_ui_showpanel == "Scatter"){
    # updateRadioGroupButtons(session, inputId = "n_ui_showpanel", selected="Intersection")
    rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                              steps = intros$n3)
    )
  } else if (input$n_ui_showpanel == "Single"){
    # updateRadioGroupButtons(session, inputId = "n_ui_showpanel", selected="Intersection")
    rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                              steps = intros$n_single)
    )
  } else if (input$n_ui_showpanel == "Network"){
    # updateRadioGroupButtons(session, inputId = "n_ui_showpanel", selected="Intersection")
    rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                              steps = intros$n_nw)
    )
  }
  
  
})





#======================================================================#
####                help buttons UI in each page                    ####
#======================================================================#
# filter page
output$f_floating_buttons <- renderUI({
  if (is.null(rv$df_n)==T){
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_f_pre", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  } else {
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_f_post", label=NULL,
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  }
})

# intersection page
output$i_floating_buttons <- renderUI({
  if (is.null(rv$df_n)==T){
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_i_pre", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  } else {
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_i_post", label=NULL,
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  }
})

# n page
output$n_floating_buttons <- renderUI({
  
  if (is.null(rv$df_n)==T){
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_n_pre", label=NULL, 
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  } else {
    div(style="margin-top:10px",
        actionBttn(
          inputId = "help_n_post", label=NULL,
          icon = icon("question"), style="material-circle", color="primary", size="lg"
        )
    )
  }
})
