
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

observeEvent(input$help_organize, {
  # print(input$tabs)
  req(input$tabs == "tab1")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$upload)
  )
})

observeEvent(input$help_x_pre, {
  # print(input$tabs)
  req(input$tabs == "tab2")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$x0
  ))
  
})

observeEvent(input$help_x_post, {
  # print(input$tabs)
  req(input$tabs == "tab2")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$x1
  ))
  
})

observeEvent(input$help_n_pre, {
  # print(input$tabs)
  req(input$tabs == "tab3")
  rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                            steps = intros$n0)
  )
  
})

observeEvent(input$help_n_post, {
  # print(input$tabs)
  req(input$tabs == "tab3")
  
  if (input$n_ui_showpanel == "Intersection"){
    rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                              steps = intros$n1)
    )
  } else if (input$n_ui_showpanel == "Heatmap"){
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


# initially hide the option panels using shinyjs, to prevent empty white boxes from flashing
observe({
  req(input$tabs != "tab1")
  shinyjs::show("sidebar_opt")
})
