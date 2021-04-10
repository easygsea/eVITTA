# # DEMO SESSION CODE -------------------------------------------------------
# 
# library(later)
# rv$demo_n = 2
# # the modal to remind the user it is a demo session
# observe({
#   req(rv$demo_n == 2)
#   init_demo()
#   showModal(modalDialog(title = tags$h3("Welcome to our easyVizR demo session"),
#                         tags$h4("Explore the sample output that performs interactively in the same way as real output.")
#                         ,br()
#                         ,tags$h4("Click OK to follow the intro tour."),
#                         size = "m",
#                         easyClose = FALSE
#                         ,footer = actionButton("welcome_modal",label = "OK")))
#   
#   rv$demo_n = 4
# 
# })
# # when the user closed the modal, start rintrojs
# observeEvent(input$welcome_modal, {
#   removeModal()
#   rv$demo_yes <- "yes"
#   rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
#                                             steps = intros$upload)
#   )
# })
# 
# # start rintrojs when users switch tabs
# observeEvent(input$tabs,{
#   if(input$tabs == "tab1" && !is.null(rv$demo_yes)){
#     later(~rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
#                                                      steps = intros$upload)
#     ), 2)
#   } else if(input$tabs == "tab_filters"){
#     later(~rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
#                                                      steps = intros$f1)
#     ), 2)
#   } else if(input$tabs == "tab_ins"){
#     later(~rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
#                                                      steps = intros$i1)
#     ), 2)
#   } else if(input$tabs == "tab3"){
#     later(~rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
#                                                      steps = intros$n2)
#     ), 2)
#   } else {
# 
#   }
# })
# # END ----------------------------------------------------------------------
