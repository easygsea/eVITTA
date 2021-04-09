# # DEMO SESSION CODE -------------------------------------------------------
# library(later)
# # the modal to remind the user it is a demo session
# observe({
#   # updateRadioButtons(session, inputId = "selected_mode", selected = "manual")
#   # To activate the manual uploads demo session, change the default value of rv$run_mode
#   # and the default value of input$selected_mode to "manual".
#   init_demo()
#   showModal(modalDialog(title = tags$h3("Welcome to our easyGEO demo session"),
#                         tags$h4("Explore the sample output that performs interactively in the same way as real output.")
#                         ,br()
#                         ,tags$h4("Click OK to follow the intro tour."),
#                         size = "m",
#                         easyClose = FALSE
#                         ,footer = actionButton("welcome_modal",label = "OK")))
# 
# })
# # when the user closed the modal, start rintrojs
# observeEvent(input$welcome_modal, {
#   removeModal()
#   rv$demo_yes <- "yes"
#   if(rv$run_mode == "auto")
#     call_introjs(rbind(intros$E_pre,intros$E_post,intros$E_post_with_summary_ui))
#   else
#     call_introjs(intros$E_manual)
#   #print(input$menu1)
# 
# })
# 
# # start rintrojs when users switch tabs
# observeEvent(input$menu1,{
#   if(input$menu1 == "tab1" && !is.null(rv$demo_yes)){
#     if(rv$run_mode == "auto")
#       later::later(~call_introjs(rbind(intros$E_pre,intros$E_post,intros$E_post_with_summary_ui)), delay = 2)
#     else
#       later::later(~call_introjs(intros$E_manual), delay = 2)
#   } else if(input$menu1 == "tab3"){
#     if(rv$run_mode == "auto")
#       later::later(~call_introjs(intros$D_post), delay = 2)
#     else
#       later::later(~call_introjs(intros$D_manual), delay = 2)
#   } else if(input$menu1 == "tab2"){
#     later(~call_introjs(intros$F_post),2)
#   } else if(input$menu1 == "tab4"){
#     later(~call_introjs(rbind(intros$R_post,intros$R_post_deg)),2)
#   } else if(input$menu1 == "tab5"){
#     later(~call_introjs(intros$V_volcano), 0.1)
#   } else {
# 
#   }
# })
# # when user select different plots, triggering different introjs
# observeEvent(input$visDEG, {
#   if(input$visDEG == "heatmap"){
#     later(~call_introjs(intros$V_heatmap), 0.1)
#   } else if(input$visDEG == "gene"){
#     later(~call_introjs(intros$V_explore_violin), 0.1)
#   } else {
# 
#   }
# })
# # when user click the boxplot next to the violin plot, trigger an introjs
# observeEvent(input$a_type, {
#   if(input$a_type == "box"){
#     call_introjs(intros$V_explore)
#   }
# })
# # END-----------------------------------------------------------------------------