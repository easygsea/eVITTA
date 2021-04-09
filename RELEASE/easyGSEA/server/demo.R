# # DEMO SESSION code -------------------------------------------------------
# 
# library(later)
# # the modal to remind the user it is a demo session
# observe({
#   init_demo_gsea()
#   #init_demo_ora()
#   showModal(modalDialog(title = tags$h3("Welcome to easyGSEA demo session"),
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
#   if(rv$demo_mode == "gsea"){
#     call_introjs(rbind(intros$R_pre,intros$R_post_with_conversion_table,intros$R_post))
#   }else {
#     call_introjs(rbind(intros$R_pre_ora,intros$R_post_with_conversion_table_ora,intros$R_post_ora))
#   }
# })
# 
# # start rintrojs when users switch tabs
# observeEvent(input$tabs,{
#   if(input$tabs == "kegg"){
#     later(~call_introjs(rbind(intros$ER_post,intros$ER_post_with_pathway)), 0.1)
#   } else if(input$tabs == "network"){
#     later(~call_introjs(intros$EN_post), 3)
#   } else if(input$tabs == "download"){
#     later(~call_introjs(intros$D_post), 2)
#   } else {
# 
#   }
# })
# # when user switch tabs, call introjs
# observeEvent(input$plot_type, {
#   if(input$plot_type != "bar")
#     later(~call_introjs(rbind(intros$ER_post,intros$ER_post_with_pathway)), 0.1)
# })
# # END--------------------------------------------------------------------------------