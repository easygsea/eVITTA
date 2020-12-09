# display choices for numeric IDs
# --------- numeric ID selection ----------
r_num_acc <- reactive({
  div(
    selectizeInput(
      "num_acc",
      HTML(paste0("Numeric IDs treated as:",add_help("num_acc_hp", style="padding:3px 0 0 0;position:absolute;right:1em;"))),
      choices = num_space[input$selected_species][[1]],
      selected = num_space[input$selected_species][[1]][grepl('ENTREZGENE',num_space[input$selected_species][[1]])]
    )
    ,bsTooltip("num_acc_hp","The identifier for fully numeric IDs"
               ,placement = "top")
  )
})

# display databases for selection
# ------- databases for selection ----------
dbs <- reactive({
  if(input$selected_species != "other"){
    return(rv$dbs)
  }else{
    return(rv$gmt_cs)
  }
})