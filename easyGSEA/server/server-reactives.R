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

# ------ color tones for ORA run --------
g_color <- function(col = rv$ora_color){
  # color tone, red or blue
  if(col == "red"){
    gcols2
  }else if(col == "blue"){
    gcols3
  }else if(col == "grey"){
    gcols_grey
  }else if(col == "purple"){
    gcols_purple
  }else if(col == "orange"){
    gcols_orange
  }else if(col == "green"){
    gcols_green
  }else if(col == "cyan"){
    gcols_cyan
  }else if(col == "salmon"){
    gcols_salmon
  }
}

word_color <- reactive({
  req(rv$ora_color)
  
  if(rv$ora_color == "red"){
    word_color <- "#a50026"
  }else if(rv$ora_color == "blue"){
    word_color <- "#08519c"
  }else if(rv$ora_color == "grey"){
    word_color <- "#C0C0C0"
  }else if(rv$ora_color == "purple"){
    word_color <- "#C77CFF"
  }else if(rv$ora_color == "orange"){
    word_color <- "#CD9600"
  }else if(rv$ora_color == "green"){
    word_color <- "#7CAE00"
  }else if(rv$ora_color == "cyan"){
    word_color <- "#00A9FF"
  }else if(rv$ora_color == "salmon"){
    word_color <- "#f8766d"
  }
})

# ------ color tones combination for GSEA run ---------
gcols_div <- function(col1 = rv$gsea_up, col2 = rv$gsea_down){
  col_up <- g_color(col1)
}
