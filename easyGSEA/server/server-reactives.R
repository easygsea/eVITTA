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

# display colors for selection
# ------ color tones for selection --------
col_opts <- c("Red"="red","Salmon"="salmon","Blue"="blue","Cyan"="cyan","Orange"="orange","Green"="green","Purple"="purple","Grey"="grey")

col_choicesopt <- list(
  content = c("<div style='color: #a50026;'>Red</div>"
              ,"<div style='color: #f8766d;'>Salmon</div>"
              ,"<div style='color: #08519c;'>Blue</div>"
              ,"<div style='color: #0098e6;'>Cyan</div>"
              ,"<div style='color: #e69500;'>Orange</div>"
              ,"<div style='color: #688800;'>Green</div>"
              ,"<div style='color: #bc63ff;'>Purple</div>"
              ,"<div style='color: #696969;'>Grey</div>"
  ))

color_tone_div <- reactive({
  if(rv$run_mode == "glist"){
    pickerInput("ora_color",
                HTML(paste0("Adjust color tone ",add_help("bar_col"))),
                col_opts,
                rv$ora_color
                ,choicesOpt = col_choicesopt
                # ,justified = TRUE,
                # checkIcon = list(
                #     yes = icon("ok", 
                #                lib = "glyphicon"))
    )
  }else{
    fluidRow(
      column(
        12,
        pickerInput("up_color",
                    HTML(paste0("Color tone for upregulation ",add_help("bar_col_up"))),
                    col_opts,
                    rv$up_color
                    ,choicesOpt = col_choicesopt
        )
      )
      ,column(
        12,
        pickerInput("down_color",
                    HTML(paste0("Color tone for downregulation ",add_help("bar_col_down"))),
                    col_opts,
                    rv$down_color
                    ,choicesOpt = col_choicesopt
        )
      )
    )
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

word_color_basic <- function(col){
  if(col == "red"){
    "#a50026"
  }else if(col == "blue"){
    "#08519c"
  }else if(col == "grey"){
    "#696969"
  }else if(col == "purple"){
    "#C77CFF"
  }else if(col == "orange"){
    "#ffa500"
  }else if(col == "green"){
    "#7CAE00"
  }else if(col == "cyan"){
    "#00A9FF"
  }else if(col == "salmon"){
    "#f8766d"
  }
  
}

word_color <- reactive({
  req(rv$ora_color)
  
  word_color_basic(rv$ora_color)
})

word_color_div <- function(col1 = rv$up_color, col2 = rv$down_color){
  c(word_color_basic(col2),word_color_basic(col1))
}

# ------ color tones combination for GSEA run ---------
gcols_div <- function(col1 = rv$up_color, col2 = rv$down_color){
  col_up <- g_color(col = col1)
  col_down <- g_color(col = col2)
  
  if(col1 == col2){
    c(rev(col_down),col_up)[-6]
  }else{
    unique(c(rev(col_down),col_up))
  }
}
