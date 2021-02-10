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
# ------ color tones for selection: options --------
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

# ------ color tones for selection: Results tab --------
color_tone_div <- reactive({
  if(rv$run_mode == "glist"){
    fluidRow(
      column(
        12,
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
        ,bsTooltip("bar_col",HTML(col_tone_bs),placement = "top")
      )
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
        ,bsTooltip("bar_col_up",HTML(paste0(col_tone_bs," for upregulation")),placement = "top")
        ,bsTooltip("bar_col_down",HTML(paste0(col_tone_bs," for downregulation")),placement = "top")
      )
    )
  }
})

# ------ color tones for selection: Enrichment vis tab --------
vis_col_div <- reactive({
  req(rv$run_mode)
  
  if(rv$run_mode == "glist"){
    column(
      width = 4, offset = 2,
      pickerInput("vis_color",
                  HTML(paste0("Color tone <br>",add_help("vis_col_q"))),
                  col_opts,
                  rv$ora_color
                  ,choicesOpt = col_choicesopt
                  # ,justified = TRUE,
                  # checkIcon = list(
                  #     yes = icon("ok", 
                  #                lib = "glyphicon"))
      )
      ,bsTooltip("vis_col_q",HTML(col_tone_bs),placement = "top")
    )
  }else if(rv$run_mode == "gsea"){
    div(
      column(
        4,
        pickerInput("vis_color_up",
                    HTML(paste0("Upregulation<br>",add_help("vis_col_up_q"))),
                    col_opts,
                    rv$up_color
                    ,choicesOpt = col_choicesopt
        )
      )
      ,column(
        4,
        pickerInput("vis_color_down",
                    HTML(paste0("Downregulation<br>",add_help("vis_col_down_q"))),
                    col_opts,
                    rv$down_color
                    ,choicesOpt = col_choicesopt
        )
      )
      ,bsTooltip("vis_col_up_q",HTML(paste0(col_tone_bs," for upregulation")),placement = "top")
      ,bsTooltip("vis_col_down_q",HTML(paste0(col_tone_bs," for downregulation")),placement = "top")
      
    )
    
  }
})

# ------ color tones for ORA run: Results tab --------
g_color <- function(col = rv$ora_color){
  # color tone, red or blue
  if(col == "red"){
    gcols_red
  }else if(col == "blue"){
    gcols_blue
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

# ------ color tones for GSEA run: Results tab ---------
gcols_div <- function(col1 = rv$up_color, col2 = rv$down_color){
  col_up <- g_color(col = col1)
  col_down <- g_color(col = col2)
  
  if(col1 == col2){
    c(rev(col_down),col_up)[-6]
  }else{
    unique(c(rev(col_down),col_up))
  }
}

# display db name & id display options
# ------- display/remove db name & id -----------
tv_div <- reactive({
  fluidRow(
    column(
      12,
      checkboxInput(
        "db_name_y",
        HTML(paste0("Display dababase prefix ",add_help("db_name_y_q")))
        ,value = rv$db_name_y
      )
    )
    ,column(
      12,
      checkboxInput(
        "db_id_y",
        HTML(paste0("Display gene set ID ",add_help("db_id_y_q")))
        ,value = rv$db_id_y
      )
    )
    ,bsTooltip("db_name_y_q",HTML("By default, each gene set is prefixed by its originating database (abbreviated). Unselect to delete the prefix.")
               ,placement = "top")
    ,bsTooltip("db_id_y_q",HTML("By defualt, each gene set is annotated with its unique ID (if any) in the original database. Unselect to delete the ID string.")
               ,placement = "top")
  )
})

tv_d_div <- reactive({
  fluidRow(
    column(
      12,
      checkboxInput(
        "db_name_d_y",
        HTML(paste0("Display dababase prefix ",add_help("db_name_y_q")))
        ,value = rv$db_name_y
      )
    )
    ,column(
      12,
      checkboxInput(
        "db_id_d_y",
        HTML(paste0("Display gene set ID ",add_help("db_id_y_q")))
        ,value = rv$db_id_y
      )
    )
    ,bsTooltip("db_name_y_q",HTML("By default, each gene set is prefixed by its originating database (abbreviated). Unselect to delete the prefix.")
               ,placement = "top")
    ,bsTooltip("db_id_y_q",HTML("By defualt, each gene set is annotated with its unique ID (if any) in the original database. Unselect to delete the ID string.")
               ,placement = "top")
  )
})

observeEvent(input$db_name_d_y,{rv$db_name_y <- input$db_name_d_y})
observeEvent(input$db_id_d_y,{rv$db_id_y <- input$db_id_d_y})
