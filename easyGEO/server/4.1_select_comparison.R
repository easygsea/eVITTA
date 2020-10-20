# -------------- filtered variables and labels -----------------
# filtered DEG pdata according to selected variable & its selected two levels
deg_pdata <- function(p_df=rv$fddf,c_var=input$sp_select_var,c_var_levels=input$sp_select_levels){
  # filter design matrix according to selections
  p_df %>% dplyr::filter(p_df[[c_var]] %in% c_var_levels)
}

# samples in control group
samples_c <- function(p_df=deg_pdata(),c_var=input$sp_select_var,c_level = input$sp_select_levels_base){
  p_df %>% dplyr::filter(p_df[[c_var]] %in% c_level) %>%
    rownames(.)
}

# treatment level
t_level <- function(c_level = input$sp_select_levels_base,c_var_levels=input$sp_select_levels){
  # selected variable - experimental level
  c_var_levels[!c_var_levels %in% c_level]
}

# samples in treatment group
samples_t <- function(p_df=deg_pdata(),c_var=input$sp_select_var){
  p_df %>% dplyr::filter(p_df[[c_var]] %in% t_level()) %>%
    rownames(.)
}

# --------------- 4.1.1 select variables and levels: fine-tune ---------------

output$select_params_ui <- renderUI({
  
  #initialize the choices for demo session function 1
  if(rv$demo == "yes"){
    init_choices()
  }
  
  req(is.null(rv$fddf)==F)
  req(input$ui_select == "sp")
  
  fddf <- rv$fddf
  
  # print(head(fddf))
  
  
  # filter only cols with >2 levels
  fddf[] <- lapply(fddf, function(x){
    if (length(unique(x))>=2){
      return(x)
    } else {return(NULL)}
  })
  
  if (ncol(fddf)>0 & nrow(fddf)>0){
    
    # count how many levels are available for each factor and put in named vector
    level_count <- unlist(lapply(fddf, function(x){
      length(unique(x))
    }))
    names(level_count) <- colnames(fddf)
    
    # prepare the choices text
    choices_text <- paste(names(level_count), " (", level_count, " levels)", sep="")
    choices <- colnames(fddf)
    names(choices) <- choices_text
    
    # autodetect if batch, na is none detected
    batch_selected = choices[grepl("batch",choices)]
    if(identical(batch_selected,character(0))){
      batch_selected = "na"
    }
    
    div(
      # fluidRow(
      #   column(12,
      #          #bsTooltip("ui_select", "Note: You may use \"By design matrix\" to select samples when the authors have uploaded their study design in full."),
      #     # wellPanel(style = paste0("background:",rv$bcol1),
      #     #           HTML("<b>Note:</b> You may use \"By design matrix\" to select samples when the authors have uploaded their study design in full. 
      #     #                           ")
      #     # )
      #   )
      # ),
      fluidRow(
        column(6,
               selectInput(
                 inputId = "sp_select_var",
                 label = HTML("Select variable to analyze:",add_help("v_hp")),
                 choices = choices
               )
               ,bsTooltip("v_hp","The experimental factor, e.g. condition, treatment, mutant, etc."
                          ,placement = "right")
        ),
        column(6,
               selectInput(
                 inputId = "sp_batch_col",
                 label = HTML("Batch effect variable:",add_help("batch_hp")),
                 choices = c("Not applicable"="na", choices),
                 selected= batch_selected#"na"
               )
               ,bsTooltip("batch_hp","Select to eliminate batch effect (if any)"
                          ,placement = "right")

        ),
      ),
      
      uiOutput("sp_select_levels"),
      uiOutput("sp_select_levels_rel"),
      uiOutput("sp_select_levels_rel_fb")
    )
  } else {
    msg = HTML(paste0("Design matrix is incomplete.<br>
                       (NOTE: at least one variable must have >2 levels)<br><br>
                      Try ",
                      "<b>Manual selection</b> to proceed"
                      # , actionLink("coerce",tags$b("Coerce Selection"))
                      # ," or return to <b>Design Matrix</b> to refine study designs."
                      ))
    box_color = "red"
    fluidRow(
      box(
        title = NULL, background = box_color, solidHeader = TRUE, width=12,
        HTML(msg)
      )
    )
  }
})


# UI select levels
output$sp_select_levels <- renderUI({
  
  #initialize the choices for demo session function 2
  if(rv$demo == "yes"){
    init_choices2()
  }
  
  fddf <- rv$fddf
  levels <- unique(fddf[[input$sp_select_var]])
  div(checkboxGroupInput(
    inputId = "sp_select_levels",
    label = HTML("Select two levels to compare:",add_help("tl_hp")),
    choices = levels,
    inline=T
  )
  ,bsTooltip("tl_hp", "Select the control and the experimental groups"
             ,placement = "right")
  )
})

# UI select base level
output$sp_select_levels_rel <- renderUI({
  req(length(input$sp_select_levels)==2 & input$sp_select_var != input$sp_batch_col)
  
  fluidRow(
    column(
      width = 12,
      # select base level
      radioButtons(
        inputId = "sp_select_levels_base",
        label = HTML("Select control level:",add_help("ct_hp")),
        choices = input$sp_select_levels,
        selected = input$sp_select_levels[1],
        inline = T
      )
      ,bsTooltip("ct_hp","The control group"
                 ,placement = "right")
    )
  )
})



# ------------- feedbacks on selected levels ---------------
output$sp_select_levels_rel_fb <- renderUI({
  req(length(input$sp_select_levels)==2 & input$sp_select_var != input$sp_batch_col)
  req(is.null(input$sp_select_levels_base)==F)
  
  # # control level name
  # c_level = input$sp_select_levels_base
  # 
  # # samples in control group
  # t_level = t_level()
  # samples_c = samples_c()
  # samples_c_n = length(samples_c)
  # 
  # # samples in treatment group
  # samples_t = samples_t()
  # samples_t_n = length(samples_t)
  # 
  # textx = paste0("Review samples: ",
  #                c_level," (n=",samples_c_n,") vs. ",
  #                t_level," (n=",samples_t_n,")"
  # )
  
  
  # fluidRow(
  #   box(title=textx, width = 12, solidHeader=F, status = "primary", collapsible=T, collapsed=F,
        div(radioGroupButtons(
          "names_toggle",
          HTML("Show sample names as",add_help("ss_hp")),
          choices = list("GEO accession"="accession","Sample name"="title"),
          selected = "title"
        )
        ,bsTooltip("ss_hp",HTML("<b>GEO accession</b>: GSM ids in NCBI GEO database<br><br><b>Sample name</b>: names provided by the authors")
                   ,placement = "right")
        
        ,uiOutput("ui_samples_fb"))
  #   )
  # )
  
})

output$ui_samples_fb <- renderUI({
  #initialize the choices for demo session function 3
  if(rv$demo == "yes"){
    init_choices3()
  }
  
  # control level name
  c_level = input$sp_select_levels_base
  
  # samples in control group
  t_level = t_level()
  samples_c = samples_c()
  print(samples_c)
  
  # samples in treatment group
  samples_t = samples_t()
  print(samples_t)
  
  # # determine input source GSM or title
  if(input$names_toggle == "title"){
    titles_c = translate_sample_names(samples_c,  rv$pdata[c("title", "geo_accession")],  "title")
    titles_t = translate_sample_names(samples_t,  rv$pdata[c("title", "geo_accession")],  "title")
    
    names(samples_c) = titles_c
    names(samples_t) = titles_t
  }
  
  fluidRow(
    box(
      title=NULL, width = 12, solidHeader=T, status="success",
      column(
        width = 6,
        pickerInput(
          inputId = "samples_c_deg",
          label = c_level,
          choices = samples_c,
          selected = samples_c,
          options = list(
            `actions-box` = TRUE,
            size = 10,
            style = "btn-default",
            `selected-text-format` = "count > 0"
          ),
          multiple = TRUE
        )
      ),
      column(
        width = 6,
        pickerInput(
          inputId = "samples_t_deg",
          label = t_level,
          choices = samples_t,
          selected = samples_t,
          options = list(
            `actions-box` = TRUE,
            size = 10,
            style = "btn-default",
            `selected-text-format` = "count > 0"
          ),
          multiple = TRUE
        )
      )
    )
    
  )
})

# --------------- 4.1.2 select variables and levels: coerce ---------------

# # --------------- jump to coerce selection tab if variable only have one level ---------------
# observeEvent(input$coerce,{
#   updateTabsetPanel(
#     session,
#     "ui_select",
#     selected = "coerce"
#   )
# })

# coerce overall ui
output$coerce_ui <- renderUI({
  req(is.null(rv$fddf)==F)
  req(input$ui_select == "coerce")

  fluidRow(
    column(
      12,
      #bsTooltip("ui_select","Note:manual selection"),
      # wellPanel(style = paste0("background:",rv$bcol1),
      #           HTML("<b>Note:</b> \"Manual selection\" is for any combination of samples. You may manually select samples in the control and the experimental groups.")
      # ),
      radioGroupButtons(
        "names_toggle2",
        HTML("Show sample names as",add_help("ss_hp2")),
        choices = list("GEO accession"="accession","Sample name"="title"),
        selected = "title"
      )
      ,bsTooltip("ss_hp2",HTML("<b>GEO accession</b>: GSM ids in NCBI GEO database<br><br><b>Sample name</b>: names provided by the authors")
                 ,placement = "right")
      
      ,uiOutput("ui_samples_fb2")
    )
  )
})

# -------- coerce sample selection -----------
output$ui_samples_fb2 <- renderUI({
  req(is.null(rv$fddf)==F)
  
  fddf <- rv$fddf
  
  samples_c = samples_t = rownames(fddf)
  
  if(input$names_toggle2 == "title"){
    titles_c = translate_sample_names(samples_c,  rv$pdata[c("title", "geo_accession")],  "title")
    titles_t = translate_sample_names(samples_t,  rv$pdata[c("title", "geo_accession")],  "title")
    
    names(samples_c) = titles_c
    names(samples_t) = titles_t
  }
  
  fluidRow(
    box(
      title=NULL, width = 12, solidHeader=T, status="success",
      column(
        width = 6,
        pickerInput(
          inputId = "samples_c_deg2",
          label = "Control group",
          choices = samples_c,
          # selected = samples_c,
          options = list(
            `actions-box` = TRUE,
            size = 10,
            style = "btn-default",
            `selected-text-format` = "count > 0"
          ),
          multiple = TRUE
        )
      ),
      column(
        width = 6,
        pickerInput(
          inputId = "samples_t_deg2",
          label = "Experimental group",
          choices = samples_t,
          # selected = samples_t,
          options = list(
            `actions-box` = TRUE,
            size = 10,
            style = "btn-default",
            `selected-text-format` = "count > 0"
          ),
          multiple = TRUE
        )
      )
    )
    
    
  )
})

# --------------- observer selections, update rv$samples ---------------------
observeEvent(list(input$samples_c_deg, input$samples_t_deg, input$samples_c_deg2, input$samples_t_deg2),{
  req(input$ui_select)

  if(input$ui_select == "sp"){
    rv$samples = c(input$samples_c_deg, input$samples_t_deg)
  }else if(input$ui_select == "coerce"){
    rv$samples = c(input$samples_c_deg2, input$samples_t_deg2)
  }
})