# --------------- set up filters ---------------

# There are 2 modes available:
#     1. filter by variables >> levels
# 2. manually filter by samples
# 
# You can only activate one mode at a time, because of potential conflicts.
# 
# When switching from variable mode to manual mode, it will carry over the sample selection. 
# However, Selection will be reset when switching from manual mode to variable mode.

# Samples under current filter is stored in rv$samples
# 
# DYNAMICS:
#     - rv$samples is initialized as all the samples upon platform selection
# - each refreshing of the filtered design table will update this 
# 
# You can keep track of this list in the top right dropdown.



# whole ui
output$filter_design_ui <- renderUI({
  div(
    # radioButtons(
    #   inputId = "fddf_filter_mode",
    #   label = "Filter mode",
    #   choices = c("By Predefined Variables"="variables", 
    #               "Manual" = "manual"),
    #   inline=T),
    
    # uiOutput("fddf_filter_samples"),
    # uiOutput("fddf_filter_vars"),
    HTML("<strong>Pre-filtering by existing study design <i>(optional)</i></strong><br>
         If the authors have uploaded their study design in full, you may use the following options to exclude unwanted samples from analysis.
         You will be able to fine-tune your selection in the next section.
         "),
    hr(),
    uiOutput("filter_vars_levels")
    
  )
  
})

##### manual filter by samples -------------##
# to carry over variable mode selections to manual mode. (but not vice versa)
# observeEvent(input$fddf_filter_mode, {
#   rv$temp_samples <- rv$samples
# })
# 
# 
# # filter by samples
# output$fddf_filter_samples <- renderUI({
#   req(input$fddf_filter_mode=="manual")
#   
#   all_gsms <- isolate(rv$all_samples)
#   choices <- all_gsms
#   if (input$fddf_show_rown == "Sample name"){
#     # show the sample names instead
#     names(choices) <- translate_sample_names(all_gsms,  # translating from
#                                              rv$pdata[c("title", "geo_accession")],  # translation df
#                                              "title") # translating to
#   }
#   
#   checkboxGroupInput("filter_samples", "Filter samples by variables:",
#                      choices= choices,
#                      selected= rv$temp_samples, # carries over previous selection
#                      inline=T
#   )
# })


##### filter by variables -------------##
# # select vars
# output$fddf_filter_vars <- renderUI({
#   # req(input$fddf_filter_mode=="variables")
#   
#   checkboxGroupInput("filter_vars", "Filter samples by variables:",
#                      choices= names(var_summary()),
#                      selected= names(var_summary()),
#                      inline=T
#   )
# })

# select levels
observe({
  req(length(var_summary()) >0)
  
  vs <- var_summary()
  filter_vars <- names(var_summary())
  if (length(filter_vars)>0){
    vs <- vs[filter_vars] # subset list to selected vars only
    
    v <- vector(mode="list", length=length(vs))
    for (i in 1:length(vs)){
      v[[i]] <- div(style="display: inline-block;vertical-align:top; width: 190px;",
                    pickerInput(
                      inputId = paste0("vs_",i),
                      label = names(vs)[[i]],
                      choices = names(vs[[i]]),
                      selected = names(vs[[i]]),
                      options = list(
                        `actions-box` = TRUE,
                        size = 10,
                        style = "btn-default",
                        `selected-text-format` = "count > 0"
                      ),
                      multiple = TRUE
                    )
                    # box(title=names(vs)[[i]], width = 12, solidHeader=F, status = "primary", collapsible=T, collapsed=T,
                    #     checkboxGroupInput(inputId = paste0("vs_",i),
                    #                        label = NULL,
                    #                        choices = names(vs[[i]]),
                    #                        selected = names(vs[[i]])
                    #     )
                    #     
                    # )
                    )
    }
    rv$v <- v
  } else {
    rv$v <- HTML("<div>Select one or more variables to filter by.</div>")
  }
  
  
  
})

output$filter_vars_levels <- renderUI({
  req(is.null(rv$v)==F)
  # req(input$fddf_filter_mode=="variables")
  rv$v
})

# --------------- filter design matrix ---------------

# filter design matrix
filtered_design_df <- reactive({
  # req(length(input$filter_vars)>0)
  # req(nchar(input$fddf_filter_mode)>0)
  # if(rv$run_mode == "auto"){
    df <- design_df()
  
  validate(
    need(ncol(df)>1,
         HTML("Design matrix only has one column; Preview is not available.")
         )
  )
  
  
  # if (input$fddf_filter_mode== "variables"){
    
    # for each specified var, filter by specified levels.
    filter_vars <- names(var_summary())
    for (i in 1:length(filter_vars)){
      var <- filter_vars[[i]]
      levels <- input[[paste0("vs_",i)]]
      
      df <- df[(df[[var]] %in% levels),]
    }

  # } else if (input$fddf_filter_mode== "manual"){
  # 
  #   # filter by samples
  #   if (length(input$filter_samples)>0){
  #     df <- df[input$filter_samples, ]
  #   }
  # 
  # }
  
  rv$samples <- rownames(df) # update filtered samples into rv
  rv$fddf <- df # update filtered table into rv
  df
})


# show filtered design matrix
output$filtered_design_df <- DT::renderDataTable({
  if(rv$run_mode == "auto") {
    req(is.null(rv$gse_all)==F)
    req(is.null(rv$plat_id)==F)
  }
  # req(length(input$filter_vars)>0)
  
  df <- filtered_design_df()
  
  # translate GSM column names to sample names on display
  if (input$fddf_show_rown == "Sample name" && rv$run_mode == "auto"){
    rownames(df) <- make.unique(as.character(translate_sample_names(rownames(df),  # translating from
                                           rv$pdata[c("title", "geo_accession")],  # translation df
                                           "title")), # translating to
                                sep = "_")
  }
  df_no(df)
  # fwrite(df, file = "design_matrix.csv", row.names = TRUE)
  
}#, plugins="ellipsis",options=dt_options(30, scrollX=T)
# ,
# # fillContainer = T # add this to prevent header not scrolling with content
)
