# --------------- show summary of the metadata ----------------
# GSE metadata ------------

# get the current gse. call using gse()
gse <- reactive({
  req(is.null(rv$gse_all)==F)
  req(is.null(rv$plat_id)==F)
  
  rv$gse_all[[rv$plat_id]]
})

# get full gse metadata as list. call using gse_meta()
gse_meta <- reactive({
  req(is.null(gse())==F)
  
  notes(experimentData(gse()))
})

# get full gse metadata as dataframe. call using gse_meta_df()
gse_meta_df <- reactive({
  req(is.null(gse_meta())==F)
  
  named_list_to_df(gse_meta(), c("Field", "Value"))
})

# GSM metadata -----------
gse_samples <- reactive({
  req(is.null(gse())==F)
  
  sampleNames(phenoData(gse()))
})

# get metadata shared among all GSMs as list. 
gsm_meta <- reactive({
  req(is.null(gse())==F)
  req(is.null(gse_samples())==F)
  
  vals <- find_repeating_values(pData(phenoData(gse())))
  c("samples" = paste(gse_samples(), collapse=" "), # add sample info to the phenodata list
    "sample_count" = length(gse_samples()),
    vals)
})

# get metadata shared among all GSMs as df. 
gsm_meta_df <- reactive({
  req(is.null(gse_meta())==F)
  
  named_list_to_df(gsm_meta(), c("Field", "Value"))
})



# show gse metadata table -----------

output$gse_meta_df <- DT::renderDataTable({
  req(is.null(rv$gse_all)==F)
  req(is.null(rv$plat_id)==F)
  req(nrow(gse_meta_df())>0)
  
  df <- gse_meta_df()
  df$Field <- tidy_field_col(df$Field)
  df
  
}, plugins="ellipsis",options=dt_options(80),
# fillContainer = T # add this to prevent header not scrolling with content
)

# show gsm metadata table -----------

output$gsm_meta_df <- DT::renderDataTable({
  req(is.null(rv$gse_all)==F)
  req(is.null(rv$plat_id)==F)
  req(nrow(gse_meta_df())>0)
  
  df <- gsm_meta_df()
  df$Field <- tidy_field_col(df$Field)
  df
  
}, plugins="ellipsis",options=dt_options(80),
# fillContainer = T # add this to prevent header not scrolling with content
)

# show summary of metadata -----------

# this is df for combined gse/gsm metadata, with unique field names. should be hidden?
all_fields <- reactive({
  xx=gse_meta_df()
  yy=gsm_meta_df()
  yy <- yy[!grepl("contact_",yy$Field) , ] # remove those duplicated contact info cols...
  # print(yy)
  for (i in yy$Field){
    if(i %in% xx$Field){
      xx$Field <- replace(xx$Field, xx$Field==i, paste0("study_",i))
      yy$Field <- replace(yy$Field, yy$Field==i, paste0("experiment_",i))
    }
  }
  rbind(xx,yy) 
})

# update the summary text according to selection
observe({
  fields <- input$summarize_meta
  
  if(length(fields)>0){
    df <- all_fields()
    # initialize text
    texts <- vector(mode="list", length=length(fields))
    for (i in 1:length(fields)){
      texts[[i]] <- paste0(
        "<strong>", tidy_field_col(fields[[i]]), ":</strong><br>"
        ,
        df[df$Field==fields[[i]],"Value"]
      )
    }
    text <- paste(texts, collapse="<br><br>")
    
    rv$text <- text
  } else {
    rv$text <- "<span style='color: gray'>Select fields above to show summary.</span>"
  }
})

# show whole summary panel
output$gse_summary_ui <- renderUI({
  req(is.null(rv$gse_all)==F)
  req(is.null(rv$plat_id)==F)
  req(nrow(gse_meta_df())>0)
  
  df <- gse_meta_df()
  selected <- grep_multiple(c("title", "study_type", "sample_count", "organism", "summary"), isolate(all_fields())$Field, order=T)
  # print(selected)
  choices <- c(selected, setdiff(all_fields()$Field, selected)) # certain order
  div(
    selectInput("summarize_meta", "Include in summary:",
                multiple=T,
                choices=choices,
                selected= selected,
                width="100%"
    ),
    uiOutput("show_summary_ui")
    
  )
  
})

# show text summary
output$show_summary_ui <- renderUI({
  req(nchar(rv$text)>0)
  shiny::HTML(rv$text)
})
