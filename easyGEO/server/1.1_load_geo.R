# # DEMO SESSION CODE -------------------------------------------------------
# library(later)
# # the modal to remind the user it is a demo session
# observe({
#   init_demo()
#   showModal(modalDialog(title = tags$h3("Welcome to our easyGEO demo session"),
#                         tags$h4("The demo session has the features of our app.
#                         Please follow the intro tour and switch to different tabs to explore it."),
#                         size = "m",
#                         easyClose = FALSE
#                         ,footer = actionButton("welcome_modal",label = "OK")))
# 
# })
# # when the user closed the modal, start rintrojs
# observeEvent(input$welcome_modal, {
#   removeModal()
#   call_introjs(rbind(intros$E_pre,intros$E_post,intros$E_post_with_summary_ui))
#   print(input$menu1)
# 
# })
# 
# # start rintrojs when users switch tabs
# observeEvent(input$menu1,{
#   if(input$menu1 == "tab3"){
#     later::later(~call_introjs(intros$D_post), delay = 2)
#   } else if(input$menu1 == "tab2"){
#     later(~call_introjs(intros$F_post),2)
#   } else if(input$menu1 == "tab4"){
#     later(~call_introjs(rbind(intros$R_post,intros$R_post_deg)),2)
#   } else if(input$menu1 == "tab5"){
#     later(~call_introjs(intros$V_volcano), 2)
#   } else {
# 
#   }
# })
# # END-----------------------------------------------------------------------------


# --------------- search GEO accession ---------------
# currently checks if input exists
# todo: check if input format is correct (GSEXXXXXX)

output$geo_accession_ui <- renderUI({
  if(rv$demo == "yes"){
    init_demo()
  }
  
  div(
    textInput(
      inputId = "geo_accession",
      label = p("GSE Accession Number:"
                   ,tags$style(type = "text/css", "#q1 {display: inline-block;width: 16px;height: 16px;padding: 0 0 0 1px;border-radius: 50%;vertical-align: baseline;font-size:10px;}"),
                   bsButton("q1", label = "", icon = icon("question"), style = "info", size = "extra-small")),
      value = rv$demo_acc,
      placeholder = "GSE147507",
      width = "100%"
    ),
    bsTooltip("q1", HTML("Begins with GSE. <i>Click</i> to load an <u>example GEO accession number</u>")
              , placement = "top")

    ,"Note: only RNA-seq and single_channel microarray datasets are currently accepted.", br(), br(),

    uiOutput("search_geo")

  )
})

# update text box when q1 actionbutton clicked
observeEvent(input$q1,{
  updateTextInput(session,
    inputId = "geo_accession",
    value = "GSE147507"
  )
})

output$search_geo <- renderUI({
  req(nchar(input$geo_accession)>0)
  # if (nchar(input$geo_accession)>0){
    actionButton("search_geo", "Search")
  # } else {
  #   "Enter a valid GEO accession number."
  # }

})

# --------------- get GEO matrix ---------------
# this takes several mins for larger datasets; need more feedback?

observeEvent(input$search_geo, {

  rv$gse_all <- NULL
  rv$geo_accession <- NULL
  rv$platforms <- NULL
  rv$plat_id <- NULL
  rv$deg <- NULL
  rv$identifiers_df <- NULL
  rv$identifiers <- NULL

  withProgress(message = 'Getting data. Please wait a minute...', value = 1, {

    rv$geo_accession <- isolate(input$geo_accession)
    #It appears that this is where load happens and we need to test if we things are unexpected when loading
    rv$gse_all <- try(getGEO(input$geo_accession, GSEMatrix=T))

    if(inherits(rv$gse_all, "try-error")) {
      ErrorMessage <- conditionMessage(attr(rv$gse_all, "condition"))  # the error message
      #Depending on what we entered, different types of errors could occur:

      if(ErrorMessage == "object 'destfile' not found"){
          DisplayText <- "Your input is invalid. Please enter an existing accession number."
          showModal(modalDialog(
            title = "Data fetching error",
            HTML(DisplayText),
            size = "l",
            easyClose = TRUE
            ,footer = modalButton("OK")
          ))
        }
      if(ErrorMessage == "HTTP error 400."){
        DisplayText <- "Input format is invalid. Please double check and try again."
        showModal(modalDialog(
          title = "Data fetching error",
          HTML(DisplayText),
          size = "l",
          easyClose = TRUE
          ,footer = modalButton("OK")
        ))
      }
      if((ErrorMessage != "object 'destfile' not found")&(ErrorMessage != "object 'destfile' not found")){
        # DisplayText <- paste0("Failed to get data from server. Please double check your query and try again", "<br>", ErrorMessage)
        DisplayText <- paste0("Unable to retrieve ",rv$geo_accession," data from NCBI server.", "<br>", ErrorMessage)
        showModal(modalDialog(
          title = "Data fetching error",
          HTML(DisplayText),
          size = "l",
          easyClose = TRUE
          ,footer = modalButton("OK")
        ))
      }
    }

    req(!inherits(rv$gse_all, "try-error"))

    rv$platforms <- tabulate(rv$gse_all, annotation)
    rv$gpl_summary <- summarize_gpl(rv$gse_all)
    # initialize gpl selection choices
    choices <- lapply(rv$gpl_summary, function(x){
      HTML(paste0(x[["ID"]],": ", x[["Organism"]], " (", x[["Samples"]]," samples)" ))
    })
    rv$gpl_choices <- invert_vector(choices)

  })

})

# --------------- select the desired platform ----------------
# Sometimes multiple platforms are in the GSE, thus the GSE object is a list with length >1
# we need to make people choose one

# select GEO platform, with tooltips that summarize the GPL
output$geo_platform_ui <- renderUI({
  req(is.null(rv$gse_all)==F)
  req(length(rv$platforms)>0)
  req(length(rv$gpl_summary)>0)
  req(length(rv$gpl_choices)>0)

  div(id="select_plat",
    radioButtons(
      inputId = "plat",
      label = HTML("Platforms available:",add_help("q2")),
      # choices = rv$platforms
      choices = rv$gpl_choices
    ),
    bsTooltip("q2",HTML("GEO platforms represent a diverse range of technologies, molecule types, and annotation conventions.<br><br>Select the platform you are interested in and <i>click</i> <b>Select to proceed</b>")
              ,placement = "top"),
    uiOutput("gpl_tooltips"),
    uiOutput("study_type_feedback"),
    uiOutput("select_geo_platform")

  )

})

# generate tooltips for gpl selections
observe({
  req(length(rv$gpl_summary)>0)
  req(length(rv$gpl_choices)>0)

  # tooltip content
  tooltips <- unlist(unname(lapply(rv$gpl_summary, function(x){
    paste0("<strong>", names(x),"</strong>: ", x, collapse="<br>")
  })))

  # generate tooltip
  for (i in 1:length(rv$gpl_choices)){
    rv$gpl_tooltips[[i]] <- radioTooltip(id = "plat",
                                     choice = rv$gpl_choices[[i]],
                                     title = tooltips[[i]],
                                     placement = "right", trigger = "hover")
  }
})

output$gpl_tooltips <- renderUI({
  req(length(rv$gpl_tooltips)>0)
  rv$gpl_tooltips
})



# detect study type during platform selection
study_type <- reactive({
  req(is.null(rv$gse_all)==F)
  req(length(rv$platforms)>0)
  req(is.null(input$plat)==F)


  gse_temp <- rv$gse_all[[match(input$plat, rv$platforms) ]]
  # print(gse_temp)

  req(is.null(gse_temp)==F)

  # get channel count
  gsm_meta_temp <- find_repeating_values(pData(phenoData(gse_temp)))
  channel_count <- gsm_meta_temp$channel_count

  # get study type
  gse_meta_temp <- notes(experimentData(gse_temp))
  type <- gse_meta_temp$type

  return(list("type" = type, "channel_count" = channel_count))
})

output$study_type_feedback <- renderUI({
  req(is.null(rv$gse_all)==F)
  req(length(rv$platforms)>0)
  req(is.null(input$plat)==F)

  study_type <- study_type()$type
  channel_count <- study_type()$channel_count

  errors = 0
  msgs = vector()
  if (study_type %in% accepted_study_types == F){
    errors = errors +1
    msgs <- c(msgs,
              paste0("<strong>CAUTION: </strong>You have selected a study of type: <strong>",study_type, "</strong>, which is not currently supported. <br>
                                Please continue at your own risk.")
    )
    box_color = "yellow"
  }

  # making sure the channel_count is not null to run the function
  if(is.null(channel_count) == F){
    if (channel_count != 1) {
      errors = errors +1
      msgs <- c(msgs,
                paste0("<strong>CAUTION: </strong>Dataset has <strong>",channel_count, "</strong> channels, which is not currently supported.<br>
                                  Only data in the first channel will be read.")
      )
      box_color = "yellow"
    }else{
      rv$microarray = "yes"
    }
  }
  msg <- paste(msgs, collapse="<br>")
  if(errors>0){
    fluidRow(
      box(
        title = NULL, background = box_color, solidHeader = TRUE, width=12,
        HTML(msg)
      )
    )
  } else {return(NULL)}


})

output$select_geo_platform <- renderUI({
  # req(
  #     # study_type()$type %in% accepted_study_types &
  #         study_type()$channel_count == 1)

  actionButton("geo_platform", "Select to proceed")
})


observeEvent(input$geo_platform, {

  plat <- isolate(input$plat)
  rv$plat_id <- match(plat, rv$platforms)

  # initialize the count matrix (even if it's empty) with first row = Name
  exprs <- exprs(gse())
  if (nrow(exprs) >0){ # put gene names onto first column
    dmdf <- cbind(Name = rownames(exprs), exprs)
  } else {
    dmdf <- cbind(Name = character(0), exprs)
  }
  rownames(dmdf) <- c() # remove rownames
  rv$dmdf <- dmdf
  rv$dmdf_o <- dmdf


  # initialize samples list
  rv$all_samples <- sampleNames(gse()) # this one stays the same
  rv$samples <- sampleNames(gse()) # this one will change

  # initialize pdata
  rv$pdata <- pData(phenoData(gse()))

  # initialize fddf
  rv$fddf <- design_df() # initially unfiltered, will update when filter
  
  # initialize gene identifiers
  if(rv$microarray == "yes"){
    # GPL probes' info
    # gset <- rv$gse_all[[match(plat, rv$platforms)]]
    
    df <- pData(featureData(gse())) 
    df <- df %>%
      dplyr::select(any_of(c(matches(accepted_gene_identifiers, ignore.case = T),ends_with("_id", ignore.case = T))))

    
    rv$identifiers <- colnames(df)
    rv$identifiers_df <- df
    
  }

})

# ---------- when all is done, show guide box to next page ---------
output$guide_1a <- renderUI({
  if (is.null(rv$plat_id)==F){ # user already selected a platform
    msg = "Navigate to <b>2. Data matrix</b> to proceed"
    guide_box("guide1", msg)
  } else {
    return(NULL)
  }
})

observeEvent(input$guide1,{
  updateTabItems(session, "menu1", "tab3")
})



# --------------- progress info box ---------------

output$progress_1 <- renderUI({

  progress_box(id="infobox_1", prompt="To-dos:",
               msg=c("1. Search a valid GSE number", "2. Select a platform", "3. Read the study information"),
               condition=c(!is.null(rv$gse_all), !is.null(rv$plat_id), !is.null(rv$plat_id)),
               bttn_id="next_p1"
               )

})

observeEvent(input$next_p1, {
  updateTabItems(session, "menu1", "tab3")
})
