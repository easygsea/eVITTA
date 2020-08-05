#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    ####---------------------- REACTIVE VALUES---------------------------####
    
    rv <- reactiveValues(gse_all = NULL
    )
    
    ####---------------------- BODY 1 ---------------------------####
    
    # --------------- search GEO accession ---------------
    # currently checks if input exists
    # need to check if input is correct format?
    
    output$geo_accession_ui <- renderUI({
        div(
            textInput(
                inputId = "geo_accession",
                label = "GSE Accession Number:", 
                value = "GSE147507",
                placeholder = "GSE137355",
                width = "100%"
            ),
            "Note: only RNA-seq and microarray datasets are accepted.", br(), br(),
            
            uiOutput("search_geo")
            
        )
    })
    
    output$search_geo <- renderUI({
        if (nchar(input$geo_accession)>0){
            actionButton("search_geo", "Search")
        } else {
            "Enter a GEO accession."
        }
        
    })
    
    # --------------- get GEO matrix ---------------
    # this takes several mins for larger datasets; need more feedback?
    
    observeEvent(input$search_geo, {
        
        rv$gse_all <- NULL
        rv$geo_accession <- NULL
        rv$platforms <- NULL
        rv$plat_id <- NULL
        
        withProgress(message = 'Getting data. Please wait a minute...', value = 0.5, {
            
            rv$geo_accession <- isolate(input$geo_accession)
            rv$gse_all <- getGEO(input$geo_accession, GSEMatrix=T) 
            
            rv$platforms <- tabulate(rv$gse_all, annotation)
            
            
        })
        
    })
    
    # --------------- select the desired platform ----------------
    # Sometimes multiple platforms are in the GSE, thus the GSE object is a list with length >1
    # we need to make people choose one
    
    output$geo_platform_ui <- renderUI({
        req(is.null(rv$gse_all)==F)
        req(length(rv$platforms)>0)
        
        div(
            radioButtons(
                inputId = "plat",
                label = "Platforms available:",
                choices = rv$platforms
            ),
            actionButton("geo_platform", "Select")
        )
        
    })
    
    observeEvent(input$geo_platform, {
        
        plat <- isolate(input$plat)
        rv$plat_id <- match(plat, rv$platforms) 
    })
    
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
        notes(experimentData(gse()))
    })
    
    # get full gse metadata as dataframe. call using gse_meta_df()
    gse_meta_df <- reactive({
        named_list_to_df(gse_meta(), c("Field", "Value"))
    })
    
    # GSM metadata -----------
    gse_samples <- reactive({
        sampleNames(phenoData(gse()))
    })
    
    # get metadata shared among all GSMs as list. 
    gsm_meta <- reactive({
        vals <- find_repeating_values(pData(phenoData(gse())))
        c("samples" = paste(gse_samples(), collapse=" "), # add sample info to the phenodata list
          "sample_count" = length(gse_samples()),
          vals)
    })
    
    # get metadata shared among all GSMs as df. 
    gsm_meta_df <- reactive({
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
        
    }, plugins="ellipsis",options=dt_options(80)
    )
    
    # show gsm metadata table -----------
    
    output$gsm_meta_df <- DT::renderDataTable({
        req(is.null(rv$gse_all)==F)
        req(is.null(rv$plat_id)==F)
        req(nrow(gse_meta_df())>0)
        
        df <- gsm_meta_df()
        df$Field <- tidy_field_col(df$Field)
        df
        
    }, plugins="ellipsis",options=dt_options(80)
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
            
            
            # strong("Title:"),br(),
            # paste0(df[df$Field=="title", "Value"]), br(),br(),
            # strong("Type:"),br(),
            # paste0(df[df$Field=="type", "Value"]), br(),br(),
            # strong("Pubmed ID:"),br(),
            # paste0(df[df$Field=="pubmed_id", "Value"]), br(),br(),
            # strong("Samples:"),br(),
            # paste0(gse_samples()[[1]], " ... ", gse_samples()[[length(gse_samples())]], " (",length(gse_samples())," samples)"), br(),br(),
            # strong("Summary:"),br(),
            # paste0(df[df$Field=="summary", "Value"]), br(),br(),
        )
        
    })
    
    output$show_summary_ui <- renderUI({
        req(nchar(rv$text)>0)
        shiny::HTML(rv$text)
    })
    
    
    
    
    
    
    # show design matrix table -----------
    
    
    design_df <- reactive({
        
        # tidy characteristics
        char_list <- data.frame(t(as.data.frame(pData(phenoData(gse()))) %>% select(contains("characteristics"))))
        char_list[char_list==""] <- NA
        char_list <- as.list(char_list)
        print(char_list)
        
        # map list of characters into dataframe format (those not found = NA)
        char_list <- lapply(char_list, function(x){
            transform_vector(x, ": ")
        })
        # char_list
        chars <- names(table(unlist(lapply(char_list, names))))
        # chars
        ls <- lapply(char_list,function(x){
            xx<- rep(NA, length(chars))
            names(xx) <- chars
            xx[names(x)] <- x
            xx
        })
        # ls
        char_mat <- data.frame(t(data.frame(ls)))
        
        # get rid of single factor columns
        to_keep <- function(x) any(is.numeric(x), length(unique(x)) > 1)
        char_mat <- Filter(to_keep, char_mat)
        
        
        # fill NAs with string?? (optional)
        # char_mat[is.na(char_mat)] <- "NA"
        
        
        char_mat # display this matrix to user
    })
    
    
    output$design_df <- DT::renderDataTable({
        req(is.null(rv$gse_all)==F)
        req(is.null(rv$plat_id)==F)
        
        design_df()
        
    }, plugins="ellipsis",options=dt_options(30)
    )
    
    
    
    # show design summary ui -----------
    
    # variable summary (is a named list of named vectors in form of $var level:freq)
    var_summary <- reactive({
        char_mat <- design_df()
        # get named list of named vectors
        var_summary <- vector(mode="list", length=ncol(char_mat))
        for (i in 1: length(colnames(char_mat))){
            var_summary[[i]] <- table(char_mat[[colnames(char_mat)[[i]]]])
        }
        names(var_summary) <- colnames(char_mat)
        var_summary
    })
    
    # construct the text summary from variable summary
    design_summary <- reactive({
        
        var_summary <- var_summary()
        
        # get text
        textt <- vector(mode="list", length=length(var_summary))
        for (i in 1: length(var_summary)){
            textt[[i]] <- paste0(
                "<strong>", names(var_summary)[[i]], ":</strong><br>", 
                paste(
                    paste(names(var_summary[[i]]), " (", var_summary[[i]], ")", sep="")
                    , collapse=", ")
                )
        }
        paste(textt, collapse="<br><br>")
    })
    
    
    output$design_summary_ui <- renderUI({
        req(is.null(rv$gse_all)==F)
        req(is.null(rv$plat_id)==F)
        
        div(
            strong(paste0("Study design for ", annotation(gse()), ": ")), br(),br(),
            
            
            shiny::HTML(design_summary())
            
            
        )
    })
    
    output$design_variables <- renderInfoBox({
        req(is.null(rv$gse_all)==F)
        req(is.null(rv$plat_id)==F)
        
        infoBox(annotation(gse()), 
                strong(paste0("Variables: ", ncol(design_df()))),
                paste0(paste(colnames(design_df()), collapse=", ")), 
                icon = icon("info-circle"))
    })
    output$design_samples <- renderInfoBox({
        infoBox(annotation(gse()), 
                strong(paste0("Samples: ", nrow(design_df()))),
                paste0(rownames(design_df())[[1]], " ... " ,
                       rownames(design_df())[[length(design_df())]]), 
                icon = icon("info-circle"))
    })
    
    output$design_variables <- renderValueBox({
        valueBox(
            paste0(ncol(design_df()), " variables"), 
            paste0(paste(colnames(design_df()), collapse=", ")), 
            icon = icon("microscope"),
            color = "blue"
        )
    })
    output$design_samples <- renderValueBox({
        valueBox(
            paste0(nrow(design_df()), " samples"), 
            paste0(rownames(design_df())[[1]], " ... " ,
                   rownames(design_df())[[length(design_df())]]), 
            icon = icon("seedling"),
            color = "purple"
        )
    })
    
    
    
    # filter design matrix ui -----------
    
    # ui for selecting variables and levels to filter by
    output$filter_design_ui <- renderUI({
        div(
            selectInput("filter_vars", "Filter samples by variables:",
                        multiple=T,
                        choices= names(var_summary()),
                        selected= names(var_summary()),
                        width="100%"
            ),
            "Ignore samples with NA in selected variables? checkbox", br(),
            uiOutput("filter_vars_levels")
            
        )
        
    })
    
    # generates dynamic ui for selection
    observe({
        req(length(var_summary()) >0)
        
        vs <- var_summary()
        if (length(input$filter_vars)>0){
            vs <- vs[input$filter_vars] # subset list to selected vars only
            
            v <- vector(mode="list", length=length(vs))
            for (i in 1:length(vs)){
                v[[i]] <- div(style="display: inline-block;vertical-align:top; width: 260px;",
                              wellPanel(checkboxGroupInput(inputId = paste0("vs_",i), 
                                                           label = names(vs)[[i]], 
                                                           choices = names(vs[[i]]),
                                                           selected = names(vs[[i]])
                              )))
            }
            rv$v <- v
        } else {
            rv$v <- HTML("<div>Select one or more variables to filter by.</div>")
        }
        
        
        
    })
    
    output$filter_vars_levels <- renderUI({
        req(is.null(rv$v)==F)
        rv$v
        # print(rv$v)
    })
    
    # show filtered design matrix -----------
    
    output$filtered_design_df <- DT::renderDataTable({
        req(is.null(rv$gse_all)==F)
        req(is.null(rv$plat_id)==F)
        req(length(input$filter_vars)>0)
        
        df <- design_df() #dk why the header doesnt scroll? same as the other one
        
        # for (i in length(input$filter_vars)){
        #     var <- input$filter_vars[[i]]
        #     levels <- input[[paste0("vs_",i)]]
        #     req(var)
        #     print(var)
        #     req(levels)
        #     print(levels)
        #     df <- df[df[[var]] %in% levels,]
        # }
        
        df
        
    }, plugins="ellipsis",options=dt_options(30)
    )
    
    
    ####---------------------- DEBUG 1 ---------------------------####
    
    output$debug0 <- renderPrint({
        paste("rv$platforms = ", rv$platforms, ", "
              ,"rv$plat_id = ", rv$plat_id, ", "
              )
    })

})
