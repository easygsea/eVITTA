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
    
    ####---------------------- 1. LOAD FROM GEO  ---------------------------####
    
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
        rv$dmdf <- exprs(gse()) # initialize the count matrix (even if it's empty)
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
    
    
    
    ####---------------------- 2. DESIGN SUMMARY  ---------------------------####
    
    
    # get initial design matrix table -----------
    
    
    design_df <- reactive({
        
        # tidy characteristics
        char_list <- data.frame(t(as.data.frame(pData(phenoData(gse()))) %>% select(contains("characteristics"))))
        char_list[char_list==""] <- NA
        char_list <- as.list(char_list)
        # print(char_list)
        
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
        char_mat[is.na(char_mat)] <- "N/A"
        char_mat[char_mat=="NA"] <- "N/A"
        
        # convert cols type. currently, all is converted to factor
        # in the future: integers >> numeric, char >> factor
        char_mat[] <- lapply(char_mat, function(x) {
            # if(is.integer(x) | is.numeric(x)) {
            #     as.numeric(x) 
            # } else {
                as.factor(x)
            # }
        })
        char_mat 
    })
    
    
    
    # summarize design -----------
    
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

    
    
    
    # filter design matrix ui -----------
    
    # ui for selecting variables and levels to filter by
    output$filter_design_ui <- renderUI({
        div(
            checkboxGroupInput("filter_vars", "Filter samples by variables:",
                        choices= names(var_summary()),
                        selected= names(var_summary()),
                        inline=T
            ),
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
                v[[i]] <- div(style="display: inline-block;vertical-align:top; width: 190px;",
                              box(title=names(vs)[[i]], width = 12, solidHeader=F, status = "primary", collapsible=T, collapsed=T,
                                  checkboxGroupInput(inputId = paste0("vs_",i),
                                                     label = NULL,
                                                     choices = names(vs[[i]]),
                                                     selected = names(vs[[i]])
                              )
                                  
                              ))
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

    filtered_design_df <- reactive({
        req(length(input$filter_vars)>0)
        
        df <- design_df()
        # print(input$filter_vars)
        # for each specified var, filter by specified levels.
        for (i in 1:length(input$filter_vars)){
            var <- input$filter_vars[[i]]
            levels <- input[[paste0("vs_",i)]]

            df <- df[(df[[var]] %in% levels),]


        }

        df
    })
    

    
    output$filtered_design_df <- DT::renderDataTable({
        req(is.null(rv$gse_all)==F)
        req(is.null(rv$plat_id)==F)
        req(length(input$filter_vars)>0)
        
        filtered_design_df()
        
    }, plugins="ellipsis",options=dt_options(30, scrollX=T)
    ,
    # fillContainer = T # add this to prevent header not scrolling with content
    )
    
    
    
    
    # show summary valueboxes -----------
    
    output$design_variables <- renderValueBox({
        valueBox(
            paste0(ncol(design_df()), " variables"), 
            paste0(paste(colnames(design_df()), collapse=", ")), 
            icon = icon("microscope"),
            color = "blue"
        )
    })
    output$design_samples <- renderValueBox({
        selected <- nrow(filtered_design_df())
        total <- nrow(design_df())
        valueBox(
            paste0(selected, "/", total, " samples"), 
            HTML(paste0("Selected: ", selected, " samples <br>", 
                        "Total: ", total, " samples")), 
            icon = icon("seedling"),
            color = "purple"
        )
    })
    
    
    ####---------------------- 3. DATA MATRIX  ---------------------------####
    
    
    # detect source of data matrix -----------
    
    output$data_matrix_ui <- renderUI({
        
        # check supplementary data in GSE
        gse_sup <- unlist(gse_meta_df()[grep("supplementary_file", gse_meta_df()$Field),"Value"])
        gse_sup[gse_sup=="NONE"] <- NA # convert NONE to NA
        gse_sup <- gse_sup[is.na(gse_sup)==F] # delete NA
        gse_sup <- strsplit(gse_sup, "\n")[[1]]
        print(gse_sup)

        # check supplementary data in GSMs
        
        gsm_sup <- unlist(gsm_meta_df()[grep("supplementary_file", gsm_meta_df()$Field),"Value"])
        gsm_sup[gsm_sup=="NONE"] <- NA # convert NONE to NA
        gsm_sup <- gsm_sup[is.na(gsm_sup)==F] # delete NA
        gsm_sup <- unlist(gsm_sup)
        print(gsm_sup)
        
        
        # detect where the data is
        if (nrow(exprs(gse()))>0){
            source = "table"
            text <- "<strong>Detected data source: <br>as preloaded datatable.</strong> 
                        <br>Datatable will be shown on the right."
            where <- "table here"
        } else if (length(gse_sup) > 0){
            source = "gse_sup"
            text <- "<strong>Detected data source: <br>as supplementary files in series record.</strong> 
                        <br>Please download the processed data matching the currently selected platform."
            where <- paste0("GSE supplementary (",length(gse_sup),"): ", paste(gse_sup, collapse=", "))
            rv$suplist <- gse_sup
        } else if (length(gsm_sup) > 0){
            source = "gsm_sup"
            text <- "<strong>Detected data source: <br>as supplementary files in samples record.</strong>
                        <br>Please download the processed data matching the currently selected platform."
            where <- paste0("GSM supplementary (",length(gsm_sup),"): ", paste(gsm_sup, collapse=", "))
            rv$suplist <- gsm_sup
        } else {
            source = "none"
            text <- "<strong>Data source not detected!</strong>
                        <br>Please download the processed data matrix from the GEO page or the associated paper."
            where <- ""
        }
        
        # write the detected data source into rv
        rv$sup_source <- source
        
        # output report and url links to the user
        div(
            HTML(text), br(), br(),
            # where,
            uiOutput("sup_links")
        )

    })
    
    
    # generates url links
    observe({
        req(rv$sup_source == "gse_sup" | rv$sup_source == "gsm_sup")
        
        for (i in 1:length(rv$suplist)){
            path = rv$suplist[[i]]
            ftp = dirname(path)
            rv$s[[i]] <- div(style="display: inline-block;vertical-align:top; width: 100%;",
                             wellPanel(tagList(basename(path), br(),
                                     a("Download", href=path), " / ",
                                     a("FTP Folder", href=ftp),
                                     )
                             ))
        }
    })
    
    output$sup_links <- renderUI({
        req(rv$sup_source == "gse_sup" | rv$sup_source == "gsm_sup")
        rv$s
    })
    
    
    
    # upload tidied matrix (only show if data in gse/gsm supplementary) -----------
    
    output$upload_matrix_ui <- renderUI({
        req(rv$sup_source == "gse_sup" | rv$sup_source == "gsm_sup")
        
        tabBox(
            title = NULL, width = 12,
            id = "upload_matrix",
            tabPanel("Upload tidied matrix", 
                     
                     "Upload tidied data matrix here",br(),
                     "Also check for sample matching here",
                     
                     fileInput("file", "Upload tidied data matrix (CSV format):",
                               accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                     )
                     
            )
        )
    })
    
    # when file is uploaded, update state 
    observeEvent(input$file, {
        inFile <- input$file
        indf <- read.csv(inFile$datapath, header=F)
        indf_coln <- unname(unlist(indf[1,])) # colnames = first row
        # print(indf_coln)
        indf_rown <- unname(unlist(indf[,1])) # rownames = first col
        # print(indf_rown)
        indf <- indf[-1,-1]
        # print(head(indf))
        
        # try to convert the indf headers into gsm format
        pdata <- pData(phenoData(gse()))
        indf_coln <- translate_sample_names(indf_coln,  # translating from
                                                 pdata[c("title", "geo_accession")],  # translation df
                                                 "geo_accession") # translating to
        colnames(indf) <- indf_coln[-1]
        
        # print(head(indf))
        
        # then, match each column to rv$dmdf and update the values into it (now it errors out if gpl mismatch....)
        rvdf <- rv$dmdf
        dmdf <- data.frame(matrix(NA, nrow = nrow(indf), ncol = ncol(rvdf))) # initialize empty df
        dmdf <- data.frame(lapply(colnames(rvdf), function(x){ # update values into dmdf (leaves NA if not found)
            if (x %in% colnames(indf)){
                dmdf[[x]] <- indf[[x]]
            } else {return (rep(NA, length(x)))}
        }))
        colnames(dmdf) <- colnames(rvdf)
        rownames(dmdf) <- indf_rown[-1] # update row names
        print(head(dmdf))
        
        rv$dmdf <- dmdf
    })
    
    
    # show data matrix df -----------
    
    output$data_matrix_df <- DT::renderDataTable({
        req(is.null(rv$gse_all)==F)
        req(is.null(rv$plat_id)==F)
        req(is.null(rv$dmdf)==F)
        
        df <- rv$dmdf
        
        # translate GSM column names to sample names on display
        if (input$dmdf_show_coln == "Sample name"){
            pdata <- pData(phenoData(gse()))
            colnames(df) <- translate_sample_names(colnames(df),  # translating from
                                                   pdata[c("title", "geo_accession")],  # translation df
                                                   "title") # translating to
        }

        
        df
        
    }, plugins="ellipsis",
    options=dt_options(30, scrollX=T)
    )
    
    
    
    
    ####---------------------- 4.1. SELECT COMPARISON  ---------------------------####
    
    # select levels for analysis ui -----------
    
    output$select_params_ui <- renderUI({
        
        fddf <- filtered_design_df()
        
        print(head(fddf))
        
        
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
            
            div(
                radioButtons(
                    inputId = "sp_select_Var",
                    label = "Select variable:",
                    choices = choices,
                    inline=T
                ),
                uiOutput("sp_select_levels"),
                uiOutput("sp_select_confirm")
            )
        } else {
            HTML("No variables are available for selection. <br>(NOTE: at least one variable must have >2 levels)")
        }
    })
    
    output$sp_select_levels <- renderUI({
        
        fddf <- filtered_design_df()
        levels <- unique(fddf[[input$sp_select_Var]])
        checkboxGroupInput(
            inputId = "sp_select_levels",
            label = "Select two levels to compare:",
            choices = levels,
            inline=T
        )
    })
    
    output$sp_select_confirm <- renderUI({
        req(length(input$sp_select_levels)==2)
        
        actionButton("sp_confirm", "Run DEG analysis")
    })
    
    
    
    ####---------------------- 4.2. CONFIRM DATA MATRIX  ---------------------------####
    
    
    ####---------------------- 4.3. RUN DEG ANALYSIS  ---------------------------####

    
    ####---------------------- DEBUG 1 ---------------------------####
    
    output$debug0 <- renderPrint({
        paste("rv$platforms = ", rv$platforms, ", "
              ,"rv$plat_id = ", rv$plat_id, ", "
              )
    })

})
