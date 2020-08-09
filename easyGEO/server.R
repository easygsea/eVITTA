
options(shiny.maxRequestSize=30*1024^2) # sets max upload size to 30 mb

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    ####---------------------- REACTIVE VALUES---------------------------####
    
    rv <- reactiveValues(gse_all = NULL
    )
    
    
    ####---------------------- HEADER DROPDOWN: SAMPLES SELECTED  ---------------------------####
    
    dropdown_report <- reactive({
        # req(is.null(rv$gse_all)==F)
        # req(is.null(rv$plat_id)==F)
        # req(is.null(rv$samples)==F)
        # req(is.null(rv$pdata)==F)

        if (is.null(rv$gse_all)==F & is.null(rv$plat_id)==F & is.null(rv$samples)==F & is.null(rv$pdata)==F){

            to_show <- translate_sample_names(rv$samples,  # translating from
                                              rv$pdata[c("title", "geo_accession")],  # translation df
                                              "title") # translating to
            if (length(to_show)>30){
                to_show_txt <- paste0(paste(to_show[1:30], collapse= ", "), "<br><i>... and ", length(to_show)-30 ," more</i>")
            } else {
                to_show_txt <- paste(to_show, collapse= ", ")
            }


            customSentence <- function(numItems, type) {
                shiny::HTML(
                    paste0("<strong>Samples selected (", length(to_show),"): </strong><br>",
                           to_show_txt)
                )
            }
        } else {
            customSentence <- function(numItems, type) {
                shiny::HTML(
                    paste0("<strong>No samples selected.</strong>")
                )
            }
        }

    })

    # actually render the dropdownMenu
    output$dropdown_menu <- renderMenu({

        dropdownMenuCustom(type = "tasks",
                           customSentence = dropdown_report()
        )
    })
    
    
    
    
    ####---------------------- 1. LOAD FROM GEO  ---------------------------####
    
    # --------------- search GEO accession ---------------
    # currently checks if input exists
    # todo: check if input format is correct (GSEXXXXXX)
    
    output$geo_accession_ui <- renderUI({
        div(
            textInput(
                inputId = "geo_accession",
                label = "GSE Accession Number:", 
                value = "GSE147507",
                placeholder = "GSE137355",
                width = "100%"
            ),
            "Note: only RNA-seq and single_channel microarray datasets are currently accepted.", br(), br(),
            
            uiOutput("search_geo")
            
        )
    })
    
    output$search_geo <- renderUI({
        if (nchar(input$geo_accession)>0){
            actionButton("search_geo", "Search")
        } else {
            "Enter a valid GEO accession number."
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
        
        # initialize the count matrix (even if it's empty) with first row = Name
        exprs <- exprs(gse()) 
        if (nrow(exprs) >0){ # put gene names onto first column
            dmdf <- cbind(Name = rownames(exprs), exprs) 
        } else {
            dmdf <- cbind(Name = character(0), exprs)
        }
        rownames(dmdf) <- c() # remove rownames
        rv$dmdf <- dmdf
        
        
        # initialize samples list
        rv$all_samples <- sampleNames(gse()) # this one stays the same
        rv$samples <- sampleNames(gse()) # this one will change
        
        # initialize pdata
        rv$pdata <- pData(phenoData(gse()))
        
        # initialize fddf
        rv$fddf <- design_df() # initially unfiltered, will update when filter

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
    
    
    
    ####---------------------- 2. DESIGN SUMMARY  ---------------------------####
    
    
    # get full design matrix table -----------#
    
    design_df <- reactive({
        req(is.null(rv$gse_all)==F)
        req(is.null(rv$plat_id)==F)
        
        # tidy characteristics
        char_list <- data.frame(t(data.frame(pData(phenoData(gse()))) %>% select(contains("characteristics"))))
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
    
    
    # summarize design -----------#
    
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
    
    # --------------- textual summary for design ---------------
    
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
            radioButtons(
                inputId = "fddf_filter_mode",
                label = "Filter mode",
                choices = c("By Predefined Variables"="variables", 
                            "Manual" = "manual"),
                inline=T),
            
            uiOutput("fddf_filter_samples"),
            uiOutput("fddf_filter_vars"),
            
            uiOutput("filter_vars_levels")

        )

    })
    
    ##### manual filter by samples -------------##
    # to carry over variable mode selections to manual mode. (but not vice versa)
    observeEvent(input$fddf_filter_mode, {
        rv$temp_samples <- rv$samples
    })
    
    
    # filter by samples
    output$fddf_filter_samples <- renderUI({
        req(input$fddf_filter_mode=="manual")
        
        all_gsms <- isolate(rv$all_samples)
        choices <- all_gsms
        if (input$fddf_show_rown == "Sample name"){
            # show the sample names instead
            names(choices) <- translate_sample_names(all_gsms,  # translating from
                                                     rv$pdata[c("title", "geo_accession")],  # translation df
                                                     "title") # translating to
        }
        
        checkboxGroupInput("filter_samples", "Filter samples by variables:",
                           choices= choices,
                           selected= rv$temp_samples, # carries over previous selection
                           inline=T
        )
    })
    
    
    ##### filter by variables -------------##
    # select vars
    output$fddf_filter_vars <- renderUI({
        req(input$fddf_filter_mode=="variables")
        
        checkboxGroupInput("filter_vars", "Filter samples by variables:",
                           choices= names(var_summary()),
                           selected= names(var_summary()),
                           inline=T
        )
    })

    # select levels
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
        req(input$fddf_filter_mode=="variables")
        
        rv$v
    })
    
    # --------------- filter design matrix ---------------
    
    # filter design matrix
    filtered_design_df <- reactive({
        req(length(input$filter_vars)>0)
        req(nchar(input$fddf_filter_mode)>0)
        
        df <- design_df()
        
        
        if (input$fddf_filter_mode== "variables"){
            
            # for each specified var, filter by specified levels.
            for (i in 1:length(input$filter_vars)){
                var <- input$filter_vars[[i]]
                levels <- input[[paste0("vs_",i)]]
                
                df <- df[(df[[var]] %in% levels),]
            }
            
        } else if (input$fddf_filter_mode== "manual"){
            
            # filter by samples
            if (length(input$filter_samples)>0){
                df <- df[input$filter_samples, ]
            }
            
        }
        
        rv$samples <- rownames(df) # update filtered samples into rv
        rv$fddf <- df # update filtered table into rv
        
        
        df
    })
    
    
    # show filtered design matrix
    output$filtered_design_df <- DT::renderDataTable({
        req(is.null(rv$gse_all)==F)
        req(is.null(rv$plat_id)==F)
        req(length(input$filter_vars)>0)
        
        df <- filtered_design_df()
        
        # translate GSM column names to sample names on display
        if (input$fddf_show_rown == "Sample name"){
            rownames(df) <- translate_sample_names(rownames(df),  # translating from
                                                   rv$pdata[c("title", "geo_accession")],  # translation df
                                                   "title") # translating to
        }
        df
        
    }, plugins="ellipsis",options=dt_options(30, scrollX=T)
    ,
    # fillContainer = T # add this to prevent header not scrolling with content
    )
    
    
    
    
    # show summary valueboxes
    output$design_variables <- renderValueBox({
        req(is.null(design_df())==F)
        
        valueBox(
            paste0(ncol(design_df()), " variables"), 
            paste0(paste(colnames(design_df()), collapse=", ")), 
            icon = icon("microscope"),
            color = "blue"
        )
    })
    output$design_samples <- renderValueBox({
        req(is.null(filtered_design_df())==F)
        req(is.null(design_df())==F)
        
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
    
    # --------------- detect data source and provide download ---------------
    
    # Data matrix source is stored in rv$sup_source
    # "table" = in gse as datatable
    # "gse_sup" = in gse as supplementary
    # "gsm_sup" = in gsm as supplementary (usually when this happens, gse_sup is also provided)
    # "none" = none of the above

    
    
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
        req(rv$sup_source == "gse_sup" | rv$sup_source == "gsm_sup" | rv$sup_source == "none")
        
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
        req(rv$sup_source == "gse_sup" | rv$sup_source == "gsm_sup" | rv$sup_source == "none")
        rv$s
    })
    
    
    # --------------- upload tidied matrix (show conditionally) ---------------
    
    
#     Full Count matrix is stored in rv$dmdf  (aka. data matrix df)
#     
#     DYNAMICS:
#         - rv$dmdf is initialized from exprs(gse()) when user selects platform. 
#     - if data is in supplementary, rv$dmdf will be initially empty (0 rows). 
#     when users upload data, the app will populate rv$dmdf by matching column names (thus column order doesn't have to be the same). 
# 	gene names will be populated into the "Name"column. 
#     - if data is in the gse to begin with (e.g. GSE137355), it will be initialized into rv$dmdf upon platform selection.
#     
#     STRUCTURE:
#     - First column is "Name", with all the gene names. (no case coercion atm).
#     - data in the rest of the columns
#     - internally, column names must be GSM accession numbers. this can be easily converted to sample names by translate_sample_names() function. 
    
    
    output$upload_matrix_ui <- renderUI({
        req(rv$sup_source == "gse_sup" | rv$sup_source == "gsm_sup" | rv$sup_source == "none")
        
        tabBox(
            title = NULL, width = 12,
            id = "upload_matrix",
            tabPanel("Upload tidied matrix", 
                     
                     fileInput("file", "Upload tidied data matrix (CSV format):",
                               accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                     ), 
                     
                     tags$hr(style="border-color: grey;"),
                     
                     strong("Note on uploading data matrix:"),br(),
                     "1) the data should be in comma-delimited format (i.e. csv),",br(),
                     "2) first row of matrix should be sample names; must match either the GEO accession or sample names,",br(),
                     "3) first column of matrix should be gene names; no duplicates are allowed,", br(),
                     "4) IMPORTANT FOR EXCEL USERS: to prevent excel auto-formatting gene names into dates, 
                     when importing data into excel, select 'do not detect data types'."
                     
            )
        )
    })
    
    # when file is uploaded, update the stored count matrix
    observeEvent(input$file, {
        inFile <- input$file
        indf <- read.csv(inFile$datapath, header=F, 
                         colClasses=c("character"))
        indf_coln <- unname(unlist(indf[1,])) # colnames = first row
        # print(indf_coln)
        indf_rown <- unname(unlist(indf[,1])) # rownames = first col
        # print(head(indf_rown))
        indf <- indf[-1,-1]
        # print(head(indf))
        
        # try to convert the indf headers into gsm format
        indf_coln <- translate_sample_names(indf_coln,  # translating from
                                            rv$pdata[c("title", "geo_accession")],  # translation df
                                                 "geo_accession") # translating to
        colnames(indf) <- indf_coln[-1]
        
        # print(head(indf))
        
        # then, match each column to rv$dmdf and update the values into it
        rvdf <- rv$dmdf
        dmdf <- data.frame(matrix(NA, nrow = nrow(indf), ncol = ncol(rvdf))) # initialize empty df
        dmdf <- data.frame(lapply(colnames(rvdf), function(x){ # update values into dmdf (leaves NA if not found)
            if (x %in% colnames(indf)){
                dmdf[[x]] <- indf[[x]]
            } else {return (rep(NA, nrow(indf)))}
        }))
        
        colnames(dmdf) <- colnames(rvdf)
        # print(head(dmdf))
        # check if all non-name cols contain any values.
        has_values <- any(unlist(lapply(dmdf[-1], function(x) any(complete.cases(x)))))
        if (has_values == T){
            dmdf$Name <- indf_rown[-1] # if has value, update first row as gene names (!!! beware of excel auto date formatting)
        } else {
            dmdf <- dmdf[0,] # if all nas, delete all rows
        }
        rownames(dmdf) <- c() # clear row names
        
        print(head(dmdf))
        
        rv$dmdf <- dmdf
    })
    
    # --------------- show data matrix df ---------------
    
    output$data_matrix_df <- DT::renderDataTable({
        req(is.null(rv$gse_all)==F)
        req(is.null(rv$plat_id)==F)
        req(is.null(rv$dmdf)==F)
        req(nchar(input$dmdf_filter))
        
        df <- rv$dmdf
        
        # filter according to stored sample list
        if (input$dmdf_filter == "Filtered"){
            df <- filtered_data_df()
        }

        # translate GSM column names to sample names on display
        if (input$dmdf_show_coln == "Sample name"){
            
            colnames(df) <- translate_sample_names(colnames(df),  # translating from
                                                   rv$pdata[c("title", "geo_accession")],  # translation df
                                                   "title") # translating to
        }

        
        df
        
    }, plugins="ellipsis",
    options=dt_options(30, scrollX=T)
    )
    
    # select whether to filter
    output$dmdf_filter_ui <- renderUI({
        req(length(rv$all_samples)>0)
        
        fm <- paste0("Full matrix (",length(rv$all_samples),")")
        fl <- paste0("Filtered (",length(rv$samples),")")
        choices <- c("Full matrix", "Filtered")
        names(choices) <- c(fm, fl)
        box(title=NULL, width = 6, solidHeader=T, status="primary",
            radioGroupButtons(
                inputId = "dmdf_filter",
                label = "Filtering options:",
                choices = choices,
                selected = "Filtered"
            ),
        )
    })
    
    # filter count matrix by selected samples
    filtered_data_df <- reactive({
        req(is.null(rv$dmdf)==F)
        req(length(rv$samples)>0)
        
        rv$dmdf[,c("Name", rv$samples)] 
    })
    
    
    
    ####---------------------- 4.1. SELECT COMPARISON  ---------------------------####
    
    # --------------- select variables and levels ---------------
    
    output$select_params_ui <- renderUI({
        
        fddf <- rv$fddf
        
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
                fluidRow(
                    column(6,
                           selectInput(
                               inputId = "sp_select_var",
                               label = "Select variable to analyze:",
                               choices = choices
                           ),
                    ),
                    column(6,
                           selectInput(
                               inputId = "sp_batch_col",
                               label = "Batch effect column:",
                               choices = c("Not applicable"="na", choices),
                               selected= "na"
                           )
                    ),
                ),
                
                uiOutput("sp_select_levels"),
                uiOutput("sp_select_confirm")
            )
        } else {
            HTML("No variables are available for selection. <br>(NOTE: at least one variable must have >2 levels)")
        }
    })
    


    
    output$sp_select_levels <- renderUI({
        
        fddf <- rv$fddf
        levels <- unique(fddf[[input$sp_select_var]])
        checkboxGroupInput(
            inputId = "sp_select_levels",
            label = "Select two levels to compare:",
            choices = levels,
            inline=T
        )
    })
    
    
    
    
    
    ####---------------------- 4.2. CONFIRM DATA MATRIX  ---------------------------####
    
    output$confirm_matrix_ui <- renderUI({
        
        div(
            uiOutput("confirm_matrix_feedback"),
            radioButtons(
                inputId = "data_type",
                label = "Type of data provided:",
                choices = c("Raw counts"="raw", "Normalized counts"="normalized"),
                inline=T
                )
        )
        
    })
    
    output$confirm_matrix_feedback <- renderUI({
        
        errors = 0
        msg = vector()
        
        # check if count matrix is empty
        if (nrow(rv$dmdf)==0){
            errors <- errors + 1
            msg = c(msg, "<strong>Data Matrix is empty. </strong><br>
            Please upload data matrix in the Data Matrix page.")
            
        } else {
            
            # check if columns are present in dmdf for the selected samples
            notfound <- setdiff(rv$samples, intersect(colnames(rv$dmdf),rv$samples))
            if (length(notfound)>0){
                errors <- errors + 1
                msg = c(msg, paste0("<strong>Selected samples are missing from data matrix: </strong><br>
            ", paste(notfound, collapse=", ")))
                
            } else {
                
                # check if the selected samples all have data in the count matrix
                checkdf <- rv$dmdf[,rv$samples]
                indx = apply(checkdf, 2, function(x) any(is.na(x)))
                has_nas <- names(indx[indx==T])
                if (length(has_nas)>0){
                    errors <- errors + 1
                    msg = c(msg, paste0("<strong>Data is incomplete for selected samples: </strong><br>
                            ", paste(has_nas, collapse=", ")))
                    
                }
                
            }
        }
        
        
        # what to show
        if(errors > 0){ 
            box_color = "red"
            msg <- paste(msg, collapse="<br><br>")
            rv$matrix_ready = F
            
        } else {
            box_color = "green"
            msg <- "<strong>Data matrix ok!</strong><br>
            Count data is complete for selected samples."
            rv$matrix_ready = T
        }
        
        
        box(
            title = NULL, background = box_color, solidHeader = TRUE, width=12,
            HTML(msg)
        )
        
    })
    
    
    
    
    
    ####---------------------- 4.3. RUN DEG ANALYSIS  ---------------------------####
    
    output$run_deg_ui <- renderUI({
        req(length(input$sp_select_levels)==2 & rv$matrix_ready==T & input$sp_select_var != input$sp_batch_col)
        
        actionButton("run_deg", "Run DEG analysis")
    })
    
    
    
    
    
    
    
    ###### ---------------- NOTES FOR JEAN ---------------- ######
    
    # Conditions for running limma: (button only shows when all fulfilled)
    # - 2 or more levels selected for comparison
    # - count matrix is uploaded
    # - columns corresponding to the filtered samples are present in the count matrix, AND do not contain *any* NA values
    # - the selected variable is not the same as selected batch effect column
    # 
    # 
    # The following reactives/ variables are provided.
    # - full count matrix: rv$dmdf
    # - filtered count matrix: filtered_data_df()
    # - full sample list: rv$all_samples
    # - filtered sample list: rv$samples
    # - full design matrix: design_df()
    # - filtered design matrix: rv$fddf
    # 
    # params:
    #     - selected variable: input$sp_select_var
    # - selected 2 levels: input$sp_select_levels
    # - batch effect col: input$sp_batch_col
    # - raw or normalized counts: input$data_type
    # 
    # Metadata:
    #     - full phenoData: rv$pdata
    # - GSE metadata as list: gse_meta()
    # - GSE metadata as df: gse_meta_df()
    # - GSM metadata as list: gsm_meta() 
    # (this only contains fields shared among all samples. e.g. organism. To access other fields that differ between samples, do subsetting on rv$pdata directly.) 
    # - GSM metadata as df: gsm_meta_df()
    # - a master df for gsm and gse metadata: all_fields() 
    # 
    # Others:
    #     - gse object for current platform: gse()
    # - summary of variables and levels: var_summary()
    # (is a named list of named vectors in form of $var level=freq)
    
    
    
    

    

    
    
    
    ####---------------------- DEBUG 1 ---------------------------####
    
    output$debug0 <- renderPrint({
        paste("rv$platforms = ", rv$platforms, ", "
              ,"rv$plat_id = ", rv$plat_id, ", "
              )
    })

})
