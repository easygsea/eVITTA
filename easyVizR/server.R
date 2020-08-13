

# ==== server.R START ===========================================================
# Define server logic 
# To access any input use input$[inputId] 
#                     ex. input$G_groups (the first select input value)
# To assign any output use output$[outputId] output$
#                      ex. output$myplot (assign the plot output)
server <- function(input, output, session) {
    
    
    
    ####---------------------- REACTIVE VALUES---------------------------####
    
    rv <- reactiveValues(upload_state = NULL, ll=ll, gg=gg, tt=tt,
                         upload_columns=NULL, df=NULL,
                         genelist=NA
                         )
    
    
    ####---------------------- HELP ---------------------------####
    # there must be an intro for each page, i.e. pre-render (n0), first page (n1), second page (n2)
    
    observeEvent(input$help_organize, {
        print(input$tabs)
        req(input$tabs == "Upload files")
        rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
            steps = intros$upload)
        )

    })
    
    observeEvent(input$help_x_pre, {
        print(input$tabs)
        req(input$tabs == "Single Dataset")
        rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                steps = intros$x0
        ))

    })
    
    observeEvent(input$help_x_post, {
        print(input$tabs)
        req(input$tabs == "Single Dataset")
        rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                                  steps = intros$x1
        ))
        
    })
    
    
    observeEvent(input$help_xy_pre, {
        print(input$tabs)
        req(input$tabs == "Two Datasets")
        rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
            steps = intros$xy0
        ))
    })
    
    observeEvent(input$help_xy_post, {
        print(input$tabs)
        req(input$tabs == "Two Datasets")
        rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                                  steps = intros$xy1
        ))
    })
    
    
    observeEvent(input$help_n_pre, {
        req(input$tabs == "Multiple Datasets")
        rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                                  steps = intros$n0)
        )
        
    })
    
    observeEvent(input$help_n_post, {
        req(input$tabs == "Multiple Datasets")
        rintrojs::introjs(session, options = list(showStepNumbers=FALSE,
                                                  steps = intros$n1)
        )
        
    })
    
    
    #======================================================================#
    ####                      ORGANIZE FILES                            ####
    #======================================================================#
    
    
    ####---------------------- Create new subset --------------------------####
    
    output$add_subset_select <- renderUI({
        selectInput(
            inputId = "add_subset_select",
            label = "Select parent dataset:",
            choices = c("Select..."="", rv$ll),
            selectize = FALSE) # show duplicate entries
    })
    
    output$add_subset_opt <- renderUI({
        req(input$add_subset_select!="")
        div(
            column(3,
            numericInput("add_subset_p", "P filter:", value = 0.05, min = 0, max = 1, step=0.001, width="100px")),
            column(3,
            numericInput("add_subset_q", "FDR filter:", value = 1, min = 0, max = 1, step=0.001, width="100px")),
            column(3,
            numericInput("add_subset_Stat", "|Stat| filter:", value = 0, min = 0, max = 5, step=0.1, width="100px")),
            column(3,
            radioGroupButtons("add_subset_sign",
                              label = "Filter Stat by:",
                              choices=c("All", "+", "-"),
                              selected="All",size="s",direction = "horizontal"))
        )
    })
    
    # constrain numericinput (???)
    observeEvent(input$add_subset_p, {
        x <- input$add_subset_p
        if (is.na(x)){
            updateNumericInput(session, "add_subset_p", value = 0.05)
        }
    })
    
    # render preview table
    output$add_subset_preview <- renderUI({
        req(input$add_subset_select!="")
        box(width=12, title = "Preview", solidHeader = F, collapsible = TRUE, status="info",
            dataTableOutput("preview_tbl", width = "100%",height="100%") 
        )
    })
    
    output$preview_tbl <- DT::renderDataTable({
        req(input$add_subset_select!="")
        
        df <- rv$preview
        # to round everything down to 3 decimals
        df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
        df
        
    }, options = list(scrollX=T, 
                      searching = FALSE, pageLength = 5))
    
    
    # input filename
    output$add_subset_name <- renderUI({
        req(input$add_subset_select!="")
        req(is.null(rv$preview_i)==F)
        req(length(rv$ll) >= rv$preview_i)
        
        if(is.null(input$add_subset_p)==F && is.null(input$add_subset_q)==F &&
           is.null(input$add_subset_Stat)==F && is.null(input$add_subset_sign)==F){
            tempname <- paste(rv$ll[[rv$preview_i]],
                              input$add_subset_p, input$add_subset_q,
                              input$add_subset_Stat, input$add_subset_sign,
                              sep="_")
        }
        else{ tempname <- "" }

        textInput(
            inputId = "add_subset_name",
            label = "Name this new dataset:",
            value= tempname)
    })
    
    
    # observe and update trimmed table
    observe({
        req(length(rv$gg)>0)
        req(input$add_subset_select!="")
        req(is.null(input$add_subset_p)==F && is.null(input$add_subset_q)==F &&
                is.null(input$add_subset_Stat)==F && is.null(input$add_subset_sign)==F)
        
        i <- match(input$add_subset_select, rv$ll)
        req(is.null(rv$gg[[i]])==F)
        df <- rv$gg[[i]]
        df <- df[df$PValue <= input$add_subset_p,]
        df <- df[df$FDR <= input$add_subset_q,]
        
        if (input$add_subset_sign=="+"){
            df <- df[df$Stat >= input$add_subset_Stat,]
        }
        else if (input$add_subset_sign=="-"){
            df <- df[df$Stat <= -input$add_subset_Stat,]
        }
        else if (input$add_subset_sign=="All"){
            df <- df[abs(df$Stat) >= input$add_subset_Stat,]
        }
        
        rv$preview_i <- i
        rv$preview <- df
    })
    
    
    
    # upload / reset buttons
    output$add_subset_confirm <- renderUI({
        req(input$add_subset_select!="")
        req(input$add_subset_p >= 0 && input$add_subset_p <= 1)
        req(input$add_subset_q >= 0 && input$add_subset_q <= 1)
        req(input$add_subset_Stat >= 0)
        req(nchar(input$add_subset_name)>0)
        
        fluidRow(
            box(width = 12, solidHeader = T,
                splitLayout(
                    actionButton('as_submit', 'Confirm and add'),
                    actionButton('as_reset', 'Reset parameters')
                )))
    })
    
    # when user presses reset
    observeEvent(input$as_reset, {
        updateNumericInput(session, "add_subset_p", value = 0.05)
        updateNumericInput(session, "add_subset_q", value = 1)
        updateNumericInput(session, "add_subset_Stat", value = 0)
        updateRadioGroupButtons(session, "add_subset_sign", selected = "All")
    })
    
    # when user presses submit
    observeEvent(input$as_submit, {
        rv$gg <- c(rv$gg,list(rv$preview))
        rv$ll <- c(rv$ll, input$add_subset_name)
        rv$tt <- c(rv$ll, rv$tt[[rv$preview_i]])
        updateSelectInput(session, "add_subset_select", selected="")
    })
    
    
    ####---------------------- batch file upload --------------------------####
    
    # when file is uploaded
    observeEvent(input$fileIn, {
        # get files
        inFiles <- input$fileIn
        inFiles <- inFiles[grepl(".csv|.txt", inFiles$name),]
        rv$batch_files <- inFiles
        
        allcols <- vector(mode="list")
        # list columns for each file
        for (i in seq_along(inFiles$name)) {
            cols <- colnames(read.csv(inFiles$datapath[[i]],nrows=1))
            allcols <- c(allcols, list(cols))
        }

        rv$upload_batch_columns <- allcols # get all col names as list
        rv$upload_batch_sharedcols <- Reduce(intersect, allcols) # get all shared col names as vector

        rv$folder_upload_state <- 'uploaded'
    })
    
    
    # select columns
    output$batch_opt_1 <- renderUI({
        req(rv$folder_upload_state == 'uploaded')
        req(length(rv$upload_batch_columns[[1]])>=4)
        fluidRow(
            box(width = 12, solidHeader = T,
                splitLayout(
                    radioButtons(
                        inputId = "batch_gene_column",
                        label = "Gene column:",
                        choices = rv$upload_batch_columns[[1]],
                        selected = firstmatch(gene_alias,rv$upload_batch_columns[[1]])
                    ),
                    radioButtons(
                        inputId = "batch_Stat_column",
                        label = "Stat column:",
                        choices = rv$upload_batch_columns[[1]],
                        selected = firstmatch(stat_alias,rv$upload_batch_columns[[1]])
                    ),
                    textInput(
                        inputId = "batch_Stat_name",
                        label = "Name this stat:",
                        #value = itemmatched(stat_alias,rv$upload_batch_columns[[1]])
                        value = firstmatch(stat_alias,rv$upload_batch_columns[[1]])
                    )
                )))
    })
    output$batch_opt_2 <- renderUI({
        req(rv$folder_upload_state == 'uploaded')
        req(length(rv$upload_batch_columns[[1]])>=4)
        fluidRow(
            box(width = 12, solidHeader = T,
                splitLayout(
                    radioButtons(
                        inputId = "batch_p_column",
                        label = "P column:",
                        choices = rv$upload_batch_columns[[1]],
                        selected = firstmatch(p_alias,rv$upload_batch_columns[[1]])
                    ),
                    radioButtons(
                        inputId = "batch_q_column",
                        label = "FDR column:",
                        choices = rv$upload_batch_columns[[1]],
                        selected = firstmatch(q_alias,rv$upload_batch_columns[[1]])
                    )
                )))
    })
    
    # first check if uploaded file list is ok
    output$batch_feedback_1 <- renderUI({
        req(rv$folder_upload_state == 'uploaded')
        
        if (nrow(rv$batch_files)==0){ # catch no supported type file error
            box(
                title = NULL, background = "red", solidHeader = TRUE, width=12,
                paste0("WARNING: folder does not contain supported type files.")
            )
        }
        else if (length(rv$upload_batch_columns[[1]])<4){ # catch first file not ok error
            box(
                title = NULL, background = "red", solidHeader = TRUE, width=12,
                paste0("WARNING: not enough columns in the first file: ",rv$batch_files$name[[1]])
            )
        }
        else{ 
            inFiles <- rv$batch_files
            box(
                title = NULL, background = "green", solidHeader = TRUE, width=12,
                paste0("File uploaded: ",paste(inFiles$name, collapse=", "), " (",nrow(inFiles)," files)")
            )
            
        }
    })

    output$batch_feedback_2 <- renderUI({
        req(rv$folder_upload_state == 'uploaded')
        req(input$batch_gene_column)
        req(input$batch_Stat_column)
        req(input$batch_p_column)
        req(input$batch_q_column)
            
        cols <- c(input$batch_gene_column, input$batch_Stat_column, input$batch_p_column, input$batch_q_column)
        if (length(unique(cols))<4){
            box(
                title = NULL, background = "yellow", solidHeader = TRUE, width=12,
                "WARNING: Did you select a column twice?"
            )
            
        }
    })
    
    output$batch_feedback_3 <- renderUI({
        req(rv$folder_upload_state == 'uploaded')
        req(input$batch_gene_column)
        req(input$batch_Stat_column)
        req(input$batch_p_column)
        req(input$batch_q_column)
        req(length(rv$upload_batch_columns[[1]])>=4)
        req(length(unique(c(input$batch_gene_column, input$batch_Stat_column, input$batch_p_column, input$batch_q_column)))>=4)
        
        failed=c()
        j <- c(input$batch_gene_column, input$batch_Stat_column, input$batch_p_column, input$batch_q_column)
        for (i in seq_along(rv$upload_batch_columns)) {
            if (all(j %in% rv$upload_batch_columns[[i]])){
                next
            }
            else{
                failed <- c(failed, rv$batch_files$name[[i]])
            }
        }
        if (is.null(failed)==F){
            rv$batch_failed <- failed
            box(
                title = NULL, background = "yellow", solidHeader = TRUE, width=12,
                paste0("Omitted because columns do not match: ",paste(failed, collapse=", "))
            )
        }
        else {
            box(
                title = NULL, background = "green", solidHeader = TRUE, width=12,
                "Files ok!"
            )
        }

    })
    
    # if there are other shared cols, also enable them to load those
    output$batch_additional_cols <- renderUI({
        req(rv$folder_upload_state == 'uploaded')
        req(input$batch_gene_column)
        req(input$batch_Stat_column)
        req(input$batch_p_column)
        req(input$batch_q_column)
        req(length(rv$upload_batch_columns[[1]])>=4)
        
        j <- c(input$batch_gene_column, input$batch_Stat_column, input$batch_p_column, input$batch_q_column)
        print(j)
        print(rv$upload_batch_sharedcols)
        additional_cols <- setdiff(rv$upload_batch_sharedcols,j)
        
        multiInput(inputId = "batch_load_other_cols",
                   label = "Load additional columns:",
                   choices = additional_cols,
                   width = "500px",
                   options = list(
                       enable_search = FALSE,
                       non_selected_header = "Click to select:",
                       selected_header = "Load these additional columns:")
        )
    })
    
    
    # render confirm and reset buttons
    output$ug_3 <- renderUI({
        req(is.null(rv$folder_upload_state)==F)
        req(rv$folder_upload_state == 'uploaded')
        req(length(rv$upload_batch_columns[[1]])>=4)
        req(length(unique(c(input$batch_gene_column, input$batch_Stat_column, input$batch_p_column, input$batch_q_column)))>=4) # ensure columns input are unique
        req(length(rv$batch_failed)!=nrow(rv$batch_files)) # make sure not all files fail
        
        fluidRow(
            box(width = 12, solidHeader = T,
                splitLayout(
                    actionButton('g_submit', 'Confirm and add'),
                    actionButton('g_reset', 'Reset')
                )))
    })
    
    # when user presses reset
    observeEvent(input$g_reset, {rv$folder_upload_state <- 'reset'})
    
    

    
    # upon submitting, add file to list of dataframes to select from
    observeEvent(input$g_submit, {
        # get files
        inFiles <- rv$batch_files
        
        # get rid of the dfs that dont fit criteria
        if(is.null(rv$batch_failed)==F){
            inFiles <- inFiles[ which( ! inFiles$name %in% rv$batch_failed) , ]
        }
        
        for (i in seq_along(inFiles$name)) {
            # print(i) # index of file
            # print(inFiles$name[[i]]) # name of file
            # print(inFiles$datapath[[i]]) # path of of file
            
            in_df <- read.csv(inFiles$datapath[[i]])
            colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$batch_gene_column, "Name")
            colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$batch_Stat_column, "Stat")
            colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$batch_p_column, "PValue")
            colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$batch_q_column, "FDR")
            
            # load only the essential columns (is it worth it to enable them to load more?)
            load_cols_list <- c(c("Name", "Stat", "PValue", "FDR"), unlist(input$batch_load_other_cols))
            print(load_cols_list)
            in_df <- in_df[,load_cols_list]
            in_df$Name <- toupper(in_df$Name)
            
            newname <- tidy_filename(inFiles$name[[i]], rv$ll)
            
            
            # write in rv
            rv$ll <- c(rv$ll, newname)
            rv$gg <- c(rv$gg, list(in_df))
            rv$tt <- c(rv$tt, input$batch_Stat_name)
        }

        rv$folder_upload_state <- "reset"
    })
    
    
    

    ####---------------------- load file --------------------------####
    
    #---------------------- events ---------------------------#
    
    # when file is uploaded, update state 
    observeEvent(input$file, {
        inFile <- input$file
        rv$upload_columns <- colnames(read.csv(inFile$datapath, nrows=1))
        rv$upload_state <- 'uploaded'
    })
    # when user presses reset, update state
    observeEvent(input$reset, {
        rv$upload_state <- 'reset'
    })
    
    # upon submitting, add file to list of dataframes to select from
    observeEvent(input$submit, {
        inFile <- input$file
        in_df <- read.csv(inFile$datapath)
        
        # replace the important column names to prevent error later on
        colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$gene_column, "Name")
        colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$Stat_column, "Stat")
        colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$p_column, "PValue")
        colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$q_column, "FDR")
        load_cols_list <- c(c("Name", "Stat", "PValue", "FDR"),input$load_other_cols)
        #print(load_cols_list)
        
        
        in_df <- in_df[,load_cols_list]
        in_df <- remove_nas(in_df)
        
        
        # set data type
        in_df$Name <- as.character(in_df$Name) # convert name column to character
        in_df$Name <- toupper(in_df$Name)
        # in_df[load_cols_list[-1]] <- sapply(in_df[load_cols_list[-1]],as.numeric) # convert all but name to numeric
        
        newname <- input$uploaded_file_name
        
        # update rv
        rv$gg <- c(rv$gg, list(in_df))
        rv$ll <- c(rv$ll, newname)
        rv$tt <- c(rv$tt, isolate(input$Stat_name))
        
        rv$upload_state <- "reset"
    })
    
    
    
    #----------------------Sidebar UI---------------------------#
    
    # THESE WILL APPEAR AFTER UPLOAD.
    
    # display uploaded file name
    output$uploaded_file <- renderUI({
        if (is.null(rv$upload_state)){return (NULL)}
        else if (rv$upload_state == 'reset'){return (NULL)}
        else if (length(rv$upload_columns)<4){
            box(
                title = NULL, background = "yellow", solidHeader = TRUE, width=12,
                paste("WARNING: columns missing: ", input$file$name)
            )
        }
        else{
            box(
                title = NULL, background = "green", solidHeader = TRUE, width=12,
                paste("Uploaded file: ", input$file$name)
            )
            
            }
    })
    
    # input filename
    output$uploaded_file_name <- renderUI({
        req(rv$upload_state == 'uploaded')
        req(length(rv$upload_columns)>=4)
        
        textInput(
            inputId = "uploaded_file_name",
            label = "Name:",
            value= tidy_filename(input$file$name, rv$ll))
    })
    
    # text instructions 
    output$cn_ins <- renderText({
        req(rv$upload_state == 'uploaded')
        req(length(rv$upload_columns)>=4)
        
        "<strong>Select columns corresponding to each element:<strong><br><br>"
    })
    
    
    
    # select columns
    output$cn_1 <- renderUI({
        req(rv$upload_state == 'uploaded')
        req(length(rv$upload_columns)>=4)
        fluidRow(
            box(width = 12, solidHeader = T,
                splitLayout(
                    radioButtons(
                        inputId = "gene_column",
                        label = "Gene column:",
                        choices = rv$upload_columns,
                        selected = firstmatch(gene_alias,rv$upload_columns) 
                    ),
                    radioButtons(
                        inputId = "Stat_column",
                        label = "Stat column:",
                        choices = rv$upload_columns,
                        selected = firstmatch(stat_alias,rv$upload_columns)
                    ),
                    textInput(
                        inputId = "Stat_name",
                        label = "Name this stat:",
                        #value = itemmatched(stat_alias,rv$upload_columns))
                        value = firstmatch(stat_alias,rv$upload_columns))
        )))
    })
    output$cn_2 <- renderUI({
        req(rv$upload_state == 'uploaded')
        req(length(rv$upload_columns)>=4)
        fluidRow(
            box(width = 12, solidHeader = T,
                splitLayout(
                    radioButtons(
                        inputId = "p_column",
                        label = "P column:",
                        choices = rv$upload_columns,
                        selected = firstmatch(p_alias,rv$upload_columns)
                    ),
                    radioButtons(
                        inputId = "q_column",
                        label = "FDR column:",
                        choices = rv$upload_columns,
                        selected = firstmatch(q_alias,rv$upload_columns)
                    )
                )))
    })
    
    # multiselect to load other columns
    output$load_other_cols <- renderUI({
        req(rv$upload_state == 'uploaded')
        req(length(rv$upload_columns)>=4)
        
        multiInput(inputId = "load_other_cols",
                   label = "Load additional columns:",
                   choices = setdiff(rv$upload_columns, c(input$gene_column, input$Stat_column,
                                                          input$p_column,input$q_column)),
                   width = "500px",
                   options = list(
                       enable_search = FALSE,
                       non_selected_header = "Click to select:",
                       selected_header = "Load these additional columns:")
        )
    })
    

    output$cn_feedback <- renderUI({
        req(rv$upload_state == 'uploaded')
        req(length(rv$upload_columns)>=4)
        
        cols <- c(input$gene_column, input$Stat_column, input$p_column, input$q_column)
        print(cols)
        if (length(unique(cols))<4){
            box(
                title = NULL, background = "yellow", solidHeader = TRUE, width=12,
                "WARNING: Did you select a column twice?"
            )
            
        }
    })
    
    # upload / reset buttons
    output$cn_3 <- renderUI({
        req(rv$upload_state == 'uploaded')
        req(length(rv$upload_columns)>=4)
        req(length(unique(c(input$gene_column, input$Stat_column, input$p_column, input$q_column)))==4)
        req(nchar(input$Stat_name)>0)
        
        fluidRow(
            box(width = 12, solidHeader = T,
                splitLayout(
                    actionButton('submit', 'Confirm and load'),
                    actionButton('reset', 'Reset')
                )))
    })
    
    
    ####---------------------- delete file --------------------------####
    
    #---------------------- events ---------------------------#
    
    observeEvent(input$delete_deg_confirm, {
        #print(input$delete_deg)
        to_delete_i <- which(rv$ll %in% input$delete_deg)
        #print(to_delete_i)
        # delete the items
        rv$ll <- rv$ll[-to_delete_i]
        rv$gg <- rv$gg[-to_delete_i]
        rv$tt <- rv$tt[-to_delete_i]
    })
    
    #---------------------- Sidebar UI ---------------------------#
    
    output$delete_deg <- renderUI({
        req(length(rv$ll) >= 1)
        multiInput(inputId = "delete_deg",
                   label = "Delete DEG list(s):",
                   choices = rv$ll,
                   width = "500px",
                   options = list(
                       enable_search = FALSE,
                       non_selected_header = "Click to select:",
                       selected_header = "Delete these:")
        )
    })
    output$delete_deg_confirm <- renderUI({
        req(length(input$delete_deg) >= 1)
        req(length(rv$ll) >= 1)
        actionButton("delete_deg_confirm", "Confirm and delete")
    })
    

    
    
    
    #======================================================================#
    ####                         SINGLE WAY                             ####
    #======================================================================#
    
    
    
    ####---------------------- Events ---------------------------####
    
    # update variables
    observe({
        if(is.null(input$df_x_p)==F) {rv$df_x_p <- input$df_x_p}
        if(is.null(input$df_x_q)==F) {rv$df_x_q <- input$df_x_q}
        if(is.null(input$df_x_Stat)==F) {rv$df_x_Stat <- input$df_x_Stat}
        
        if(is.null(input$fs_volcano_p)==F) {rv$fs_volcano_p <- input$fs_volcano_p}
        if(is.null(input$fs_volcano_Stat)==F) {rv$fs_volcano_Stat <- input$fs_volcano_Stat}
        
        if(is.null(input$df_gl_p)==F) {rv$df_gl_p <- input$df_gl_p}
        if(is.null(input$df_gl_q)==F) {rv$df_gl_q <- input$df_gl_q}
        if(is.null(input$df_gl_Stat)==F) {rv$df_gl_Stat <- input$df_gl_Stat}
        
        if(is.null(input$gl_volcano_p)==F) {rv$gl_volcano_p <- input$gl_volcano_p}
        if(is.null(input$gl_volcano_Stat)==F) {rv$gl_volcano_Stat <- input$gl_volcano_Stat}
        
        if(is.null(input$p1_bar_data)==F) {rv$p1_bar_data <- input$p1_bar_data}
        if(is.null(input$p1_bar_sig)==F) {rv$p1_bar_sig <- input$p1_bar_sig}
        
    })
    
    # update current view
    current<- reactive({
        if (is.null(input$show_df)){return(1)}
        else {return({match(input$show_df,rv$ll)})}
    })
    
    
    
    
    # submit full data
    observeEvent(input$submit_x,{
        #print("submit full data: mode")
        
        rv$mode <- isolate(input$mode)
        withProgress(message = 'Updating data...', value = 0, {
            incProgress(0.2)
            
            df <- isolate(rv$gg[[current()]])
            # set data type
            df$Name <- as.character(df$Name) # convert name column to character
            df[-1] <- sapply(df[-1],as.numeric) # convert all but name to numeric
            
            
            
            incProgress(0.2)
            
            # initialize graphing params
            rv$show_df <- isolate(input$show_df)
            rv$x_i <- isolate(current())
            
            rv$df_x_p <- 0.05
            rv$df_x_q <- 1
            rv$df_x_Stat <- 0
            
            rv$fs_volcano_p <-0.05
            rv$fs_volcano_Stat <-0
            rv$volcano_xmax <- max(abs(df$Stat), na.rm = TRUE)+0.5 # find max of Stat
            rv$volcano_ymax <- max(-log10(df$PValue), na.rm = TRUE)+0.5 # find max of -log10p
            incProgress(0.2)
            
            rv$df <- df
            incProgress(0.2)
        })
    })
    
    # submit gene list
    observeEvent(input$submit_genelist,{
        #print("submit genelist: mode")
        
        rv$mode <- isolate(input$mode)
        rv$genelist <- isolate(as.list(strsplit(input$genelist_p1, '\\n+'))) 
        withProgress(message = 'Updating data...', value = 0, {
            incProgress(0.2)
            
            df <- isolate(rv$gg[[current()]])
            incProgress(0.2)
            
            # initialize graphing params
            rv$show_df <- isolate(input$show_df)
            rv$x_i <- isolate(current())
            
            rv$df_gl_p <- 1
            rv$df_gl_q <- 1
            rv$df_gl_Stat <- 0
            
            rv$gl_volcano_p <-0.05
            rv$gl_volcano_Stat <-0
            rv$volcano_xmax <- max(abs(df$Stat), na.rm = TRUE)+0.5 # find max of Stat
            rv$volcano_ymax <- max(-log10(df$PValue), na.rm = TRUE)+0.5 # find max of -log10p
            
            # bar params
            rv$gl_cols <- isolate(colnames(rv$gg[[current()]])[-1])
            rv$p1_bar_data <- rv$gl_cols[[1]]
            rv$p1_bar_sig <- "PValue"
            
            incProgress(0.2)
            
            # trim by gene list
            #print(rv$genelist[[1]])
            df <- df[df$Name %in% rv$genelist[[1]],]
            df <- df[order(match(df$Name, rv$genelist[[1]])), ]
            incProgress(0.2)
            #print(head(df))
            rv$df <- df

        })
    })
    
    
    
    ####---------------------- Sidebar ---------------------------####

    
    #select data to show
    output$select_df <- renderUI({
        selectInput(
            inputId = "show_df",
            label = "Select from uploaded datasets:",
            choices = rv$ll,
            selectize = FALSE) # show duplicate entries
    })
    
    # select subset mode
    output$deg_subset_mode <- renderUI({
        req(length(rv$ll) >= 1)
        req(is.null(input$show_df)==F)
        radioButtons(
            inputId = "mode",
            label = "Subset data:",
            choices = c("All genes", "List of genes"))
    })
    
    # show subset options and submit button
    output$x_confirm <- renderUI({
        req(length(rv$ll) >= 1)
        req(is.null(input$show_df)==F)
        req(is.null(input$mode)==F)
        
        if(input$mode== "All genes"){
            actionButton("submit_x", "Visualize!")
        }
        else if(input$mode== "List of genes"){
            div(
                textAreaInput("genelist_p1", "Input gene list (separated by new line):", ""),
                uiOutput("submit_genelist")
            )
        }
    })
    output$submit_genelist <- renderUI({
        req(is.null(input$genelist_p1)==F)
        req(nchar(input$genelist_p1)>0)
        
        actionButton("submit_genelist", "Visualize!")
    })
    
    
    ####----------------------Main Tabs---------------------------####
    
    output$x_header <- renderUI({
        req(is.null(rv$mode)==T)
        fluidRow(
            column(12, align="right",
                   div(style="display:inline-block",
                       
                       actionBttn(
                           inputId = "help_x_pre", label=NULL, 
                           icon = icon("question"), style="material-circle", color="primary"
                       ),
                   )
            )
        )
    })
    
    
    output$single_panels <- renderUI({
        if(is.null(rv$mode)==T){
            div(
            box(
                title = span( icon("exclamation"), "Notification"), status = "warning", width=8,
                "No data selected."
            )
            )
        }
        
        else if (rv$mode == "All genes"){
            div(
                fluidRow(
                    column(12, align="right",
                           div(style="display:inline-block",
                               
                               actionBttn(
                                   inputId = "help_x_post", label=NULL, 
                                   icon = icon("question"), style="material-circle", color="primary"
                               ),
                           )
                    )
                ),
                fluidRow(
                    column(6,
                           box(
                               title = span( icon("chart-area"), "Volcano"), status = "primary", solidHeader = TRUE, width=12,
                               plotlyOutput("p1_fs_volcano",
                                            width = "100%",height = "400px"),
                               
                               div(style = "position: absolute; left: 1em; bottom: 1em",
                                   dropdown(
                                       sliderTextInput("fs_volcano_p",
                                                       label = "Select P threshold:",
                                                       choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                                       selected=0.05, grid=T, force_edges=T),
                                       sliderInput("fs_volcano_Stat",
                                                   label = "Select |Stat| threshold:",
                                                   min=0, max=5, step=0.1, value=0)
                                       
                                       ,
                                       size = "xs",
                                       icon = icon("gear", class = "opt"),
                                       up = TRUE, width=300
                                   )
                               ),
                               div(style = "position: absolute; left: 4em; bottom: 1em",
                                   dropdown(
                                       downloadButton("x_vol_dl", "Download plot")
                                       ,
                                       size = "xs",
                                       icon = icon("download", class = "opt"),
                                       up = TRUE
                                   )
                               )
                           ),
                           
                           ),
                    column(6,
                           box(
                               title = span( icon("chart-area"), "Bar"), status = "warning", solidHeader = TRUE, width=12,
                               
                               "Bar plot is only available for gene list mode."
                           ),
                           box(
                               title = span( icon("table"), "Table"), status = "primary", solidHeader = TRUE, width=12,
                               dataTableOutput("single_tbl", width = "100%",height="100%"),
                               
                               div(style = "position: absolute; left: 1em; bottom: 1em",
                                   dropdown(
                                       sliderTextInput("df_x_p",
                                                       label = "Select P cutoff:",
                                                       choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                                       selected=0.05, grid=T, force_edges=T),
                                       sliderTextInput("df_x_q",
                                                       label = "Select P cutoff:",
                                                       choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                                       selected=1, grid=T, force_edges=T),
                                       sliderInput("df_x_Stat", "Select |Stat| cutoff:", min = 0, max = 5,
                                                   value = 0, step = 0.25)
                                       ,
                                       size = "xs",
                                       icon = icon("cut", class = "opt"),
                                       up = TRUE, width=300
                                   )
                               ),
                               div(style = "position: absolute; left: 4em; bottom: 1em;",
                                   dropdown(
                                       downloadButton("downloaddf","Download table"),
                                       downloadButton("downloadrnk","Download RNK")
                                       ,
                                       size = "xs",
                                       icon = icon("download", class = "opt"),
                                       up = TRUE
                                   )
                               ),
                           
                           
                           )
                )
                
                
                )
            )
        }
        else if (rv$mode == "List of genes"){
            div(
                fluidRow(
                    column(12, align="right",
                           div(style="display:inline-block",
                               
                               actionBttn(
                                   inputId = "help_x_post", label=NULL, 
                                   icon = icon("question"), style="material-circle", color="primary"
                               ),
                           )
                    )
                ),
                fluidRow(
                    column(6,
                           box(
                               title = span( icon("chart-area"), "Volcano"), status = "primary", solidHeader = TRUE, width=12,
                               plotlyOutput("p1_gl_volcano",
                                            width = "100%",height = "400px"),
                               
                               div(style = "position: absolute; left: 1em; bottom: 1em",
                                   dropdown(
                                       sliderTextInput("gl_volcano_p",
                                                       label = "Select P threshold:",
                                                       choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                                       selected=0.05, grid=T, force_edges=T),
                                       sliderInput("gl_volcano_Stat",
                                                   label = "Select |Stat| threshold:",
                                                   min=0, max=5, step=0.1, value=0)
                                       
                                       ,
                                       size = "xs",
                                       icon = icon("gear", class = "opt"),
                                       up = TRUE, width=300
                                   )
                               )
                           ),
                           ),
                    column(6,
                           box(
                               title = span( icon("chart-area"), "Bar"), status = "primary", solidHeader = TRUE, width=12,
                               uiOutput("gl_bar_fig"),
                               div(style = "position: absolute; left: 1em; bottom: 1em",
                                   dropdown(
                                       radioButtons(
                                           inputId = "p1_bar_data",
                                           label = "Data to plot in bar graph:",
                                           choices = rv$gl_cols, #can select any column except gene name
                                           selected = rv$gl_cols[[1]]
                                       ),
                                       radioButtons(
                                           inputId = "p1_bar_sig",
                                           label = "Color by significance:",
                                           choices = c("PValue","FDR"),
                                           selected = "PValue"
                                       )
                                       
                                       ,
                                       size = "xs",
                                       icon = icon("gear", class = "opt"),
                                       up = TRUE, width=200
                                   )
                               ),
                               div(style = "position: absolute; left: 4em; bottom: 1em",
                                   dropdown(
                                       downloadButton("x_bar_dl", "Download plot")
                                       ,
                                       size = "xs",
                                       icon = icon("download", class = "opt"),
                                       up = TRUE
                                   )
                               )
                           ),
                           box(
                               title = span( icon("table"), "Table"), status = "primary", solidHeader = TRUE, width=12,
                               dataTableOutput("single_gl_tbl", width = "100%",height="100%"),
                               
                               div(style = "position: absolute; left: 1em; bottom: 1em",
                                   dropdown(
                                       sliderTextInput("df_gl_p",
                                                       label = "Select P cutoff:",
                                                       choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                                       selected=1, grid=T, force_edges=T),
                                       sliderTextInput("df_gl_q",
                                                       label = "Select P cutoff:",
                                                       choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                                       selected=1, grid=T, force_edges=T),
                                       sliderInput("df_gl_Stat", "Select |Stat| cutoff:", min = 0, max = 5,
                                                   value = 0, step = 0.25)
                                       ,
                                       size = "xs",
                                       icon = icon("cut", class = "opt"),
                                       up = TRUE, width=300
                                   )
                               ),
                               div(style = "position: absolute; left: 4em; bottom: 1em;",
                                   dropdown(
                                       downloadButton("downloadgldf","Download table"),
                                       downloadButton("downloadrnk","Download RNK")
                                       ,
                                       size = "xs",
                                       icon = icon("download", class = "opt"),
                                       up = TRUE
                                   )
                               ),
                           
                           )
                )
                
                
                )
            )
        }
    })
    
 
    
    ####================= SINGLE VISUALIZATIONS =====================####
    
    
    ####--------------------Table------------------------####
    
    # download_df <- function(name, tag, df){
    #     downloadHandler(
    #         filename = function() {
    #             paste(tag, "-", name, "-", Sys.Date(), ".csv", sep="")},
    #         content = function(file) {
    #             write.csv(df, file, 
    #                       row.names = FALSE, quote=FALSE)})
    # }
    
    # don't use these functions; they don't update after the first time
    # download current df
    # output$downloaddf <- download_df(rv$show_df, "data", {
    #     df <- rv$df
    #     df <- subset(df, PValue <= rv$df_x_p)
    #     df <- subset(df, FDR <= rv$df_x_q)
    #     df <- subset(df, abs(Stat) >= rv$df_x_Stat)
    #     names(df)[names(df) == 'Stat'] <- rv$tt[[rv$x_i]]
    #     df
    # })
    
    output$downloaddf <- downloadHandler(
        filename = function() {
            paste("data", "-", rv$show_df, "-", Sys.Date(), ".csv", sep="")},
        content = function(file) {
            write.csv({
                
                df <- rv$df
                df <- subset(df, PValue <= rv$df_x_p)
                df <- subset(df, FDR <= rv$df_x_q)
                df <- subset(df, abs(Stat) >= rv$df_x_Stat)
                names(df)[names(df) == 'Stat'] <- rv$tt[[rv$x_i]]
                df
                
            }, file, 
                      row.names = FALSE, quote=T)})
        
        
        

    
    # download current gl df
    output$downloadgldf <- downloadHandler(
        filename = function() {
            paste("subset", "-", rv$show_df, "-", Sys.Date(), ".csv", sep="")},
        content = function(file) {
            write.csv({
                
                df <- rv$df
                df <- subset(df, PValue <= rv$df_gl_p)
                df <- subset(df, FDR <= rv$df_gl_q)
                df <- subset(df, abs(Stat) >= rv$df_gl_Stat)
                names(df)[names(df) == 'Stat'] <- rv$tt[[rv$x_i]]
                df
                
            }, file, 
                      row.names = FALSE, quote=T)})
        

    
    
    # download rnk 
    output$downloadrnk <- downloadHandler(
        filename = function() {
            paste("RNK-", rv$show_df, "-", Sys.Date(), ".rnk", sep="")},
        content = function(file) {
            output_file <- rv$df
            output_file <- append_rank(output_file)
            output_file <- output_file[,c("Name","Rank")]
            write.table(output_file, file, 
                        row.names=FALSE, quote=F,sep="\t")})

    
    
    # show table
    output$single_tbl <- DT::renderDataTable({
        req(length(rv$ll) >= 1)
        req(rv$df)
        
        df <- rv$df
        df <- subset(df, PValue <= rv$df_x_p)
        df <- subset(df, FDR <= rv$df_x_q)
        df <- subset(df, abs(Stat) >= rv$df_x_Stat)
        
        names(df)[names(df) == 'Stat'] <- rv$tt[[rv$x_i]]
        
        # to round everything down to 3 decimals
        df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
        
        df
    }, 
    plugins = "ellipsis",
    options = list(scrollX=TRUE, 
                   columnDefs = list(
                       list(
                           targets = 1,
                           render = JS("$.fn.dataTable.render.ellipsis( 17, true )")
                       ),
                       list(
                           targets = "_all",
                           render = JS("$.fn.dataTable.render.ellipsis( 6, true )")
                       )
                   ))
    )
    
    

    
    
    # show gl table
    output$single_gl_tbl <- DT::renderDataTable({
        req(length(rv$ll) >= 1)
        req(rv$df)
        
        df <- rv$df
        df <- subset(df, PValue <= rv$df_gl_p)
        df <- subset(df, FDR <= rv$df_gl_q)
        df <- subset(df, abs(Stat) >= rv$df_gl_Stat)
        
        print(rv$x_i)
        print(rv$tt)
        colnames(df)[colnames(df) == 'Stat'] <- rv$tt[[rv$x_i]]
        
        # to round everything down to 3 decimals
        df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
        
        df
    })
    
    
    
    ####-----------------single fs volcano ----------------####
    
    
    # volcano graph 
    x_vol_plt <- reactive({
        df_p <- rv$df
        df_p[df_p==0]<-0.00001 # replace 0 with 0.001
        df_p$color <- ifelse(df_p$PValue <rv$fs_volcano_p & abs(df_p$Stat)>rv$fs_volcano_Stat, "red", "gray")
        
        fs_volcano <- plot_ly(
            data = df_p, 
            x = df_p$Stat,
            y = -log10(df_p$PValue),
            mode = 'markers', marker = list(color = df_p$color),
            hoverinfo="text",
            text=c(paste(df_p$Name, 
                         "<br>",rv$tt[[rv$x_i]],":", as.character(round(df_p$Stat, 3)),
                         "<br>PValue:", as.character(round(df_p$PValue, 3)),
                         "<br>FDR:", as.character(round(df_p$FDR, 3))
            ))
        )
        
        fs_volcano <- fs_volcano %>% layout(title = paste0("Volcano plot of ", rv$show_df, " (n=",nrow(df_p),")"),
                                            yaxis = list(zeroline = T, title="-log10(PValue)",
                                                         range=c(0,rv$volcano_ymax)),
                                            xaxis = list(zeroline = T, title=rv$tt[[rv$x_i]], 
                                                         range=c(-rv$volcano_xmax,rv$volcano_xmax)))
        
        return(fs_volcano)
    })
    output$p1_fs_volcano <- renderPlotly({
        req(length(rv$ll) >= 1)
        req(rv$df)

        x_vol_plt()
    })
    
    # download plotly html graph
    output$x_vol_dl <- downloadHandler(
        filename = function() {paste("volcano-single-", Sys.Date(), ".html", sep = "")},
        content = function(file) {saveWidget(as_widget(x_vol_plt()), file, selfcontained = TRUE)})

    
    ####-----------------single gl bar plot----------------####
    

    
    output$gl_bar_fig <- renderUI({
        req(length(rv$ll) >= 1)
        req(rv$df)

        #req(input$gl_plottype=='Bar plot')
        plotlyOutput(
            outputId = "p1_bar",
            width = "100%",
            height = "400px"
        )
    })
    
    # bar graph
    x_bar_plt <- reactive({
        df_p <- rv$df
        df_p[df_p==0]<-0.00001 # replace 0 with 0.001
        df_p$color <- -log10(as.numeric(df_p[[rv$p1_bar_sig]]))
        print(head(df_p))
        fig <- plot_ly(
            x = df_p$Name,
            y = df_p[[rv$p1_bar_data]],
            name = "Expression bar plot",
            type = "bar",
            hoverinfo="text",
            marker = list(
                colorscale=cscale,
                color = df_p$color,
                colorbar=list(title=paste0("-log10(",rv$p1_bar_sig, ")")),
                cauto = F,cmin = 0,cmax = 3
            ),
            text=c(paste(df_p$Name, 
                         "<br>",rv$tt[[rv$x_i]],":", as.character(round(df_p$Stat, 3)),
                         "<br>PValue:", as.character(round(df_p$PValue, 3)),
                         "<br>FDR:", as.character(round(df_p$FDR, 3))))
            
            
        )
        return(fig)
    })
    
    output$p1_bar <- renderPlotly({
        req(length(rv$ll) >= 1)
        req(rv$df)
        
        x_bar_plt()
    })
    
    # download plotly html graph
    output$x_bar_dl <- downloadHandler(
        filename = function() {paste("bar-single-", Sys.Date(), ".html", sep = "")},
        content = function(file) {saveWidget(as_widget(x_bar_plt()), file, selfcontained = TRUE)})
    
    
    ####------------------single gl volcano---------------------####
    
    
    # volcano graph 
    output$p1_gl_volcano <- renderPlotly({
        req(length(rv$ll) >= 1)
        req(rv$df)
 
        df_p <- rv$df
        df_p[df_p==0]<-0.00001 # replace 0 with 0.001
        df_p$color <- ifelse(df_p$PValue <rv$gl_volcano_p & abs(df_p$Stat)>rv$gl_volcano_Stat, "red", "gray")
        
        gl_volcano <- plot_ly(
            data = df_p, 
            x = df_p$Stat,
            y = -log10(df_p$PValue),
            mode = 'markers', marker = list(color = df_p$color),
            hoverinfo="text",
            text=c(paste(df_p$Name, 
                         "<br>",rv$tt[[rv$x_i]],":", as.character(round(df_p$Stat, 3)),
                         "<br>PValue:", as.character(round(df_p$PValue, 3)),
                         "<br>FDR:", as.character(round(df_p$FDR, 3)))
                   )
            )
        gl_volcano <- gl_volcano %>% layout(title = paste0("Volcano plot of subset of ", rv$show_df, " (n=",nrow(df_p),")"),
                                            yaxis = list(zeroline = T, title="-log10(PValue)",
                                                         range=c(0, rv$volcano_ymax)),
                                            xaxis = list(zeroline = T, title=rv$tt[[rv$x_i]],
                                                         range=c(-rv$volcano_xmax,rv$volcano_xmax)))
        return(gl_volcano)
    })
    

    
    
    #======================================================================#
    ####                         TWO WAY                                ####
    #======================================================================#
    
    ####---------------------- Events ---------------------------####
    
    # update params
    observe({
        if(is.null(input$xy_p)==F){rv$xy_p <- input$xy_p}
        if(is.null(input$xy_q)==F){rv$xy_q <- input$xy_q}
        if(is.null(input$xy_Stat)==F){rv$xy_Stat <- input$xy_Stat}
        
        if(is.null(input$xy_sc_p)==F){rv$xy_sc_p <- input$xy_sc_p}
        if(is.null(input$xy_sc_q)==F){rv$xy_sc_q <- input$xy_sc_q}
        if(is.null(input$xy_sc_Stat)==F){rv$xy_sc_Stat <- input$xy_sc_Stat}
        if(is.null(input$xy_colormode)==F){rv$xy_colormode <- input$xy_colormode}
        if(is.null(input$xy_sig)==F){rv$xy_sig <- input$xy_sig}
        if(is.null(input$xy_thresh)==F){rv$xy_thresh <- input$xy_thresh}
        if(is.null(input$xy_sc_size)==F){rv$xy_sc_size <- input$xy_sc_size}
        if(is.null(input$xy_sc_logic)==F){rv$xy_sc_logic <- input$xy_sc_logic}
        
        
    })
    
    
    # observe and pull out shared cols and rows among input dfs (as lists)
    observe({
        req(is.null(input$selected_x)==F)
        req(is.null(input$selected_y)==F)
        
        selected <- c(input$selected_x, input$selected_y)
        observed <- detect_shared_dimensions(selected, rv$gg, rv$ll, input_mode="names")
        rv$xy_sharedcols <- observed$shared_cols
        rv$xy_sharedrows <- observed$shared_rows
    })
    
    
    # initialize
    observeEvent(input$xy_confirm, {
        req(is.null(input$selected_x)==F)
        req(is.null(input$selected_y)==F)
        
        rv$df_xy <- NULL
        withProgress(message = 'Updating data...', value = 0, {
            
            # df_x <- isolate(rv$gg[[match(input$selected_x, rv$ll)]])
            # incProgress(0.2)
            # df_y <- isolate(rv$gg[[match(input$selected_y, rv$ll)]])
            # incProgress(0.2)
            # df_xy <- merge(df_x,df_y, by = "Name")
            # incProgress(0.2)
            selected <- c(input$selected_x, input$selected_y)
            df_xy <- build_df_n(selected, rv$gg, rv$ll, input_mode="names")
            
            # initialize cor line
            statx <- paste0("`Stat_", selected[[1]],"`") # back ticks to escape possible illegal punctuation
            staty <- paste0("`Stat_", selected[[2]],"`")
            lm_fun <- paste(statx, staty, sep = " ~ ")
            print(lm_fun)
            print(head(df_xy))
            rv$fit_xy <- lm(as.formula(lm_fun), data = df_xy)
            
            
            
            # initialize params
            rv$xyx_i <- isolate(match(input$selected_x, rv$ll))
            rv$xyy_i <- isolate(match(input$selected_y, rv$ll))
            
            rv$xy_p <- 0.25
            rv$xy_q <- 1
            rv$xy_Stat <- 0.25
            
            rv$selected_x <- isolate(input$selected_x)
            rv$selected_y <- isolate(input$selected_y)
            
            rv$xy_sc_p <- 0.25
            rv$xy_sc_q <- 1
            rv$xy_sc_Stat <- 0
            rv$xy_colormode <- "None"
            rv$xy_sig <- "PValue"
            rv$xy_thresh <- 0.05
            rv$xy_sc_size <- 3
            rv$xy_sc_logic <- "Both"
            
            incProgress(0.2)
        })
        rv$df_xy <- df_xy
        print(head(rv$df_xy))
        
    })
    
    

    
    
    ####---------------------- Sidebar ---------------------------####
    
    # select x and y
    output$select_x <- renderUI({
        selectizeInput(
            inputId = "selected_x",
            label = "Select dataset 1:",
            choices = rv$ll)
    })
    output$select_y <- renderUI({
        selectizeInput(
            inputId = "selected_y",
            label = "Select dataset 2:",
            choices = rv$ll)
    })
    
    # feedback on whether the data has enough shared rows/cols
    output$xy_shared <- renderUI({
        req(is.null(rv$xy_sharedcols)==F)
        req(is.null(rv$xy_sharedrows)==F)
        
        if (length(rv$xy_sharedcols)>=1){msgx=" (ok)"}
        else{ msgx=""}
        if (length(rv$xy_sharedrows)>=1){msgy=" (ok)"}
        else{ msgy=""}
        
        if (msgx==" (ok)" & msgy==" (ok)"){
            box(
                title = NULL, background = "green", solidHeader = TRUE, width=12,
                paste0("Shared columns: ",length(rv$xy_sharedcols), msgx),br(),
                paste0("Shared rows: ",length(rv$xy_sharedrows), msgy)
            )
        }
        else{
            box(
                title = NULL, background = "red", solidHeader = TRUE, width=12,
                paste0("Shared columns: ",length(rv$xy_sharedcols), msgx),br(),
                paste0("Shared rows: ",length(rv$xy_sharedrows), msgy)
            )
        }
    })
    
    

    
    # confirm and submit
    output$xy_confirm <- renderUI({
        req(rv$xy_sharedcols>=1)
        req(rv$xy_sharedrows>=1)
        req(input$selected_x != input$selected_y)
        
        actionButton("xy_confirm", "Visualize!")
    })
    
    
    
    
    ####================= TWO WAY VISUALIZATIONS =====================####
    
    
    filtered_df_xy <- reactive({
        req(is.null(rv$df_xy)==F)
        req(is.null(rv$selected_x)==F)
        req(is.null(rv$selected_y)==F)
        
        df_xy <- rv$df_xy
        selected <- c(rv$selected_x, rv$selected_y)
        for (i in selected){
            df_xy <- apply_single_cutoff(df_xy, i, rv$xy_p, rv$xy_q, rv$xy_Stat, tolerate=F)
        }
        
        df_xy
    })
    
    ####--------------- table -------------------####
    
    # show df_xy table
    output$xy_tbl <- DT::renderDataTable({
        req(rv$df_xy)
        
        
        df <- filtered_df_xy()
        
        
        rv$df_xy_fullcols <- colnames(df)
        
        # to abbreviate the long column names...take first 5 letters
        char_limit <- 56 / length(colnames(df))
        # print(char_limit)
        colnames(df) <- sapply(names(df), function(x){
            if (nchar(x)>char_limit)
            {return (paste0(substr(x, start = 1, stop = char_limit),"..."))}
            else{return (x)}
        })
        
        
        # to replace the stat col names 
        colnames(df) <- gsub("Stat", rv$tt[[rv$xyx_i]], colnames(df))

        # to round everything down to 3 decimals
        df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
        
        
        # print(head(df))
        df
        
    }, plugins = "ellipsis",
    options = list(scrollX=TRUE, 
                   columnDefs = list(
                       list(
                           targets = 1,
                           render = JS("$.fn.dataTable.render.ellipsis( 17, true )")
                       ),
                       list(
                           targets = "_all",
                           render = JS("$.fn.dataTable.render.ellipsis( 6, true )")
                       )
                   ),
                   headerCallback= JS("function(thead, data, start, end, display){",
                                      sprintf("  var tooltips = [%s];", toString(paste0("'", rv$df_xy_fullcols, "'"))),
                                      "  for(var i = 1; i <= tooltips.length; i++){",
                                      "    $('th:eq('+i+')',thead).attr('title', tooltips[i-1]);",
                                      "  }",
                                      "}"))
    )
    
    # download table
    output$xy_tbl_dl <- downloadHandler(
        filename = function() {paste0("data-",Sys.Date(),"-",input$selected_x,"_vs_",input$selected_y,".csv")},
        content = function(file) {
            
            output_file <- filtered_df_xy()
            
            write.csv(output_file, file, 
                      row.names = FALSE, quote=T)})
    
    
    ####--------------- scatter plot -------------------####
    
    
    
    # color mode
    output$xy_colormode <- renderUI({
        req(is.null(rv$df_xy)==F)
        radioButtons(
            inputId = "xy_colormode",
            label = "Represent significance by:",
            choices = c("None", "Two colors", "Color and size"))
    })
    output$xy_sig <- renderUI({
        req(is.null(rv$df_xy)==F)
        req(rv$xy_colormode !="None")
        radioButtons(
            inputId = "xy_sig",
            label = "Significance:",
            choices = c("PValue", "FDR"),
            selected="PValue")
    })
    output$xy_thresh <- renderUI({
        req(rv$xy_colormode =="Two colors")
        
        numericInput("xy_thresh", 
                     "Threshold:", value = 0.05, min = 0, max = 1, step=0.001, width="100px")
    })
    
    
    # plotly scatter
    xy_sc_plt <- reactive({
        req(is.null(rv$df_xy)==F)
        req(is.null(rv$selected_x)==F)
        req(is.null(rv$selected_y)==F)
        
        # withProgress(message = 'Making graph...', value = 0, {
            print(dim(rv$df_xy[[1]]))
            
            df_p <- rv$df_xy
            
            selected <- c(rv$selected_x, rv$selected_y)
            xsig <- paste(rv$xy_sig, selected[[1]], sep="_")
            ysig <- paste(rv$xy_sig, selected[[2]], sep="_")
            xstat <- paste("Stat", selected[[1]], sep="_")
            ystat <- paste("Stat", selected[[2]], sep="_")
            xp <- paste("PValue", selected[[1]], sep="_")
            yp <- paste("PValue", selected[[2]], sep="_")
            xq <- paste("FDR", selected[[1]], sep="_")
            yq <- paste("FDR", selected[[2]], sep="_")
            
            
            
            df_p[df_p==0]<-0.00001 # replace 0 with 0.001
            # cutoffs
            x_filtered <- apply_single_cutoff(df_p, selected[[1]], p=rv$xy_sc_p, q=rv$xy_sc_q, stat=rv$xy_sc_Stat, tolerate=F)
            y_filtered <- apply_single_cutoff(df_p, selected[[2]], p=rv$xy_sc_p, q=rv$xy_sc_q, stat=rv$xy_sc_Stat, tolerate=F)
            print(head(x_filtered))
            if (rv$xy_sc_logic == "Both"){
                df_p <- df_p[df_p$Name %in% intersect(x_filtered$Name, y_filtered$Name), ]
                # df_p <- df_p %>% filter(PValue.x < rv$xy_sc_p & PValue.y < rv$xy_sc_p)
                # df_p <- df_p %>% filter(FDR.x < rv$xy_sc_q & FDR.y < rv$xy_sc_q)
                # df_p <- df_p %>% filter(abs(Stat.x) > rv$xy_sc_Stat & abs(Stat.y) > rv$xy_sc_Stat)
            } else if (rv$xy_sc_logic == "Either"){
                # df_p <- df_p %>% filter(PValue.x < rv$xy_sc_p | PValue.y < rv$xy_sc_p)
                # df_p <- df_p %>% filter(FDR.x < rv$xy_sc_q | FDR.y < rv$xy_sc_q)
                # df_p <- df_p %>% filter(abs(Stat.x) > rv$xy_sc_Stat | abs(Stat.y) > rv$xy_sc_Stat)
                df_p <- df_p[df_p$Name %in% union(x_filtered$Name, y_filtered$Name), ]
            }
            
            df_p <- remove_nas(df_p) # when using Either mode, NA might slip by. 
            # need to delete NA rows before graphing, although those can show up in table.
            
            req(nrow(df_p)>0)
            
            incProgress(0.2)
            
            # initialize marker settings as none
            df_p$color <- "black"
            df_p$color <- as.factor(df_p$color)
            df_p$size <- rv$xy_sc_size+2 # initialized to 5
            marker_settings <- list(
                color= df_p$color, size= df_p$size, 
                line = list(color = 'white', width = 0))
            
            incProgress(0.2)
            
            if (rv$xy_colormode== "Two colors"){ # is this a good idea????
                #print(head(df_p))
                
                df_p$color <- as.character(df_p$color)
                df_p$color[which(df_p[[xsig]] < rv$xy_thresh | df_p[[ysig]] < rv$xy_thresh)] <- "red"
                df_p$color <- as.factor(df_p$color)
                
                df_p$size <- rv$xy_sc_size+2 # initialized to 5
                marker_settings <- list(
                    color= df_p$color, size= df_p$size, 
                    line = list(color = 'white', width = 1))
            }
            else if (rv$xy_colormode== "Color and size"){
                df_p[df_p==0]<-0.00001 # replace 0 with 0.001
                df_p$color <- -log10(as.numeric(df_p[[xsig]]))
                df_p$color <- as.numeric(df_p$color)
                df_p$size <- -log10(as.numeric(df_p[[ysig]]))* (rv$xy_sc_size-1) + 3 # initialized to 2+3
                #print(head(df_p))
                marker_settings <- list(
                    color= df_p$color, size= df_p$size,
                    opacity=.7, line = list(color = 'white', width = 1),
                    colorscale=cscale, cauto=F, cmin=0, cmax=3,
                    colorbar=list(title=paste0('-log10(',rv$xy_sig,'(Name))')))
            }
            
            incProgress(0.2)
            lm_fun <- paste0("`", xstat, "` ~ `", ystat, "`")
            rv$fit_xy <- lm(lm_fun, data = df_p)
            
            print(head(df_p))
            
            
            
            fig <- plot_ly(
                data = df_p, 
                x = df_p[[xstat]],
                y = df_p[[ystat]],
                type = 'scatter',
                mode = 'markers', 
                marker = marker_settings,
                hoverinfo="text",
                text=c(paste(df_p$Name, 
                             "<br>",rv$tt[[rv$xyx_i]],"(x):", round(df_p[[xstat]], 3),
                             "<br>p(x):", round(df_p[[xp]], 3),
                             ", q(x):", round(df_p[[xq]], 3),
                             "<br>",rv$tt[[rv$xyy_i]],"(y):", round(df_p[[ystat]], 3),
                             "<br>p(y):", round(df_p[[yp]], 3),
                             ", q(y):", round(df_p[[yq]], 3)
                ))
            )
            fig <- fig %>% layout(title = paste0(rv$selected_x, " vs ", rv$selected_y, " (n=",nrow(df_p),")"),
                                  yaxis = list(zeroline = T, title=paste0(rv$tt[[rv$xyx_i]],"_",rv$selected_y)),
                                  xaxis = list(zeroline = T, title=paste0(rv$tt[[rv$xyy_i]],"_",rv$selected_x))
            )
            
        # })
        return(fig)
    })
    
    
    output$df_xy_scatter <- renderPlotly({
        req(is.null(rv$df_xy)==F)
        req(is.null(rv$xy_colormode)==F)
        req(nrow(rv$df_xy) > 0)
        
        xy_sc_plt()
    })
    
    
    
    # download plotly html graph
    output$scatter_xy_dl <- downloadHandler(
        filename = function() {paste("scatter-xy-", Sys.Date(), ".html", sep = "")},
        content = function(file) {saveWidget(as_widget(xy_sc_plt()), file, selfcontained = TRUE)})
    
    
    ####--------------- correlation -------------------####
    
    
    # correlation line
    output$xy_corline <- renderUI({
        req(is.null(rv$fit_xy)==F)
        req(is.null(xy_sc_plt())==F)
        
        intercept = format(round(coef(rv$fit_xy)[[1]], 2), nsmall = 2)
        slope = format(round(coef(rv$fit_xy)[[2]], 2), nsmall = 2)
        r2= format(round(summary(rv$fit_xy)$r.squared, 2), nsmall = 2)
        box(
            title = NULL, background = "aqua", solidHeader = TRUE, width=12,
            strong("Correlation line:"),br(),
            column( 12,align="center" ,
                    paste0("y = ", slope,"x + ",intercept), br(),
                    paste0("(R^2 = ", r2,")")
                    )
            
        )
    })
    output$xy_cor_summary <- renderPrint({
        req(is.null(rv$df_xy)==F)
        summary(rv$fit_xy)
    })
    
    
    
    
    
    
    
    ####----------------------Main Tabs---------------------------####
    
    output$xy_header <- renderUI({
        req(is.null(rv$df_xy)==T)
        fluidRow(
            column(12, align="right",
                   div(style="display:inline-block",
                       
                       actionBttn(
                           inputId = "help_xy_pre", label=NULL, 
                           icon = icon("question"), style="material-circle", color="primary"
                       ),
                   )
            )
        )
        
    })
    
    
    output$xy_panels <- renderUI({

        if(is.null(rv$df_xy)==T){
            box(
                title = span( icon("exclamation"), "Notification"), status = "warning", width=8,
                "No data selected."
            )
        }
        else{
        div(
            fluidRow(
                column(12, align="right",
                       div(style="display:inline-block",
                           
                           actionBttn(
                               inputId = "help_xy_post", label=NULL, 
                               icon = icon("question"), style="material-circle", color="primary"
                           ),
                       )
                )
            ),
            box(
                title = span( icon("chart-area"), "Scatter"), status = "primary", solidHeader = TRUE, width=8,
                
                plotlyOutput("df_xy_scatter",
                             width = "100%",height = "600px")
                ,
                div(style = "position: absolute; left: 1em; bottom: 1em",
                    dropdown(
                        numericInput("xy_sc_p", 
                                     "P filter:", value = 0.05, min = 0, max = 1, step=0.001, width="100px"),
                        numericInput("xy_sc_q", 
                                     "FDR filter:", value = 1, min = 0, max = 1, step=0.001, width="100px"),
                        numericInput("xy_sc_Stat", 
                                     "|Stat| filter:", value = 0, min = 0, max = 10, step=0.01, width="100px"),

                        radioGroupButtons("xy_sc_logic",
                                          label = "Cutoff mode:",
                                          choices=c("Either", "Both"),
                                          selected="Both",size="s")
                        ,
                        size = "xs",
                        icon = icon("cut", class = "opt"),
                        up = TRUE, width=200
                    )
                ),
                div(style = "position: absolute; left: 4em; bottom: 1em",
                    dropdown(
                        uiOutput("xy_colormode"),
                        uiOutput("xy_sig"),
                        uiOutput("xy_thresh"),
                        numericInput(
                            inputId = "xy_sc_size",
                            label = "Dot size:",
                            value = 3, step=0.5, width="100px")
                        ,
                        size = "xs",
                        icon = icon("gear", class = "opt"),
                        up = TRUE, width=230
                    )
                ),
                div(style = "position: absolute; left: 7em; bottom: 1em",
                    dropdown(
                        downloadButton("scatter_xy_dl", "Download plot")
                        ,
                        size = "xs",
                        icon = icon("download", class = "opt"),
                        up = TRUE
                    )
                )
                
                
            ),
            box(
                title = span( icon("calculator"), "Correlation"), status = "primary", solidHeader = TRUE, width=4
                ,

                uiOutput("xy_corline"),br(),
                verbatimTextOutput("xy_cor_summary")

            ),
            box(
                title = span( icon("table"), "Table"), status = "primary", solidHeader = TRUE, width=12,
                
                dataTableOutput("xy_tbl", width = "100%",height="100%") 
                ,
                div(style = "position: absolute; left: 1em; bottom: 1em",
                    dropdown(
                        sliderTextInput("xy_p",
                                        label = "Select P cutoff:",
                                        choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                        selected=0.25, grid=T, force_edges=T),
                        sliderTextInput("xy_q",
                                        label = "Select FDR cutoff:",
                                        choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                        selected=1, grid=T, force_edges=T),
                        sliderInput("xy_Stat",
                                    label = "Select |Stat| cutoff:",
                                    min=0, max=5, step=0.1, value=0),
                        "Note: This will be applied to all columns."
                        ,
                        size = "xs",
                        icon = icon("cut", class = "opt"),
                        up = TRUE, width=300
                    )
                ),
                div(style = "position: absolute; left: 4em; bottom: 1em",
                    dropdown(
                        downloadButton("xy_tbl_dl",
                                       label = "Download Table")
                        ,
                        size = "xs",
                        icon = icon("download", class = "opt"),
                        up = TRUE
                    )
                )
                
                
            )
        )
        }
    })
    
    
    
    #======================================================================#
    ####                        MULTIPLE WAY                            ####
    #======================================================================#
    
    
    
    ####---------------------- Events ---------------------------####
    
    # update these into rv when selections change
    observeEvent(input$n_cutoff_by,{rv$n_cutoff_by<-input$n_cutoff_by})
    observeEvent(input$p_cutoff_2,{rv$p_cutoff_2<-input$p_cutoff_2})
    observeEvent(input$q_cutoff_2,{rv$q_cutoff_2<-input$q_cutoff_2})
    observeEvent(input$Stat_cutoff_2,{rv$Stat_cutoff_2<-input$Stat_cutoff_2})
    observeEvent(input$n_to_plot,{rv$n_to_plot<-input$n_to_plot})
    observeEvent(input$heatmap_sortby,{rv$heatmap_sortby<-input$heatmap_sortby})
    
    observe({
        req(is.null(rv$nx_n)==F)
        if(is.null(input$n_basic_cutby)==F){ rv$n_basic_cutby <- input$n_basic_cutby }
        
        if(is.null(input$n_hm_cutmode)==F){ rv$n_hm_cutmode <- input$n_hm_cutmode }
        if(is.null(input$n_hm_showna)==F){ rv$n_hm_showna <- input$n_hm_showna }
        if(is.null(input$sign_2)==F){ rv$sign_2 <- input$sign_2 }
        
        if(is.null(input$n_tbl_cutmode)==F){ rv$n_tbl_cutmode <- input$n_tbl_cutmode }
        if(is.null(input$n_tbl_cutoff_by)==F){ rv$n_tbl_cutoff_by <- input$n_tbl_cutoff_by }
        if(is.null(input$n_tbl_showna)==F){ rv$n_tbl_showna <- input$n_tbl_showna }
        if(is.null(input$n_p)==F){ rv$n_p <- input$n_p }
        if(is.null(input$n_q)==F){ rv$n_q <- input$n_q }
        if(is.null(input$n_Stat)==F){ rv$n_Stat <- input$n_Stat }
        if(is.null(input$n_sign)==F){ rv$n_sign <- input$n_sign }
        
        if(is.null(input$n_3ds_p)==F){ rv$n_3ds_p <- input$n_3ds_p }
        if(is.null(input$n_3ds_q)==F){ rv$n_3ds_q <- input$n_3ds_q }
        if(is.null(input$n_3ds_Stat)==F){ rv$n_3ds_Stat <- input$n_3ds_Stat }
        if(is.null(input$n_3ds_pcut)==F){ rv$n_3ds_pcut <- input$n_3ds_pcut }
        if(is.null(input$n_3ds_qcut)==F){ rv$n_3ds_qcut <- input$n_3ds_qcut }
        
        if(is.null(input$n_ins_plot)==F){ rv$n_ins_plot <- input$n_ins_plot }
        if(is.null(input$n_ins_thresh)==F){ rv$n_ins_thresh <- input$n_ins_thresh }
        if(is.null(input$n_ins_sign)==F){ rv$n_ins_sign <- input$n_ins_sign }
        if(is.null(input$n_ins_view)==F){ rv$n_ins_view <- input$n_ins_view }
        if(is.null(input$n_ins_stat_thresh)==F){ rv$n_ins_stat_thresh <- input$n_ins_stat_thresh }
        
        if(is.null(input$n_venn_label)==F){ rv$n_venn_label <- input$n_venn_label }

        if(is.null(input$n_upset_sortby)==F){ rv$n_upset_sortby <- input$n_upset_sortby }
        if(is.null(input$n_upset_showempty)==F){ rv$n_upset_showempty <- input$n_upset_showempty }
        
        if(is.null(input$n_ui_showpanel)==F){ rv$n_ui_showpanel <- input$n_ui_showpanel }
        
        for (i in 1:length(rv$nx_n)){
            if(is.null(input[[paste0("nic_p_",i)]])==F){ rv[[paste0("nic_p_",i)]] <- input[[paste0("nic_p_",i)]] }
            if(is.null(input[[paste0("nic_q_",i)]])==F){ rv[[paste0("nic_q_",i)]] <- input[[paste0("nic_q_",i)]] }
            if(is.null(input[[paste0("nic_Stat_",i)]])==F){ rv[[paste0("nic_Stat_",i)]] <- input[[paste0("nic_Stat_",i)]] }
            if(is.null(input[[paste0("nic_sign_",i)]])==F){ rv[[paste0("nic_sign_",i)]] <- input[[paste0("nic_sign_",i)]] }
            if(is.null(input[[paste0("nic_apply_",i)]])==F){ rv[[paste0("nic_apply_",i)]] <- input[[paste0("nic_apply_",i)]] }
            if(is.null(input[[paste0("nic_na_",i)]])==F){ rv[[paste0("nic_na_",i)]] <- input[[paste0("nic_na_",i)]] }
        }

        # if(is.null(input$n_igl)==F){ rv$n_igl <- input$n_igl }
    })
    
    # observe and match selected df index into new list
    observeEvent(input$heatmap_dfs,{
        rv$heatmap_i <- input$heatmap_dfs
        rv$heatmap_i <- lapply(rv$heatmap_i,function(x){match(x,rv$ll)})
    })
    
    # observe and pull out shared cols and rows among input dfs (as lists)
    observe({
        if (length(rv$heatmap_i)>=2){
            try({
                observed <- detect_shared_dimensions(rv$heatmap_i, rv$gg, rv$ll, input_mode="indices")
                rv$n_sharedcols <- observed$shared_cols
                rv$n_sharedrows <- observed$shared_rows
            })
            
        }
        else{
            rv$n_sharedcols <- NULL
            rv$n_sharedrows <- NULL
        }
    })
    

    
    # observe selection and combine to data table
    observeEvent(input$n_use_data,{
        shinyjs::disable("n_use_data")
        rv$df_n <- NULL # reset every time??
        
        withProgress(message = 'Updating data...', value = 0, {
            
            df_n <- build_df_n(input$heatmap_dfs, rv$gg, rv$ll, input_mode = "names")
            
            incProgress(0.5)
            
            # initialize params
            rv$nx_i <- isolate(rv$heatmap_i)
            rv$nx_n <- isolate(input$heatmap_dfs)
            
            rv$n_ui_showpanel <- "Main"
            
            rv$n_igl <- ""
            
            rv$n_basic_cutby <- rv$nx_n # default cut by all
            
            rv$n_tbl_cutmode <- "All"
            rv$n_tbl_cutoff_by <- isolate(input$heatmap_dfs[[1]])
            rv$n_tbl_showna <- T
            rv$n_p <- 1
            rv$n_q <- 1
            rv$n_Stat <- 0
            rv$n_sign <- "All"
            
            rv$iso_graph_dfs<-isolate(input$heatmap_dfs)
            rv$iso_sharedcols<- isolate(rv$n_sharedcols)
            
            rv$n_hm_cutmode <- "Single"
            rv$n_hm_showna <- T
            rv$n_cutoff_by <- isolate(input$heatmap_dfs[[1]])
            rv$p_cutoff_2 <- 0.05
            rv$q_cutoff_2 <- 1
            rv$Stat_cutoff_2 <- 0
            rv$sign_2 <- "All"
            rv$n_to_plot <- "Stat"
            rv$heatmap_sortby <- isolate(input$heatmap_dfs[[1]])
            
            rv$n_3ds_p <- 0.05
            rv$n_3ds_q <- 1
            rv$n_3ds_Stat <- 0
            rv$n_3ds_pcut <- 0.25
            rv$n_3ds_qcut <- 1
            
            rv$n_ins_plot <- "PValue"
            rv$n_ins_thresh <- 0.05
            rv$n_ins_sign <- "All"
            rv$n_ins_view <- "Full"
            
            rv$n_venn_label <- "counts"
            
            rv$n_upset_sortby <- "freq"
            rv$n_upset_showempty <- FALSE
            
            # initialize filters
            for (i in 1:length(rv$nx_n)){
                rv[[paste0("nic_p_",i)]] <- 0.05
                rv[[paste0("nic_q_",i)]] <- 1
                rv[[paste0("nic_Stat_",i)]] <- 0
                rv[[paste0("nic_sign_",i)]] <- "All"
                rv[[paste0("nic_apply_",i)]] <- T
                rv[[paste0("nic_na_",i)]] <- T
            }
            
            
            
            if (length(rv$nx_i) <= 5){rv$n_venn_status <- "ok"}
            else{ rv$n_venn_status <- "no" }
            if (length(rv$nx_i) == 3){rv$n_3ds_status <- "ok"}
            else{ rv$n_3ds_status <- "no" }
            
            rv$s <- vector(mode="list", length=length(rv$nx_i))
            rv$nic <- vector(mode="list", length=length(rv$nx_i))
            rv$v <- vector(mode="list", length=length(rv$nx_i))
            
            incProgress(0.2)
            print(tt)
            
        
        })
        rv$df_n <- df_n
        
        # find max stat and generate scale
        statmax <- max(dplyr::select(df_n, contains("Stat_")), na.rm=TRUE)
        rv$n_stat_scale <- round(generate_scale(statmax, 10),2)
        
        shinyjs::enable("n_use_data")
    })
    
    
    
    # update output variables (must be here!!)
    output$n_venn_status <- reactive(rv$n_venn_status)
    outputOptions(output, "n_venn_status", suspendWhenHidden = F)
    output$n_3ds_status <- reactive(rv$n_3ds_status)
    outputOptions(output, "n_3ds_status", suspendWhenHidden = F)
    
    
    ####---------------------- Sidebar ---------------------------####
    
    # select data
    output$select_df_p2 <- renderUI({
        req(length(rv$ll) >= 1)
        checkboxGroupInput(
            inputId = "heatmap_dfs",
            label= shiny::HTML("Select from uploaded datasets: 
                               <span style='color: gray'>(2 or more required)</span>"),
            choices = rv$ll)
    })
    
    # feedback on whether the data has enough shared rows/cols
    output$n_shared <- renderUI({
        # req(is.null(rv$n_sharedcols)==F)
        # req(is.null(rv$n_sharedrows)==F)
        
        if (length(rv$n_sharedcols)>=1){msgx=" (ok)"}
        else{ msgx=""}
        if (length(rv$n_sharedrows)>=1){msgy=" (ok)"}
        else{ msgy=""}
        
        
        if(length(input$heatmap_dfs) < 2){
            box(
                title = NULL, background = "black", solidHeader = TRUE, width=12,
                "Not enough datasets selected."
            )
        }
        else if (msgx==" (ok)" & msgy==" (ok)"){
            box(
                title = NULL, background = "green", solidHeader = TRUE, width=12,
                paste0("Shared columns: ",length(rv$n_sharedcols), msgx),br(),
                paste0("Shared rows: ",length(rv$n_sharedrows), msgy)
            )
        }
        else{
            box(
                title = NULL, background = "red", solidHeader = TRUE, width=12,
                paste0("Shared columns: ",length(rv$n_sharedcols), msgx),br(),
                paste0("Shared rows: ",length(rv$n_sharedrows), msgy)
            )
        }
    })
    
    output$n_shared_cols <- renderText({
        req(length(rv$ll) >= 1)
        if(length(input$heatmap_dfs) >= 2){
            if (length(rv$n_sharedcols)>=1){msg=" (ok)"}
            else{msg=" (x)"}
            paste0("Shared columns: ",length(rv$n_sharedcols), msg)
        }
        
        else{ "Shared columns: "}
        
        
    })
    output$n_shared_rows <- renderText({
        req(length(rv$ll) >= 1)
        if(length(input$heatmap_dfs) >= 2){
            if (length(rv$n_sharedrows)>=1){msg=" (ok)"}
            else{msg=" (x)"}
            paste0("Shared rows: ",length(rv$n_sharedrows), msg)
        }
        
        else{ "Shared rows: "}
    })
    
    output$n_use_data <- renderUI({
        req(length(rv$ll) >= 2)
        req(length(rv$heatmap_i) > 1)
        req(length(rv$n_sharedrows)>=1)
        req(length(rv$n_sharedcols)>=1)
        actionButton("n_use_data", "Visualize!")
    })
    
    
    
    ####----------------------Main Tabs---------------------------####
    
    
    output$n_header <- renderUI({
        req(is.null(rv$df_n)==T)
        fluidRow(
            column(12, align="right",
                   div(style="display:inline-block",
                       
                       actionBttn(
                           inputId = "help_n_pre", label=NULL, 
                           icon = icon("question"), style="material-circle", color="primary"
                       ),
                   )
            )
        )
        
    })
    
    
    
    output$n_panels <- renderUI({
        if(is.null(rv$df_n)==T){
            box(
                title = span( icon("exclamation"), "Notification"), status = "warning", width=8,
                "No data selected."
            )
        }
        else{
        div(
            fluidRow(
                column(6,
                       radioGroupButtons("n_ui_showpanel",
                                         choices=c("Main", "Intersection", "Correlation"),
                                         selected="Main", status="primary",
                                         checkIcon = list(
                                             yes = tags$i(class = "fa fa-check-square", 
                                                          style = "color: white"),
                                             no = tags$i(class = "fa fa-square-o", 
                                                         style = "color: white"))
                       ),
                ),
                column(6, align = "right",
                       div(style="display:inline-block",
                           dropdown(
                               
                               tags$h3("Customize Filters"),
                               
                               uiOutput("ui_n_gls_opt"),
                               
                               style = "material-circle", icon = icon("gear"),
                               status = "primary", width = "900px",
                               right=T,
                               animate = animateOptions(
                                   enter = "slideInRight",
                                   exit = "fadeOutRight", duration = 0.5
                               ),
                           ),
                       ),
                       div(style="display:inline-block",
                        
                           actionBttn(
                               inputId = "help_n_post", label=NULL, 
                               icon = icon("question"), style="material-circle", color="primary"
                           ),
                           )
                       
                       )
            ),
            
            
            uiOutput("n_ui_basic"),
            uiOutput("n_ui_intersect"),
            uiOutput("n_ui_correlation")

            
        )   
        }
        
    })
    
    #----------------- basic analysis --------------------
    
    output$n_ui_basic <- renderUI({
        req(rv$n_ui_showpanel == "Main")
        div(
            
            
            #----------------- heatmap --------------------
            box(
                title = span( icon("chart-area"), "Heatmap"), status = "primary", solidHeader = TRUE, width=6,
                
                uiOutput("n_heatmap"), 

                div(style = "position: absolute; left: 1em; bottom: 1em",
                    dropdown(
                        # materialSwitch(
                        #     inputId = "n_hm_showna", label = "Show rows with NA?", status="primary",
                        #     value = T
                        # ),
                        uiOutput("select_sortby_p2"),
                        uiOutput("n_to_plot"),
                        
                        size = "xs",
                        icon = icon("gear", class = "opt"),
                        up = TRUE, width=300
                    )
                ),
                div(style = "position: absolute; left: 4em; bottom: 1em",
                    dropdown(
                        downloadButton("n_hm_dl", "Download plot")
                        ,
                        size = "xs",
                        icon = icon("download", class = "opt"),
                        up = TRUE
                    )
                )
                
                
            ),
            
            
            #----------------- 3ds --------------------
            
            conditionalPanel("output.n_3ds_status == 'ok'",
                             box(
                                 title = span( icon("chart-area"), "3D Scatter"), status = "primary", solidHeader = TRUE, width=6,
                                 plotlyOutput("df_n_3ds",
                                              width = "100%",height = "600px"),
                                 

                                 div(style = "position: absolute; left: 1em; bottom: 1em",
                                     dropdown(
                                         "Color threshold options:",
                                         sliderTextInput("n_3ds_p",
                                                         label = "Select P threshold:",
                                                         choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                                         selected=0.05, grid=T, force_edges=T),
                                         sliderTextInput("n_3ds_q",
                                                         label = "Select FDR threshold:",
                                                         choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                                         selected=1, grid=T, force_edges=T),
                                         sliderTextInput("n_3ds_Stat",
                                                         label = "Select |Stat| threshold:",
                                                         choices= c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5),
                                                         selected=0, grid=T, force_edges=T),
                                         "Note: This will be applied to all columns.",
                                         
                                         
                                         size = "xs",
                                         icon = icon("gear", class = "opt"),
                                         up = TRUE, width=300
                                     )
                                 ),
                                 div(style = "position: absolute; left: 4em; bottom: 1em",
                                     dropdown(
                                         strong("Summary:"),
                                         dataTableOutput("n_3ds_prop_tbl"),
                                         
                                         
                                         size = "xs",
                                         icon = icon("table", class = "opt"),
                                         up = TRUE, width=300
                                     )
                                 ),
                                 div(style = "position: absolute; left: 7em; bottom: 1em",
                                     dropdown(
                                         downloadButton("n_3ds_dl", "Download plot"),
                                         downloadButton("download_3ds_df", "Download summary")
                                         ,
                                         size = "xs",
                                         icon = icon("download", class = "opt"),
                                         up = TRUE
                                     )
                                 )
                                 
                                 
                                 
                             )
            ),
            conditionalPanel("output.n_3ds_status == 'no'",
                             uiOutput("n_3ds_placeholder")
            ),
            
            
            #----------------- table --------------------
            box(
                title = span( icon("table"), "Table"), status = "primary", solidHeader = TRUE, width=12, height="610px",
                
                dataTableOutput("df_n_tbl"),
                
                div(style = "position: absolute; left: 1em; bottom: 1em",
                    dropdown(
                        downloadButton("download_n_df", "Download data")
                        ,
                        size = "xs",
                        icon = icon("download", class = "opt"),
                        up = TRUE
                    )
                )
                
            )
        )
    })
    
    #----------------- intersection analysis --------------------
    
    output$n_ui_intersect <- renderUI({
        req(rv$n_ui_showpanel == "Intersection")
        
        
        div(

            
            #----------------- venn --------------------
            
            conditionalPanel("output.n_venn_status == 'ok'",
                             box(title = span( icon("chart-area"), "Venn Diagram"), status = "primary", solidHeader = TRUE, width=6,
                                 tabBox(width=12,
                                        tabPanel("Basic",
                                                 plotOutput("df_n_npvenn", width="100%")
                                        ),
                                        tabPanel("Area proportional",
                                                 plotOutput("df_n_venn", width="100%"),
                                        )
                                        
                                 ),
                                 
                                 
                                 
                                 div(style = "position: absolute; left: 1em; bottom: 1em",
                                     dropdown(
                                         checkboxGroupInput(
                                             inputId = "n_venn_label",
                                             label= "Show in label:",
                                             choices = c("Counts"="counts", "Percent"="percent"),
                                             selected="counts",
                                             inline=T, width="250px"
                                         )
                                         ,
                                         size = "xs",
                                         icon = icon("gear", class = "opt"),
                                         up = TRUE
                                     )
                                 ),
                                 div(style = "position: absolute; left: 4em; bottom: 1em",
                                     dropdown(
                                         downloadButton("n_npvenn_dl", "Download basic"),
                                         downloadButton("n_venn_dl", "Download area-proportional"),
                                         
                                         size = "xs",
                                         icon = icon("download", class = "opt"),
                                         up = TRUE, width=300
                                     )
                                     
                                 )
                                 
                                 
                             ),
                             
            ),
            
            conditionalPanel("output.n_venn_status == 'no'",
                             uiOutput("n_venn_placeholder")
            ),
            
            
            
            #----------------- upset --------------------
            
            box(
                title = span( icon("chart-area"), "UpSet Plot"), status = "primary", solidHeader = TRUE, width=6,
                
                plotOutput("df_n_upset", width = "100%"),
                
                div(style = "position: absolute; left: 1em; bottom: 1em",
                    dropdown(
                        selectInput(
                            inputId = "n_upset_sortby",
                            label = "Order by:",
                            choices = c("Frequency"="freq", "Degree"="degree"),
                            selected = "freq"),
                        materialSwitch(
                            inputId = "n_upset_showempty", label = "Show empty intersections?", status="primary",
                            value = FALSE
                        ),
                        
                        size = "xs",
                        icon = icon("gear", class = "opt"),
                        up = TRUE, width=300
                    )
                    
                ),
                div(style = "position: absolute; left: 4em; bottom: 1em",
                    dropdown(
                        downloadButton("n_upset_dl", "Download plot"),
                        
                        size = "xs",
                        icon = icon("download", class = "opt"),
                        up = TRUE, width=300
                    )
                    
                )
                
            ),
            
            #----------------- Intersect table--------------------
            
            box(
                width = 12, status = "primary",solidHeader = T, height=750,
                title = span(icon("table"),"View intersections"),
                
                wellPanel(
                    "Combine options below to view specific intersections: ",br(),br(),
                    uiOutput("ui_intersections")
                ),
                
                
                dataTableOutput("n_ins_tbl"),
                
                
                div(style = "position: absolute; left: 1em; bottom: 1em",
                    dropdown(
                        radioGroupButtons(
                            inputId = "n_ins_view",
                            label = "Choose view:",
                            choices = c("Full", "Minimized", "T/F Matrix"),
                            selected= "Full", direction="vertical"
                        ),
                        
                        size = "xs",
                        icon = icon("gear", class = "opt"),
                        up = TRUE, width=300
                    )
                    
                ),
                
                div(style = "position: absolute; left: 4em; bottom: 1em",
                    dropdown(
                        column(8,
                               column(2, textInput("n_ins_wc_sep", "Separator:", value="_")),
                               column(10, textInput("n_ins_Wc_ignore", "Ignore strings: (separated by spaces)", value="and or of GO KEGG WP")),
                               
                               plotOutput("n_ins_wc"),
                        ),
                        column(4,
                               dataTableOutput("n_ins_wc_df"),
                        ),
                        
                        size = "xs",
                        icon = icon("cloud", class = "opt", lib="glyphicon"),
                        up = TRUE, width=800
                    )
                    
                ),
                div(style = "position: absolute; left: 7em; bottom: 1em",
                    dropdown(
                        downloadButton("download_ins_df", "Download current table"),
                        downloadButton("download_ins_gl", "Download gene list"),
                        
                        size = "xs",
                        icon = icon("download", class = "opt"),
                        up = TRUE, width=300
                    )
                    
                )
                
                
            )
            
        )
    })
    
    
    output$n_venn_placeholder <- renderUI({
        box(
            title = span( icon("chart-area"), "Venn Diagram"), status = "warning", solidHeader = TRUE, width=6,
            paste0("Venn diagram is only available for 4 or less datasets. You have selected ", length(rv$nx_i)," datasets.")
        )
    })
    output$n_3ds_placeholder <- renderUI({
        box(
            title = span( icon("chart-area"), "3D Scatter"), status = "warning", solidHeader = TRUE, width=6,
            paste0("3D Scatter is only available for 3 datasets. You have selected ", length(rv$nx_i)," datasets.")
        )
    })
    
    
    
    output$n_ui_correlation <- renderUI({
        req(rv$n_ui_showpanel == "Correlation")
        div(
            
            
           box(title = span( icon("chart-area"), "Correlation Heatmap"), status = "primary", solidHeader = TRUE, width=12,
               "Correlation heatmap here"
               
               )
                   
            
            
            
        )
    })
    
    

    
    ####================= MULTIPLE VISUALIZATIONS =====================####
    
    
    ####-------------------- Correlation ------------------------####
    
    
    
    
    
    
    
    ####-------------------- Intersect options ------------------------####
    # generates dynamic ui for selection
    
    output$ui_n_gls_opt <- renderUI({
        req(nrow(rv$df_n)>0)
        
        append(rv$nic, rv$global_nic, 0) # add the global box to first item when output
    })
    
    observeEvent(input$nic_applytoall, {
        for (i in 1:length(rv$nx_n)){
            updateNumericInput(session, paste0("nic_p_",i), value=input$nic_p)
            updateNumericInput(session, paste0("nic_q_",i), value=input$nic_q)
            updateNumericInput(session, paste0("nic_Stat_",i), value=input$nic_Stat)
            updateRadioGroupButtons(session, paste0("nic_sign_",i), selected=input$nic_sign)
            updateCheckboxInput(session, paste0("nic_apply_",i), value=input$nic_apply)
            updateCheckboxInput(session, paste0("nic_na_",i), value=input$nic_na)
        }
    })
    observeEvent(input$nic_resetall, {
        for (i in 1:length(rv$nx_n)){
            updateNumericInput(session, paste0("nic_p_",i), value=0.05)
            updateNumericInput(session, paste0("nic_q_",i), value=1)
            updateNumericInput(session, paste0("nic_Stat_",i), value=0)
            updateRadioGroupButtons(session, paste0("nic_sign_",i), selected="All")
            updateCheckboxInput(session, paste0("nic_apply_",i), value=T)
            updateCheckboxInput(session, paste0("nic_na_",i), value=T)
            updateNumericInput(session, "nic_p", value=0.05)
            updateNumericInput(session, "nic_q", value=1)
            updateNumericInput(session, "nic_Stat", value=0)
            updateRadioGroupButtons(session, "nic_sign", selected="All")
            updateCheckboxInput(session, "nic_apply", value=T)
            updateCheckboxInput(session, "nic_na", value=T)
        }
    })
    
    observe({
        req(nrow(rv$df_n)>0)
        
        rv$global_nic[[1]] <- div(style="display: inline-block;vertical-align:top; width: 280px",
                           box(title = NULL, status="primary", solidHeader = TRUE, width=12, height="320px", align = "left",
                               strong("Global settings"),
                               # tags$hr(style="border-color: grey;"),
                               fluidRow(
                                   column(6, align = "left",
                                          numericInput("nic_p", 
                                                       "P filter:", value = 0.05, min = 0, max = 1, step=0.001, width="100px")),
                                   column(6, align = "left",
                                          numericInput("nic_Stat", 
                                                       "|Stat| filter:", value = 0, min = 0, max = 5, step=0.1, width="100px")),
                               ),
                               fluidRow(
                                   column(6, align = "left",
                                          numericInput("nic_q", 
                                                       "FDR filter:", value = 1, min = 0, max = 1, step=0.001, width="100px")),
                                   column(6, align = "left",
                                          radioGroupButtons("nic_sign", 
                                                            label = "Filter by sign:",
                                                            choices=c("All"="All", "+"="Positive", "-"="Negative"),
                                                            selected="All",size="s",direction = "horizontal"),
                                   ),
                                   
                               ),
                               fluidRow(
                                   column(4,offset=1, align = "left",
                                          checkboxInput(
                                              inputId= "nic_apply",
                                              label = "Apply",
                                              value = T)),
                                   column(7, align = "left",
                                          checkboxInput(
                                              inputId= "nic_na",
                                              label = "Show NAs",
                                              value = T, ))
                                   
                               ),
                               actionButton("nic_applytoall", "Apply to all"),
                               actionButton("nic_resetall", "Reset all"),
                               
                        style = "padding: 15px;")
                    
        )
        for (i in 1:length(rv$nx_n)){
            rv$nic[[i]] <- div(style="display: inline-block;vertical-align:top; width: 280px;",
                             wellPanel( align = "left",
                                 rv$nx_n[[i]], 
                                 tags$hr(style="border-color: grey;"),
                                 fluidRow(
                                     column(6, align = "left",
                                        numericInput(inputId = paste0("nic_p_",i), 
                                                     "P filter:", value = 0.05, min = 0, max = 1, step=0.001, width="100px")),
                                 column(6, align = "left",
                                        numericInput(paste0("nic_Stat_",i), 
                                                     "|Stat| filter:", value = 0, min = 0, max = 5, step=0.1, width="100px")),
                                 ),
                                 fluidRow(
                                     column(6, align = "left",
                                        numericInput(inputId = paste0("nic_q_",i), 
                                                     "FDR filter:", value = 1, min = 0, max = 1, step=0.001, width="100px")),
                                 column(6, align = "left",
                                        radioGroupButtons(inputId = paste0("nic_sign_",i), 
                                                          label = "Filter by sign:",
                                                          choices=c("All"="All", "+"="Positive", "-"="Negative"),
                                                          selected="All",size="s",direction = "horizontal"),
                                        ),
                                 
                                 ),
                                 fluidRow(
                                     column(4,offset=1, align = "left",
                                            checkboxInput(
                                                inputId= paste0("nic_apply_",i),
                                                label = "Apply",
                                                value = T)),
                                     column(7, align = "left",
                                            checkboxInput(
                                                inputId= paste0("nic_na_",i),
                                                label = "Show NAs",
                                                value = T, ))
                                     
                                 )
                             ,style = "padding: 15px;")
                             )
        }
    })
    

    
    
    
    
    ####-------------------- view intersections ------------------------####

    # generates dynamic ui for selection
    observe({
        req(nrow(rv$df_n)>0)
        
        for (i in 1:length(rv$nx_n)){
            rv$s[[i]] <- div(style="display: inline-block;vertical-align:top; width: 280px;",
                radioGroupButtons(inputId = paste0("ins_",i),
                                               label = rv$nx_n[[i]], size="s",
                                               choices = c("True", "False", "Ignore"),
                                               selected ="Ignore",
                                               status = "info"))
        }
    })
    
    output$ui_intersections <- renderUI({
        req(nrow(rv$df_n)>0)
        rv$s
    })
    
    # updates criteria
    observe({
        req(rv$df_n)
        req(length(rv$s)==length(rv$nx_i))
        
        # turn those T/F inputs into a true/ false vector
        criteria <- vector(mode = "list", length = length(rv$v))
        for (i in 1:length(rv$s)){
            criteria[[i]] <- input[[paste0("ins_", i)]]
        }
        criteria <- unlist(criteria)
        criteria <- gsub("False",FALSE, criteria)
        criteria <- gsub("True",TRUE, criteria)
        criteria <- gsub("Ignore",NA, criteria)
        criteria <- as.logical(criteria)
        
        # name the criteria vector
        if (length(criteria) == length(rv$nx_n)){
            names(criteria) <- rv$nx_n
        }
        print(criteria)
        rv$ins_criteria <- criteria
    })
    
    ####### -------------- Processing for all intersection related analysis. ---------------
    # 1. generate gene lists (gls) according to individual cutoffs
    n_ins_gls <- reactive({
        req(length(rv$nx_n)<=5)
        req(nrow(rv$df_n)>0)
        req(length(rv$s)>0)
        req(length(rv$s)==length(rv$nx_i))
        
        df <- rv$df_n
        
        gls <- vector(mode="list") # initialize gls as empty list
        
        
        # using advanced settings
        for (i in 1:length(rv$nx_n)){
            req(rv[[paste0("nic_p_",i)]])
            req(rv[[paste0("nic_q_",i)]])
            req(rv[[paste0("nic_Stat_",i)]])
            req(rv[[paste0("nic_sign_",i)]])
            
            n <- rv$nx_n[[i]]
            ss <- df
            ss <- ss[ss[[paste0("PValue","_", n)]]<=rv[[paste0("nic_p_",i)]], ] # filter by p
            ss <- ss[ss[[paste0("FDR","_", n)]]<=rv[[paste0("nic_q_",i)]], ] # filter by q
            ss <- ss[abs(ss[[paste0("Stat","_", n)]])>=rv[[paste0("nic_Stat_",i)]], ] # filter by stat
            ss <- filter_by_sign(ss, paste0("Stat","_", n), rv[[paste0("nic_sign_",i)]], tolerate=T) # filter by stat sign

            gl <- as.character(na.omit(ss$Name)) # format
            gls[[n]] <- gl # write into list as named vector
        }
        
        return(gls)
    })
    
    
    # 2. turns gls into matrix
    n_ins_glm <- reactive({
        req(nrow(rv$df_n)>0)
        req(length(n_ins_gls())>0)

        df <- rv$df_n
        
        gls <- n_ins_gls()
        xx <- lapply(seq_along(names(gls)),function(x){
            unique(unlist(gls)) %in% gls[[names(gls)[[x]]]]
        })
        names(xx) <- names(gls)
        
        # turn into df
        glm <- data.frame(xx, row.names = unique(unlist(gls)))
        
        glm
    })
    
    # 3. filters gls matrix by criteria
    n_ins_ss <- reactive({ 
        req(nrow(rv$df_n)>0)
        req(nrow(n_ins_glm())>0)
        
        df <- rv$df_n
        
        # # apply stat filters to selected data only # is this appropriate here????
        # for (i in 1:length(rv$ins_criteria)){
        #     if (is.na(rv$ins_criteria[[i]])==F){
        #         df <- filter_by_sign(rv$df_n, paste0("Stat_",names(rv$ins_criteria)[[i]]),
        #                              rv$n_ins_sign, tolerate=T)
        #     }
        # }
        # 
        # # get t/f table
        # subset <- df %>% dplyr::select(contains(rv$n_ins_plot)) < rv$n_ins_thresh  # turn df into t/f
        # subset[is.na(subset)] <- FALSE # support for na values
        # rownames(subset) <- df$Name
        
        glm <- n_ins_glm()
        
        # get subset of genes based on t/f table
        subset <- glm[apply(glm,1,function(x) {
            match_skipna(x,rv$ins_criteria)
        }),] 
        subset

    })
    
    # 4. turns ss into filtered gl
    n_ins_fgl <- reactive({
        subset <- n_ins_ss()
        genelist <- rownames(subset) # these are gene list
        genelist
    })
    
    # 5. subsets full df based on ss and filtered gl
    n_ins_df <- reactive({
        req(length(rv$ins_criteria)>0)
        req(length(rv$ins_criteria)==length(rv$nx_i))

        df <- rv$df_n # full df to subset
        genelist <- n_ins_fgl() # list of genes to show in table
        
        if(rv$n_ins_view == "Full"){
            df <- df[df$Name %in% genelist,] # extract the rows from full df
        }
        else if (rv$n_ins_view == "Minimized"){
            xx <- dplyr::select(df, contains(c("Name","Stat", rv$n_ins_plot)))
            print(head(xx))
            df <- xx[xx$Name %in% genelist,]
        }
        else if (rv$n_ins_view == "T/F Matrix"){
            df <- as.data.frame(n_ins_ss())
            
            # turn row names into identifying column
            df <- cbind(rownames(df),df)
            colnames(df)[[1]] <- "Name"
        }
        
        # tidy row names
        if (nrow(df)>0){rownames(df) <- seq(1,nrow(df),1)}
        
        # to replace the stat col names
        colnames(df) <- gsub("Stat", rv$tt[[rv$nx_i[[1]]]], colnames(df))
        
        df
    })
    
    ####-------------------- view intersections table ------------------------####
    
    # show intersection preview table
    output$n_ins_tbl <- DT::renderDataTable({
        req(rv$df_n)
        req(length(rv$ins_criteria)>0)
        
        df <- n_ins_df()

        rv$df_ins_fullcols <- colnames(df)

        # to abbreviate the long column names...take first 5 letters
        char_limit <- 56 / length(colnames(df))
        print(char_limit)
        colnames(df) <- sapply(names(df), function(x){
            if (nchar(x)>char_limit)
            {return (paste0(substr(x, start = 1, stop = char_limit),"..."))}
            else{return (x)}
        })

        # to round everything down to 3 decimals
        df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
        
        
        df
        
        
    }
    , plugins = "ellipsis",
    options = list(scrollX=TRUE, 
                   columnDefs = list(
                       list(
                           targets = 1,
                           render = JS("$.fn.dataTable.render.ellipsis( 17, true )")
                       ),
                       list(
                           targets = "_all",
                           render = JS("$.fn.dataTable.render.ellipsis( 6, true )")
                       )
                   ),
                   headerCallback= JS("function(thead, data, start, end, display){",
                                      sprintf("  var tooltips = [%s];", toString(paste0("'", rv$df_ins_fullcols, "'"))),
                                      "  for(var i = 1; i <= tooltips.length; i++){",
                                      "    $('th:eq('+i+')',thead).attr('title', tooltips[i-1]);",
                                      "  }",
                                      "}"))
    )
    
    
    # download intersection table as csv
    output$download_ins_df <- downloadHandler( # needs to be exactly the same as table render
        filename = function() {
            paste("intersection", "-", "multiple", "-", Sys.Date(), ".csv", sep="")},
        content = function(file) {
            write.csv(n_ins_df(), file, 
            row.names = F, quote=TRUE)})
    
    # download filtered gene list as txt
    output$download_ins_gl <- downloadHandler(
        filename = function() {
            paste("names", "-", "multiple", "-", Sys.Date(), ".txt", sep="")},
        content = function(file) {
            ins <- n_ins_fgl()
            fwrite(list(ins), file, sep=",", 
                   row.names = F, quote=F)
        }
    )
    

    
    
    ####-------------------- Upset plot ------------------------####
    
    n_upset_plt <- reactive({
        gls <- n_ins_gls()
        
        names(gls) <- gsub(".csv","",names(gls))
        #c(intersection size title, intersection size tick labels, set size title, set size tick labels, set names, numbers above bars)
        textscale <- c(1.3, 2, 1.3, 1, 1.1, 2)
        if (rv$n_upset_showempty==F){
            upset <- upset(fromList(gls), order.by = rv$n_upset_sortby,
                           text.scale = textscale)
        }
        else if (rv$n_upset_showempty==T){
            upset <- upset(fromList(gls), empty.intersections = "on", order.by = rv$n_upset_sortby,
                           text.scale = textscale)
        }
        upset
    })
    
    
    # gl ver (uses the shared reactive)
    output$df_n_upset <- renderPlot({
        req(rv$df_n)
        req(is.null(rv$n_upset_showempty)==F)
        req(is.null(rv$n_upset_sortby)==F)
        req(max(lengths(n_ins_gls()))>0)
        req(min(lengths(n_ins_gls()))>0)
        req(length(n_ins_gls())>1)
        
        
        n_upset_plt()
    })
    
    output$n_upset_dl <- downloadHandler(
        filename = function() { paste("upset-multiple-",Sys.Date(), '.pdf', sep='') },
        content = function(file) {
            pdf(file)
            print(n_upset_plt())
            dev.off()
        }
    )
    
    
    
    # # matrix ver
    # output$df_n_upset <- renderPlot({
    #     req(rv$df_n)
    #     req(is.null(rv$n_upset_showempty)==F)
    #     req(is.null(rv$n_upset_sortby)==F)
    #     
    #     df <- rv$df_n
    #     
    #     df <- filter_by_sign(rv$df_n, "Stat", rv$n_ins_sign, tolerate=T)
    #     req(nrow(df)>0) # halt if error
    #     
    #     subset <- df %>% dplyr::select(contains(rv$n_ins_plot)) < rv$n_ins_thresh  # turn df into t/f
    #     subset[is.na(subset)] <- FALSE # support for na values
    #     subset <- subset * 1 # turn t/f into 0/1
    #     subset <- data.frame(subset)
    #     
    #     # remove "pvalue_" from colnames to prevent long string
    #     colnames(subset) <- gsub(paste0(rv$n_ins_plot,"_"),"",colnames(subset))
    #     
    #     if (rv$n_upset_sortby=="Frequency"){ sortby <- "freq" }
    #     else if (rv$n_upset_sortby=="Degree"){ sortby <- "degree" }
    #     
    #     if (rv$n_upset_showempty==F){
    #         upset <- upset(subset, order.by = sortby)
    #     }
    #     else if (rv$n_upset_showempty==T){
    #         upset <- upset(subset, empty.intersections = "on", order.by = sortby)
    #     }
    # 
    #     
    #     upset
    # })
    
    
    
    
    ####-------------------- Venn ------------------------####
    
    output$n_npvenn_dl <- downloadHandler(
        filename = function() { paste("venn1-multiple-",Sys.Date(), '.png', sep='') },
        content = function(file) {
            req(is.null(n_npvenn_plt())==F)
            ggsave(file, plot = n_npvenn_plt(), device = "png")
        }
    )
    
    output$n_venn_dl <- downloadHandler(
        filename = function() { paste("venn2-multiple-",Sys.Date(), '.png', sep='') },
        content = function(file) {
            req(is.null(n_venn_plt())==F)
            ggsave(file, plot = n_venn_plt(), device = "png")
        }
    )
    
    
    #------------------- eulerR area proportional venn
    # gl ver (uses the shared reactive)
    n_venn_plt <- reactive({
        gls <- n_ins_gls()
        names(gls) <- gsub(".csv","",names(gls))
        fit2 <- euler(gls)
        venn <- plot(fit2, quantities = list(type = rv$n_venn_label))
        
        venn
    })
    
    output$df_n_venn <- renderPlot({
        req(length(n_ins_gls())>0)
        req(max(lengths(n_ins_gls()))>0)
        n_venn_plt()
    })
    
    # # plot (matrix ver)
    # output$df_n_venn <- renderPlot({
    #     req(length(rv$nx_i)<=4)
    # 
    #     df <- filter_by_sign(rv$df_n, "Stat", rv$n_ins_sign, tolerate=T)
    # 
    #     # false if: 1) it fails the criteria provided; 2) value is missing
    #     subset <- df %>% dplyr::select(contains(rv$n_ins_plot)) < rv$n_ins_thresh  # turn df into t/f
    #     subset[is.na(subset)] <- FALSE
    # 
    #     # remove "pvalue_" from colnames to prevent long string
    #     colnames(subset) <- gsub(paste0(rv$n_ins_plot,"_"),"",colnames(subset))
    # 
    #     # plot data
    #     fit2 <- euler(subset)
    #     venn <- plot(fit2, quantities = list(type = "counts"))
    # 
    #     venn
    # })
    
    
    
    
    # output$df_n_venn_fit <- renderPlot({
    #     req(rv$df_n)
    #     
    #     df <- rv$df_n
    #     subset <- df %>% dplyr::select(contains(rv$n_ins_plot)) < rv$n_ins_thresh  # turn df into t/f
    #     
    #     # remove "pvalue_" from colnames to prevent long string
    #     colnames(subset) <- gsub(paste0(rv$n_ins_plot,"_"),"",colnames(subset))
    #     
    #     # plot data
    #     fit2 <- euler(subset)
    #     fig <- error_plot(fit2)
    #     fig
    # })
    
    # #------------------- VennDiagram non area proportional venn
    n_npvenn_plt <- reactive({
        req(length(n_ins_gls())>0)
        req(max(lengths(n_ins_gls()))>0)
        req(n_ins_gls())

        grid.newpage()
        gls <- n_ins_gls()
        names(gls) <- gsub(".csv","",names(gls))
        len <- length(gls)
        palette <- c("white","grey","darkgrey","lightgrey","black")
        palette <- palette[1:len]
        venn1 <- venn.diagram(gls, filename = NULL,
                              lwd = 1, # border width
                              fontfamily = "sans",
                              cat.fontface = "bold",
                              cex = 1, # catname size
                              # cat.pos = rep(180,len), # catname position
                              cat.cex = 1, # areaname size
                              cat.fontfamily = "sans",
                              fill = palette, # area fill
                              alpha = 0.5, # area alpha
                              ext.text=T, # draw label outside in case no space
                              ext.line.lty = "dotted", # pattern of extline
                              sigdigs = 2,
                              print.mode = gsub("counts","raw", rv$n_venn_label)
        )
        venn1
    })
    
    output$df_n_npvenn <- renderPlot({
        req(length(n_ins_gls())>0)
        req(max(lengths(n_ins_gls()))>0)
        
        
        
        
        grid.draw(n_npvenn_plt())
        
        
    })
    
    ####-------------------- Processing for all basic visualizations. ------------------------####
    
    # 1. cut first by input genelist (if any)
    n_basic_igl <- reactive({
        df <- rv$df_n
        if (nchar(rv$n_igl)>0){
            igl <- isolate(as.list(strsplit(rv$n_igl, '\\n+')))
            # print(igl)
            df <- df[df$Name %in% igl[[1]],]
            df <- df[order(match(df$Name, igl[[1]])), ]
        }
        return(df)
    })
    
    # report genes that are not found
    n_basic_igl_nm <- reactive({
        if (nchar(rv$n_igl)>0){
            igl <- isolate(as.list(strsplit(rv$n_igl, '\\n+')))
            notfound <- setdiff(igl[[1]], n_basic_igl()$Name)
        }
        else{notfound=vector()}
        return(notfound)
    })
    output$n_igl_nm <- renderUI({
        req(length(n_basic_igl_nm())>0)
        nl <- paste(n_basic_igl_nm(), collapse=", ")
        box(width=12,
            shiny::HTML(paste0("<strong>Not found</strong> (",length(n_basic_igl_nm()),"): ", nl)),
        )
    })
    observeEvent(input$n_igl_update,{
        rv$n_igl <- input$n_igl
    })
    observeEvent(input$n_igl_reset,{
        updateTextAreaInput(session, "n_igl", value="")
        rv$n_igl <- ""
    })

    # 2. apply cutoffs to the master df
    n_basic_df <- reactive({
        req(nrow(rv$df_n)>0)
        req(length(rv$s)>0)
        req(length(rv$s)==length(rv$nx_i))
        
        df <- n_basic_igl()
        # tol <- rv$n_tbl_showna

        # # specify which datasets to cut by (those not selected will be ignored)
        # cut_by <- rv$n_basic_cutby
        
        
        # using advanced settings
        for (i in 1:length(rv$nx_n)){
            req(rv[[paste0("nic_p_",i)]])
            req(rv[[paste0("nic_q_",i)]])
            req(rv[[paste0("nic_Stat_",i)]])
            req(rv[[paste0("nic_sign_",i)]])
            req(is.null(rv[[paste0("nic_na_",i)]])==F)
            req(is.null(rv[[paste0("nic_apply_",i)]])==F)
            
            
            tol = rv[[paste0("nic_na_",i)]]
            apply = rv[[paste0("nic_apply_",i)]]
            
            if (apply==T){
                # get the col names
                n <- rv$nx_n[[i]]
                statn <- paste0("Stat_", n)
                pn <- paste0("PValue","_", n)
                qn <- paste0("FDR","_", n)
                
                # filter by sign
                df <- filter_by_sign(df, statn, 
                                     rv[[paste0("nic_sign_",i)]], 
                                     tolerate=tol)
                # filter by cutoffs
                df <- apply_single_cutoff(df, n, 
                                          rv[[paste0("nic_p_",i)]],
                                          rv[[paste0("nic_q_",i)]], 
                                          rv[[paste0("nic_Stat_",i)]], 
                                          tolerate=tol)
                
            }
            

        }
        
        
        # # basic cutoffs
        # withProgress(message = 'Applying cutoffs...', value = 0.5, {
        # 
        # # apply cutoffs to those datasets only
        # if (length(cut_by)>0){
        #     for (i in cut_by){
        #         statn <- paste0("Stat_", i)
        #         df <- filter_by_sign(df, statn, rv$n_sign, tolerate=tol)
        #         df <- apply_single_cutoff(df, i, rv$n_p, rv$n_q, rv$n_Stat, tolerate=tol)
        #     }
        # }
        # 
        # })
        
        return(df)

    })
    
    
    ####-------------------- 3D scatter ------------------------####

    
    # main graph
    n_3ds_plt <- reactive({

        withProgress(message = 'Making 3D Scatter...', value = 0, {
            
            df <- n_basic_df()
            df <- remove_nas(df)
            
            # attain columns max/min values for cutoff and coloring (automatically filters out NA)
            df <- df %>% mutate(mp = do.call(pmax, dplyr::select(df, contains("PValue")))) 
            df <- df %>% mutate(mq = do.call(pmax, dplyr::select(df, contains("FDR")))) 
            df <- df %>% mutate(msmin = do.call(pmin, dplyr::select(df, contains("Stat")))) %>% 
                mutate(msmax = do.call(pmax, dplyr::select(df, contains("Stat")))) 
            
            incProgress(0.2)
            
            # # apply cutoffs
            # df <- df %>% filter(mp < rv$n_p) %>% filter(mq < rv$n_q)
            
            # assign color by certain criteria
            df$color <- ifelse(df$mp < rv$n_3ds_p & df$mq < rv$n_3ds_q & df$msmin > rv$n_3ds_Stat, "red", "gray")
            df$color <- ifelse(df$mp < rv$n_3ds_p & df$mq < rv$n_3ds_q & df$msmax < -rv$n_3ds_Stat, "blue",df$color)
            df$color <- as.factor(df$color)
            #print(head(df))
            
            incProgress(0.2)
            
            # get col names
            statcols <- colnames(df[1 ,grepl("Stat", colnames(df))])
            pcols <- colnames(df[1 ,grepl("PValue", colnames(df))])
            qcols <- colnames(df[1 ,grepl("FDR", colnames(df))])
            #print(statcols)
            
            incProgress(0.2)
            
            fig <- plot_ly(df, x = df[[statcols[[1]]]], y = df[[statcols[[2]]]], z = df[[statcols[[3]]]], marker = list(color = df$color, size=2),
                           hoverinfo="text",
                           text=c(paste(df$Name, 
                                        "<br>logFC(x):", round(df[[statcols[[1]]]], 3),
                                        "<br>p=", round(df[[pcols[[1]]]], 3),", q=", round(df[[qcols[[1]]]], 3),
                                        "<br>logFC(y):", round(df[[statcols[[2]]]], 3),
                                        "<br>p=", round(df[[pcols[[2]]]], 3),", q=", round(df[[qcols[[2]]]], 3),
                                        "<br>logFC(z):", round(df[[statcols[[3]]]], 3),
                                        "<br>p=", round(df[[pcols[[3]]]], 3),", q=", round(df[[qcols[[3]]]], 3)
                           )))
            
            incProgress(0.2)
            
            # generate properties table
            summary <- c("total"=nrow(df), table(df$color))
            summary_df <- t(data.frame(as.list(summary)))
            summary_df <- data.frame(summary_df)
            # add genes to the table
            unicl <- unique(df$color)
            gene_cats <- lapply(unicl, function(x){
                gl <- df[which(df$color==x),][["Name"]]
                paste(gl, collapse=" ")
            })
            gene_cats <- c(paste(df$Name, collapse=" "),gene_cats)
            summary_df$genes <- unlist(gene_cats)
            colnames(summary_df) <- c("n","Names")
            rv$n_3ds_prop <- summary_df
            
            fig <- fig %>% add_markers()
            fig <- fig %>% layout(title = paste0('3D Scatter, n=',nrow(df)),
                                  scene = list(xaxis = list(title = paste0(statcols[[1]])),
                                               yaxis = list(title = paste0(statcols[[2]])),
                                               zaxis = list(title = paste0(statcols[[3]]))))
            
        })
        fig
    })
    
    output$df_n_3ds <- renderPlotly({
        req(rv$df_n)
        req(rv$n_3ds_status=="ok")
        
        n_3ds_plt()
    })
    
    n_3ds_prop_df <- reactive({ rv$n_3ds_prop })
    # summary table
    output$n_3ds_prop_tbl <- DT::renderDataTable({
        n_3ds_prop_df()
    }, plugins="ellipsis",
    options=list(scrollX=T, scrollY=T, paging = FALSE, searching = FALSE, info=FALSE,
                 columnDefs = list(
                     list(
                         targets = "_all",
                         render = JS("$.fn.dataTable.render.ellipsis( 36, false )")
                     ))
    ))

    # download plotly html graph
    output$n_3ds_dl <- downloadHandler(
        filename = function() {paste("scatter-multiple-", Sys.Date(), ".html", sep = "")},
        content = function(file) {saveWidget(as_widget(n_3ds_plt()), file, selfcontained = TRUE)})
    
    # download summary table
    output$download_3ds_df <- downloadHandler(
        filename = function() {
            paste("summary-scatter-multiple-", Sys.Date(), ".csv", sep="")},
        content = function(file) {
            write.csv(n_3ds_prop_df(), file, 
                      row.names = T, quote=TRUE)})
    
    
    ####-------------------- table ------------------------####
    
    df_n_tbl <- reactive({
        
        df <- n_basic_df()
        
        # tidy row names
        if (nrow(df)>0){rownames(df) <- seq(1,nrow(df),1)}
        
        # to replace the stat col names
        colnames(df) <- gsub("Stat", rv$tt[[rv$nx_i[[1]]]], colnames(df))
        
        df
    })
    
    # display table
    output$df_n_tbl <- DT::renderDataTable({
        #req(length(rv$ll) >= 1)
        req(rv$df_n)
        
        df <- df_n_tbl()

        rv$df_n_fullcols <- colnames(df)

        # to abbreviate the long column names...take first 5 letters
        char_limit <- 56 / length(colnames(df))
        # print(char_limit)
        colnames(df) <- sapply(names(df), function(x){
            if (nchar(x)>char_limit)
            {return (paste0(substr(x, start = 1, stop = char_limit),"..."))}
            else{return (x)}
        })
        
        # to round everything down to 3 decimals
        df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
        

        df
        
        
    }, plugins = "ellipsis",
    options = list(scrollX=TRUE, 
                   columnDefs = list(
                       list(
                           targets = 1,
                           render = JS("$.fn.dataTable.render.ellipsis( 17, true )")
                       ),
                       list(
                           targets = "_all",
                           render = JS("$.fn.dataTable.render.ellipsis( 6, true )")
                       )
                   ),
        headerCallback= JS("function(thead, data, start, end, display){",
                                         sprintf("  var tooltips = [%s];", toString(paste0("'", rv$df_n_fullcols, "'"))),
                                         "  for(var i = 1; i <= tooltips.length; i++){",
                                         "    $('th:eq('+i+')',thead).attr('title', tooltips[i-1]);",
                                         "  }",
                                         "}")))
    

    
    # download current df
    output$download_n_df <- downloadHandler(
        filename = function() {
            paste("data", "-", "multiple", "-", Sys.Date(), ".csv", sep="")},
        content = function(file) {
            write.csv(df_n_tbl(), file, 
                      row.names = F, quote=TRUE)})
        
        
        

    
    
    
    
    
    ####--------------------multiple deg heatmap------------------------####
    
    #-------------- param -------------------#
    
    
    output$n_hm_cutoffs <- renderUI({
        req(is.null(rv$n_hm_cutmode)==F)
        
        
        if (rv$n_hm_cutmode == "All"){
            div(
                sliderTextInput("p_cutoff_2",
                                label = "Select P cutoff:",
                                choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                selected=0.05, grid=T, force_edges=T),
                sliderTextInput("q_cutoff_2",
                                label = "Select FDR cutoff:",
                                choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                selected=1, grid=T, force_edges=T),
                sliderTextInput("Stat_cutoff_2",
                                label = "Select |Stat| cutoff:",
                                choices= c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5),
                                selected=0, grid=T, force_edges=T),
                radioGroupButtons("sign_2",
                                  label = "Filter Stat by:",
                                  choices=c("All", "Positive", "Negative"),
                                  selected="All",size="s"
                ),
                "Note: This will be applied to all columns."
            )
        }
        else if (rv$n_hm_cutmode == "Single"){
            div(
                selectInput(
                    inputId = "n_cutoff_by",
                    label= "Cutoff by:",
                    choices = rv$iso_graph_dfs,
                    selected = rv$n_cutoff_by
                ),
                sliderTextInput("p_cutoff_2",
                                label = "Select P cutoff:",
                                choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                selected=0.05, grid=T, force_edges=T),
                sliderTextInput("q_cutoff_2",
                                label = "Select FDR cutoff:",
                                choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                selected=1, grid=T, force_edges=T),
                sliderTextInput("Stat_cutoff_2",
                                label = "Select |Stat| cutoff:",
                                choices= c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5),
                                selected=0, grid=T, force_edges=T),
                radioGroupButtons("sign_2",
                                  label = "Filter Stat by:",
                                  choices=c("All", "Positive", "Negative"),
                                  selected="All",size="s"
                ),
                "Note: This will only be applied to the selected column."
            )
            
        }
        
    })
    
    
    
    # sort by which dataset
    output$select_sortby_p2 <- renderUI({
        req(rv$df_n)
        radioButtons(
            inputId = "heatmap_sortby",
            label= "Sort heatmap by:",
            choices = rv$iso_graph_dfs,
            selected = rv$select_sortby_p2
            )
    })
    
    # which data column to plot (i.e. Stat, etc)
    output$n_to_plot <- renderUI({
        req(rv$df_n)
        selectInput(
            inputId = "n_to_plot",
            label= shiny::HTML("Plot data: 
                               <span style='color: gray'>(Note: only shared columns are selectable)</span>"),
            choices = rv$iso_sharedcols, # this displays all the shared columns, 
            selected = "Stat"
            )
    })
    

    
    
    
    
    
    
    output$n_heatmap <- renderUI({
        #req(length(rv$ll) >= 1)
        req(rv$df_n)
        # req(input$draw_heatmap)
        req(is.null(heatmap)==F)
        
        # plotOutput(
        #     outputId = "heatmap", width = "100%", height = "600px"
        # ) %>% withSpinner(type=4)
        plotlyOutput("heatmapp", height="600px")
    })
    
    

    
    # output$heatmap <- renderPlot({
    #     req(rv$df_n)
    #     req(is.null(rv$n_hm_showna)==F)
    #     
    #     plotted <- rv$df_n
    #     tol <- rv$n_hm_showna
    #     if (rv$n_hm_cutmode == "All"){
    #         plotted <- filter_by_sign(plotted, "Stat", rv$sign_2, tolerate=tol) # global filter
    #         plotted <- apply_n_cutoffs(plotted, rv$p_cutoff_2, rv$q_cutoff_2, rv$Stat_cutoff_2, tolerate=tol)
    #     }
    #     else if (rv$n_hm_cutmode == "Single"){
    #         plotted <- filter_by_sign(plotted, paste0("Stat_",rv$n_cutoff_by), rv$sign_2, tolerate = tol) # single filter
    #         plotted <- apply_single_cutoff(plotted, rv$n_cutoff_by, rv$p_cutoff_2, rv$q_cutoff_2, rv$Stat_cutoff_2, tolerate=tol)
    #     }
    #     
    #     print(head(plotted))
    #     
    #     rownames(plotted) <- plotted$Name # put genename as index
    #     plotted <- dplyr::select(plotted,contains(rv$n_to_plot)) # only extract wanted columns to plot
    #     
    #     req(nrow(plotted) != 0)
    #     
    #     #colnames(plotted) <- input$heatmap_dfs # name columns after analysis # rename columns (optional)
    #     plotted <- plotted[order(-plotted[paste(rv$n_to_plot,"_", rv$heatmap_sortby, sep="")]),] # sort in descending order based on selected column
    #     
    #     # to replace the stat col names 
    #     colnames(plotted) <- gsub("Stat", rv$tt[[rv$nx_i[[1]]]], colnames(plotted))
    #     
    #     hmplot <- pheatmap(plotted, 
    #                   #scale = "row", # don't use z score
    #                   cluster_rows=F, cluster_cols=T,
    #                   show_rownames=F, show_colnames=T) 
    #     return (hmplot)
    # })
    
    
    
    
    # plotly
    n_hm_plt <- reactive({
        # df <- rv$df_n
        # tol <- rv$n_hm_showna
        # if (rv$n_hm_cutmode == "All"){
        #     df <- filter_by_sign(df, "Stat", rv$sign_2, tolerate=tol) # global filter
        #     df <- apply_n_cutoffs(df, rv$p_cutoff_2, rv$q_cutoff_2, rv$Stat_cutoff_2, tolerate=tol)
        # }
        # else if (rv$n_hm_cutmode == "Single"){
        #     df <- filter_by_sign(df, paste0("Stat_",rv$n_cutoff_by), rv$sign_2, tolerate = tol) # single filter
        #     df <- apply_single_cutoff(df, rv$n_cutoff_by, rv$p_cutoff_2, rv$q_cutoff_2, rv$Stat_cutoff_2, tolerate=tol)
        # }
        
        # print(head(df))
        
        withProgress(message = 'Drawing heatmap...', value = 0, {
        
            df <- n_basic_df()
            
            rownames(df) <- df$Name # put genename as index
            
            # order by selected column
            sortby_coln <- paste0(rv$n_to_plot,"_", rv$heatmap_sortby)
            df <- df[order(-df[sortby_coln]),] 
            names <- df$Name # preserve formatting in vector
            incProgress(0.1)
            
            # extract plotted values
            to_match <- paste0(rv$n_to_plot, "_")
            plotted <- data.frame(t(dplyr::select(df,contains(to_match))))
            req(nrow(plotted) > 0)
            incProgress(0.1)
            
            # make matrix for plot
            dat <- expand.grid(x = rownames(plotted), y = addlinebreaks(names,30,"<br>"))
            dat$z <- unlist(plotted)
            req(length(dat$z)>0)
            incProgress(0.2)
            
            # put all the shared columns in the hovertext (as many as you have).
            sharedcols <- rv$n_sharedcols
            addlabel <- ""
            for (coln in sharedcols){
                le <- unlist(data.frame(t(dplyr::select(df,matches(paste0("^",coln,"_"))))))
                if (is.numeric(le)){
                    le <- round(le,3)
                }
                else if (is.character(le)){
                    le <- addlinebreaks(le,30,"<br>")
                }
                addlabel <- paste(addlabel, paste0(coln, ": ", le), sep="<br>")
            }
            incProgress(0.2)
            
            # define the hovertext
            textt <- ~paste(dat$y, addlabel)
            
            
            fig <- plot_ly() %>%
                add_trace(data = dat, x = ~x, y = ~y, z = ~z, type = "heatmap",
                          colorscale  = cscale_simple,zauto = T, zmid= 0, colorbar = list(title = rv$n_to_plot),
                          hoverinfo = 'text',
                          text = textt)
            incProgress(0.2)
            fig <- fig %>% layout(
                xaxis = list(title = "", showticklabels = T),
                yaxis = list(title = "", showticklabels = F)
                # ,margin = list(l=200)
            )
        
        })
        
        fig
    })
    output$heatmapp <- renderPlotly({
        req(rv$df_n)
        req(is.null(rv$n_hm_showna)==F)

        n_hm_plt()

    })
    
    
   
    # download plotly html graph
    output$n_hm_dl <- downloadHandler(
        filename = function() {paste("heatmap-multiple-", Sys.Date(), ".html", sep = "")},
        content = function(file) {saveWidget(as_widget(n_hm_plt()), file, selfcontained = TRUE)})

    
    
    ####--------------------wordcloud for pathways------------------------####
    
    map2color<-function(x,pal,limits=NULL){
        if(is.null(limits)) limits=range(x)
        pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
    }
    
    # compute the frequency table
    n_ins_wc_df <- reactive({
        vc <- n_ins_fgl()
        sep <- input$n_ins_wc_sep
        words <- unlist(lapply(vc, function(x){
            toupper(unlist(strsplit(x, sep)))
        }))
        words <- gsub("[[:punct:]]$", "", words) # get rid of punctuations at end
        words <- gsub("^[[:punct:]]", "", words) # get rid of punctuations at beginning
        
        df <- data.frame(table(words))
        
        # ignore certain words (supports regex pattern!!)
        ignore <- unlist(strsplit(input$n_ins_Wc_ignore, "\\s+"))
        # ignore <- c("GO", "KEGG", "of", "and", "pathway")
        # df <- df[df$words %in% ignore ==F,]
        pattern <- paste(ignore, collapse = "|")
        df <- df[-grep(pattern, df$words, ignore.case=TRUE),]
        
        df
    })
    
    n_ins_wc_plt <- reactive({
        df <- n_ins_wc_df()
        req(max(df$Freq)>1) # blocks further processing if no repeated words are found
        
        
        # only draw top x words
        df <- df[order(df$Freq, decreasing = TRUE), ]
        if (nrow(df)>150){
            df <- df[1:150, ] #or set 150 to whatever
        }
        
        # print(head(df))
        wordcloud(df$words, df$Freq, 
                  min.freq = 1, max.words=200, scale=c(3,.5),
                  random.order=FALSE, 
                  rot.per=0.2, # freq of vertical words
                  colors=brewer.pal(8, "Dark2"))
    })
    
    output$n_ins_wc <- renderPlot({
        req(length(n_ins_fgl())>0)
        
        n_ins_wc_plt()
    })
    
    output$n_ins_wc_df <- DT::renderDataTable({
        req(nrow(n_ins_wc_df())>0)
        n_ins_wc_df()[order(n_ins_wc_df()$Freq, decreasing=T),]
    }
    , plugins="ellipsis",
    options=list(scrollX=T, scrollY=T, dom= 'tp',
                 pageLength = 10
                 ),
    rownames= FALSE
    )
    
    

    
    # ####-------------------- Info ------------------------####
    # output$text1 <- renderUI({
    #     if(input$More_info=="Introduction"){
    #         includeHTML("inc/introduction.html")
    #     } else if(input$More_info=="Information"){
    #         includeHTML("inc/information.html")
    #     } else if(input$More_info=="Help"){
    #         includeHTML("inc/help.html")
    #     } else if(input$More_info=="References"){
    #         includeHTML("inc/references.html")
    #     }
    # })
    
    ####------------------- DEBUG -----------------------####
    
    # DEBUG (single)
    output$odataset <- renderPrint({
        paste("rv show_df"," = ", rv$show_df, ", ",
              "rv_genelist"," = ", rv$genelist, ", ",
              "input$genelist_p1"," = ", input$genelist_p1, ", ",
              "Table index"," = ", current(), ",",
              "rv$x_i = ", rv$x_i, ",",
              "ll size = ", length(rv$ll),
              "gg size = ", length(rv$gg))
    })
    output$debugxy <- renderPrint({
        paste("rv$selected_x = ", rv$selected_x, ", ",
              "rv$ll = ", rv$ll, ",",
              "ll size = ", length(rv$ll), ",",
              "gg size = ", length(rv$gg))
    })
    output$debug2 <- renderPrint({
        paste("length(rv$nx_i)= ",length(rv$nx_i), ", ",
              "input$n_hm_showna = ", input$n_hm_showna, ",",
              "sorthmby = ", paste0(rv$n_to_plot,"_", rv$heatmap_sortby), ",",
              "p_cutoff = ", rv$p_cutoff_2, ",",
              "ll size = ", length(rv$ll), ",",
              "gg size = ", length(rv$gg))
    })
    output$debug0 <- renderPrint({
        paste("rv$folder_upload_state = ", rv$folder_upload_state, ", ",
              "rv$batch_files = ", rv$batch_files$name, ", ",
              "rv$batch_failed = ", rv$batch_failed, ", ",
              "ll size = ", length(rv$ll), ",",
              "gg size = ", length(rv$gg))
    })

    
    # # # pass the rv to client (required for conditional panels to use rv values)
    # output$n_len <- reactive({
    #     return(length(rv$nx_i))
    # })
    # outputOptions(output, "n_len", suspendWhenHidden = F)

    
}
# ===================================================== server.R END ============
