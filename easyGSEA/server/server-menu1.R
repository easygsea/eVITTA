# RNK help --------------
    observeEvent(input$q1,{
        showModal(modalDialog(
            inputId = "rank_md",
            title = "Ranked list file format (*.rnk)",
            includeMarkdown(paste0(getwd(),"/inc/rnk_explaination.md")),
            # includeMarkdown(knitr::knit(paste0(getwd(),"/inc/rnk_explaination.Rmd"),quiet=T)),
            easyClose = TRUE,size="l",
            footer = modalButton("Close")
        ))
    })
    
#-------------- UI select mode of analysis ----------------
output$ui_mode <- renderUI({
    box(
        title = NULL, background = "yellow", solidHeader = T, width = 12,
        radioButtons(
            inputId = "selected_mode",
            label = "Mode of analysis",
            choices = run_modes,
            selected = "gsea"
        )
    )
})

# UI select species ------------------
    
    output$select_species <- renderUI({
        selectizeInput(
            "selected_species",
            "1. Select species that matches your input query:",
            choices = species_names,
            options = list(
                placeholder = 'Type to search ...',
                onInitialize = I('function() { this.setValue(""); }')
            )
        )
    })
    
    # disable selection when user confirms gmts; enables upon modify
    # this is to prevent accidentally messing up selections by changing species
    observe({
        req(is.null(rv$db_status)==F)
        if (rv$db_status == "selected"){
            shinyjs::disable("selected_species")
        }
        else if (rv$db_status == "modify"){
            shinyjs::enable("selected_species")
        }
    })

    #-------------- UI select GMTs ----------------
    
    observe({
        req(nchar(input$selected_species)>0)
        req(is.null(rv$db_status)==TRUE || rv$db_status == "modify")
        
        species <- input$selected_species

        for(collection in sort(names(gmt_collections_paths[[species]]))){
            # print(paste0(species,gsub(" ","_",collection)))
            rv$v[[species]][[collection]] <- div(
                checkboxGroupInput(
                    inputId = paste0(species,gsub(" ","_",collection)),
                    label = strsplit(collection,"_")[[1]][[2]],
                    choices = sort(gmt_collections[[species]][[collection]]),
                    selected = gmt_collections_selected[[species]][[collection]]

                )
            )
        }
    })
    
    output$test_db <- renderUI({
        req(nchar(input$selected_species)>0)
        req(is.null(rv$db_status)==TRUE || rv$db_status == "modify")
        
        species = input$selected_species

        return(rv$v[[species]])
    })
    
    
    #-------------- button control of gmt selection ----------------
    
    # add / modify button
    observeEvent(input$add_db, {
        rv$db_status <- "selected"
        rv$dbs = NULL
        
        species<-input$selected_species
        
        for(collection in sort(names(gmt_collections_paths[[species]]))){
            db_id = paste0(species,gsub(" ","_",collection))
            # db_name = input[[db_id]]
            rv$dbs = c(rv$dbs,gmt_collections[[species]][[collection]][which(gmt_collections[[species]][[collection]] %in% isolate(input[[db_id]]))])
        }
    })
    
    observeEvent(input$add_db_modify, {
        rv$db_status <- "modify"
    })
    
    output$bs_add_db <- renderUI({
        req(input$selected_species != "")
        if ((is.null(rv$db_status)==TRUE) || (rv$db_status=="modify")){
            bsButton(
                inputId = "add_db", 
                label = "Confirm selection",
                style = "success",
                type = "button")
        }
        else if (rv$db_status == "selected"){
            bsButton(
                inputId = "add_db_modify", 
                label = "Modify selection",
                style = "info",
                type = "button")
        }
        
    })
    
    # reset button
    observeEvent(input$reset_db, {
        species <- input$selected_species
        for(collection in sort(names(gmt_collections_paths[[species]]))){
            updateCheckboxGroupInput(session,
                                     inputId = paste0(species,gsub(" ","_",collection)),
                                     label = strsplit(collection,"_")[[1]][[2]],
                                     choices = sort(gmt_collections[[species]][[collection]]),
                                     selected = gmt_collections_selected[[species]][[collection]]
            )
        }
    })
    
    output$bs_reset_db <- renderUI({
        req(input$selected_species != "")
        req(is.null(rv$db_status)==T || rv$db_status == "modify")
        bsButton(
            inputId = "reset_db", 
            label = "Reset",
            style = "info",
            type = "button")
    })
    
# ------------ Upload & reset RNK -------------
    # UI file input
    output$ui_rnk <- renderUI({
        req(input$selected_mode == "gsea")
        # req(rv$db_status == "selected")
        # div(
        #     class = "btn-danger",
            fileInput("rnkfile",
                      label = p("2. Upload RNK file:",
                                tags$style(type = "text/css", "#q1 {display: inline-block;width: 20px;height: 20px;padding: 0;border-radius: 50%;vertical-align: baseline;margin-left: 160px;}"),
                                bsButton("q1", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                      buttonLabel = "Upload...",
                      accept = c(
                          "text/tab-separated-values",
                          "text/comma-separated-values",
                          ".csv",".txt",".tab",".tsv",
                          ".rnk")

            )
        # )

    })
    
    # UI reset
    output$bs_file_reset <- renderUI({
        req(input$selected_mode == "gsea")
        req(is.null(rv$infile_name)==F)
        bsButton(
            inputId = 'reset',
            label = 'Reset file',
            style = "info",
            type = "button")
    })
    
    # reset RNK input widget
    observeEvent(input$reset, {
        rv$file_upload_status = "reset"
        rv$infile_confirm = NULL
        rv$infile_name = NULL
        rv$infile_path = NULL
        rv$rnk_or_deg = NULL
        # rv$run = NULL
        shinyjs::reset("rnkfile")
        shinyjs::enable("rnkfile")
        rv$infile_check = NULL
        rv$example_file = NULL
    })
    
    # read in RNK file path name, disable widget
    observeEvent(input$rnkfile, {
        rv$file_upload_status = "uploaded"
        rv$infile_name = input$rnkfile$name
        rv$infile_path = input$rnkfile$datapath
        shinyjs::disable("rnkfile")
    })
    
        
# ------------- Upload and reset example RNK/DE --------------
    observeEvent(input$loadExampleRNK,{
        rv$example_file = NULL
        if(input$selected_species == ""){
            showNotification("Please select your species of interest.",type="error",duration=2)
        }else{
            sampleRNK_file <- paste0(getwd(),"/inc/",input$selected_species,".rnk")
            rv$infile_name = paste0(input$selected_species,".rnk")
            rv$infile_path = sampleRNK_file
            shinyjs::reset("rnkfile")
            shinyjs::disable("rnkfile")
            removeModal()
            rv$infile_check = NULL
            rv$example_file = "yes"
            rv$file_upload_status = "uploaded"
        }
    })
    
    observeEvent(input$loadExampleDE,{
        rv$example_file = NULL
        if(input$selected_species == ""){
            showNotification("Please select your species of interest.",type="error",duration=2)
        }else{
            sampleDE_file <- paste0(getwd(),"/inc/",input$selected_species,".csv")
            rv$infile_name = paste0(input$selected_species,".csv")
            rv$infile_path = sampleDE_file
            shinyjs::reset("rnkfile")
            shinyjs::disable("rnkfile")
            removeModal()
            rv$infile_check = NULL
            rv$example_file = "yes"
            rv$file_upload_status = "uploaded"
        }
    })

    
#---------- Return RNK ---------
    # check and store input file content into rv$data_head
    observe({
        req(is.null(rv$infile_name)==F)
        rv$infile_check=NULL
        rv$input_symbol = NULL
        rv$rnkll <- strsplit(isolate(rv$infile_name),"\\.(?=[^\\.]+$)", perl=TRUE)[[1]][1] # add value to rv
        ranks <- read_delim(isolate(rv$infile_path), "," , escape_double = FALSE, trim_ws = TRUE)
        ranks = ranks[complete.cases(ranks), ]
        
        # print(str(head(ranks)))
        if(ncol(ranks)==1){
            ranks <- read_delim(isolate(rv$infile_path), "\t" , escape_double = FALSE, trim_ws = TRUE)
            ranks = ranks[complete.cases(ranks), ]
        }
        
        # detect if RNK or DEG
        if(ncol(ranks)==2){
            rv$rnk_or_deg = "rnk"
        }else if(ncol(ranks)>2){
            rv$rnk_or_deg = "deg"
        }else{
            showNotification("You probably uploaded a wrong file. Please check",type = "error",duration = 3)
        }
        
        # save had data into RV
        rv$data_head = ranks
    })
    
    # convert into ranks
    observeEvent(input$filecontent_confirm,{
        rv$infile_confirm = "confirm"
        data = rv$data_head
        
        # total no of genes before conversion
        rv$total_genes = nrow(data)
        
        # clear rv which was used to store input file data
        rv$data_head = NULL
        
        if(ncol(data)==2){
            if(is.numeric(data[[input$rank_column]])){
                ranks <- setNames(data[[input$rank_column]], data[[input$gene_column]])
                rv$infile_check = "pass"
                rv$rnkgg = ranks
            }else{
                rv$infile_check = "wrong_rnk"
            }
        }else if(ncol(data)>2){
            genes <- data[[input$gene_column]]
            logfc <- data[[input$logfc_column]]
            pval <- data[[input$p_column]] #%>% mutate_if(is.numeric,  ~replace(., . == 0, 0.00001))
            pval[pval==0] = 0.000000001
            if(is.numeric(pval) && is.numeric(logfc)){
                rank_values <- -log10(pval) * sign(logfc)
                ranks <- setNames(rank_values,genes)
                rv$infile_check = "pass"
                rv$rnkgg <- ranks # add value to rv
            }else{
                rv$infile_check = "wrong_deg"
            }
        }
        
        # total no of genes before conversion
        rv$total_genes_after = length(rv$rnkgg)
        
    })
    
#====================================================#
# ----------- GList mode: input gene lists ----------#
#====================================================#
    output$ui_glist <- renderUI({
        req(input$selected_mode == "glist")
        # req(input$selected_species != "")
        # req(is.null(rv$dbs)==F)
        div(
            textAreaInput(
                inputId = "gene_list",
                label = p("2. Input your gene list (",
                          tags$style(type = "text/css", "#load_example_glist {display: inline-block;height: 20px;padding: 0;vertical-align: baseline;}"),
                          actionLink("load_example_glist", label = tags$u("example data")),
                          "):"
                          ),
                placeholder = "Type to input..",
                height = 150
            ),
            fluidRow(
                column(
                    width = 7,
                    fileInput("glistfile","Or, upload a file",
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         "text/plain"
                                         )
                              )
                ),
                column(
                    width = 2, #offset = 6,
                    br(),
                    bsButton(
                        inputId = "gene_list_clear",
                        label = "Clear",
                        style = "info"
                    )
                ),
                column(
                    br(),
                    width = 2, #offset = 1,
                    bsButton(
                        inputId = "gene_list_add",
                        label = "Submit",
                        style = "success"
                    )
                )
            )
        )
    })
    
    #----------------read in lists-----------------
    # from a file
    observeEvent(input$glistfile, {
        # get file
        infile <- input$glistfile
        data <- read_csv(infile$datapath)
        data = unique(data[[1]][data[[1]] != ""])
        # print(str(data))
        

        # data <- lapply(data, function(x) toupper(unique(x[!is.na(x)])))
        showNotification(paste0(infile$name," has ", length(data)," genes Click Add to confirm. Or Browse again for another file."),type="warning",duration=3)
        
        rv$data_glist = data
        
        rv$rnkll = strsplit(isolate(infile$name),"\\.(?=[^\\.]+$)", perl=TRUE)[[1]][1]
    })
    
    # from input field
    observeEvent(input$gene_list_add,{
        print(input$gene_list)
        if(is.null(rv$data_glist)==T){
            if(input$gene_list != ""){
                genelist = as.character(input$gene_list)
                genelist = gsub("\"","",genelist)
                genelist = strsplit(genelist,"\n")
                genelist = unlist(lapply(genelist, function(x) strsplit(x,'\\s*,\\s*')))
                genelist = unlist(lapply(genelist, function(x) strsplit(x,'\\s*;\\s*')))
                genelist = unlist(strsplit(genelist," "))
                genelist = unique(genelist)
                
                if(is.null(genelist)==F){
                    rv$gene_lists = genelist
                    rv$rnkll = "unamed"
                }
            }
        }else{
            rv$gene_lists = rv$data_glist
        }
                
    })
    
    # disable file upload
    observe({
        req(is.null(rv$data_glist)==F)
        shinyjs::disable("glistfile")
    })
    
    # clear input
    observeEvent(input$gene_list_clear, {
        shinyjs::reset("glistfile")
        shinyjs::enable("glistfile")
        updateTextAreaInput(session,
                            inputId = "gene_list",
                            value = "",
                            placeholder = "Type to input.."
        )
    })
    
    #----------- Example gene lists --------------
    observeEvent(input$load_example_glist,{
        if(input$selected_species == ""){
            showNotification("Please select your species of interest.",type="error",duration=2)
        }else{
            updateTextAreaInput(session,
                                inputId = "gene_list",
                                value = gsub(";","\n",glist_example[input$selected_species][[1]])
            )
        }
    })
    
    

#---------- UI confirm & confirm modals ---------
    # clear Glist rv when switching to gsea mode
    observe({
        req(input$selected_mode == "gsea")
        rv$gene_lists = NULL
    })
    
    # UI GSEA parameter
    output$ui_gsea_par <- renderUI({
        req(is.null(rv$dbs)==F)
        req(input$selected_species != "")
        req((is.null(rv$infile_name)==F & is.null(rv$infile_check)==F)|(is.null(rv$gene_lists)==F))
        
        box(
            width = 12,
            shiny::HTML("<p style='font-style:italic'>Run parameters</p>"), #&nbsp&nbsp&nbsp&nbsp&nbsp
            splitLayout(
                numericInput("mymin", "Min:",15),
                numericInput("mymax", "Max:",500),
                uiOutput("ui_nperm")
            )
        )
    })

    output$ui_nperm <- renderUI({
        req(input$selected_mode == "gsea")
        numericInput("nperm", "# perm:",1000)
    })
    
    # UI confirm GSEA
    output$run_GSEA <- renderUI({
        req(input$selected_mode == "gsea")
        req(is.null(rv$infile_name)==F)
        req(is.null(rv$dbs)==F)
        req(input$selected_species != "")
        req(is.null(rv$infile_check)==F)
        
        bsButton(inputId = "confirm1", 
                 label = "RUN GSEA", 
                 # block = TRUE,
                 icon = icon("play-circle"), 
                 style = "danger")
    })
    
    # UI confirm GList
    output$run_GList <- renderUI({
        req(input$selected_mode == "glist")
        req(is.null(rv$dbs)==F)
        req(input$selected_species != "")
        req(is.null(rv$gene_lists)==F)
        
        bsButton(inputId = "confirm2", 
                 label = "RUN GLIST", 
                 # block = TRUE,
                 icon = icon("play-circle"), 
                 style = "danger")
    })
    
    # modals: first confirm selection, then name analysis. gsea runs when new name is assigned.
    # observeEvent(input$confirm1, {
    #     rv$run = NULL
    #     rv$fgseagg=NULL
    #     rv$gmts=NULL
    #     
    #     # first modal
    #     shinyalert(inputId = "confirm_inputs",
    #                # showModal(modalDialog(
    #                title = "Confirm input", 
    #                text = paste0("Selected databases: ",tags$br(),
    #                              "MSigDB (", length(input$selected_db),
    #                              "), ",
    #                              "External (", length(input$selected_db_ext),")",tags$br(),
    #                              "Selected rnk: ",
    #                              tags$br(),
    #                              rv$infile_name
    #                ),  # revise it using html
    #                type = "info",
    #                html = T,
    #                showCancelButton = T,
    #                confirmButtonText = "Confirm",
    #                animation = T
    #                
    #                # second modal 
    #                # callbackR = function(value){
    #                #     shinyalert(inputId = "gs_newname",
    #                #         title = "Enter analysis name:",
    #                #         text = "This is a modal",
    #                #         type = "input",
    #                #         inputType = "text",
    #                #         inputValue = "my_analysis",
    #                #         inputPlaceholder = "my_analysis",
    #                #         confirmButtonText = "Run!",
    #                #         animation = T
    #                #     )
    #                # }
    #     )
    # })
    
    #===============================================#
    #####               run GSEA!!!             #####
    #===============================================#
    # runs upon the analysis name is provided.
    observeEvent(input$confirm1, {
        
        rv$run_mode = "gsea"
        ranks = rv$rnkgg
        names(ranks) = toupper(names(ranks))
        
        sd = sd(ranks); rv$sd_high = sd * 2.5
        
        rv$gmin=isolate(input$mymin)
        rv$gmax=isolate(input$mymax)
        rv$gperm=isolate(input$nperm)

        # reset RVs
        rv$run = NULL
        rv$fgseagg=NULL
        rv$gmts=NULL
        
        rv$no_up_01 = 0;rv$no_up_05 = 0;rv$no_down_01 = 0;rv$no_down_05 = 0
        
        rv$es_term = NULL
        
        rv$kegg_yes=NULL;rv$kegg_confirm=NULL;rv$reactome_yes=NULL;rv$reactome_confirm=NULL
        rv$wp_yes = NULL;rv$wp_confirm=NULL;rv$vis=NULL

        
               
   # observeEvent(input$confirm_inputs,{

        # substitute in default name if no name provided.
        # if(nchar(input$gs_newname)==0){
        #     rv$newname <- "my_analysis"
        # }
        # else{
        #     rv$newname <- input$gs_newname
        # }

        rv$bar_pathway = rv$dbs
        rv$bubble_pathway = rv$dbs
        rv$volcano_pathway = rv$dbs
        
        species <- isolate(input$selected_species)
        
        # # convert IDs
        # N = 1
        # withProgress(message = "Checking input gene IDs...",{
        # 
        #     # for(i in 1:N){
        #     Sys.sleep(.01)
        #     incProgress(1)
        #     # incProgress(1/N)
        #     # }
        # 
        #     # gconvert to NCBI ACC #
        #     ranks = rv$rnkgg
        #     results = gconvert(
        #         names(ranks),
        #         organism = species_names_go[species][[1]],
        #         target = "ENTREZGENE_ACC", # results$target is ACC, results$name is ACC name
        #         numeric_ns = "",
        #         mthreshold = Inf,
        #         filter_na = TRUE
        #     )
        # 
        #     test = results %>% filter(input == name)
        #     perc = nrow(test)/nrow(results)
        #     if(perc > 0.7){
        #         # input is SYMBOL
        #         rv$input_symbol = "yes"
        #         ranks = ranks[names(ranks) %in% test$input]
        # 
        #         # load to rv
        #         rv$rnkgg = ranks
        #         rv$input_mat = test
        #     }else{
        #         # input is other type, rename rank list
        #         test = distinct(results, input, .keep_all = TRUE)
        #         ranks = ranks[names(ranks) %in% test$input]
        #         names(ranks) = test$name[test$input %in% names(ranks)]
        # 
        #         # load to rv
        #         rv$rnkgg = ranks
        #         rv$input_mat = test
        #     }
        # })
        
        withProgress(message = "Running GSEA analysis...",value = 0.2, {

            # ------ read GMTs into list ------ #
            # gmts are temporarily stored in list and will be looped over during gsea run.
            
            # initialize
            gmts=list()
            catnames=list()
            
            for(collection in sort(names(gmt_collections_paths[[species]]))){
                db_id = paste0(species,gsub(" ","_",collection))
                if(is.null(input[[db_id]])==F){
                    for(cat_name in input[[db_id]]){
                        gmt_path = gmt_collections_paths[[species]][[collection]][[cat_name]]
                        m_list <- gmtPathways(gmt_path)

                        gmts = c(gmts,list(m_list))
                        catnames <- c(catnames, cat_name)
                        incProgress(0.1)
                    }
                 }
                
            }
            
            
            # compile gmts from msigdb
            # if(is.null(input$selected_db)==F){
            #     for(cat_name in isolate(input$selected_db)) {
            #         # define cat name and sub category name
            #         cat = unlist(strsplit(cat_name, "_"))
            #         subcat = cat[2]
            #         cat = cat[1]
            #         if(is.na(subcat)==TRUE){subcat=""}
            #         
            #         # subset database
            #         # m_df_s = m_df[((m_df$gs_cat==cat)&(m_df$gs_subcat==subcat)),]
            #         spc <- species_translate(input$selected_species)  # translate from abbreviation to full name
            #         m_df_s = msigdbr(species = spc, category = cat, subcategory = subcat)
            #         if(nrow(m_df_s)==0){next}
            #         m_list = m_df_s %>% split(x = .$gene_symbol, f = .$gs_name)
            #         
            #         m_df_s = NULL
            #         
            #         # put all loaded gmts in a list
            #         gmts = c(gmts,list(m_list))
            #         # gmts = c(gmts,m_list)
            #         catnames <- c(catnames, cat_name)
            #         incProgress(0.1)
            #     }
            # }
            
            # save GMTs to rv
            rv$gmts = unlist(gmts,recursive = F)

            # ------ run fgsea ------ #

            for (i in seq_along(gmts)){
                # uppercase genes
                m_list = lapply(gmts[[i]], function(x) toupper(x))
                
                fgseaRes <- fgsea(pathways = m_list,
                                  stats    = ranks,
                                  minSize  = rv$gmin,
                                  maxSize  = rv$gmax,
                                  nperm = rv$gperm)
                # fgseaRes <- fgseaMultilevel(
                #     pathways = gmts[[i]],
                #     stats = ranks,
                #     minSize = rv$gmin,
                #     maxSize = rv$gmax
                # )
                db <- rep(catnames[[i]], nrow(fgseaRes))
                fgseaRes <- cbind(db,fgseaRes)
                #print(head(fgseaRes))
                rv$fgseagg <- rbind(rv$fgseagg, fgseaRes)
                # rv$fgseagg <- c(rv$fgseagg, list(fgseaRes))
                rv$no_up_01 = rv$no_up_01 + sum(fgseaRes$padj<0.01&fgseaRes$ES>0,na.rm=TRUE)
                rv$no_up_05 = rv$no_up_05 + sum(fgseaRes$padj<0.05&fgseaRes$ES>0,na.rm=TRUE)
                rv$no_down_01 = rv$no_down_01 + sum(fgseaRes$padj<0.01&fgseaRes$ES<0,na.rm=TRUE)
                rv$no_down_05 = rv$no_down_05 + sum(fgseaRes$padj<0.05&fgseaRes$ES<0,na.rm=TRUE)
                # rv$fgseagg <- c(rv$fgseagg, list(catnames[[i]] = fgseaRes))
                incProgress(0.2)
            }
            
            # rv$fgseagg = rv$fgseagg[order(padj)]
            # rv$fgseagg[["pathway"]] <- lapply(rv$fgseagg[["pathway"]], as.character)
            # flatten list of gmts to single list, for future use
            # rv$gmts = do.call(c, unlist(gmts,recursive = F))
            l = unlist(lapply(rv$gmts, function(x){return(length(x)>=rv$gmin && length(x)<=rv$gmax)}))
            rv$gmts_length = sum(l)
            incProgress(0.1)
            
            # determine if success or warnings
            if(is.null(rv$fgseagg)==FALSE){
                rv$run = "success"
            } else {
                rv$run = "failed"
            }
            incProgress(0.1)
        })
    })
    
    #===============================================#
    #####               run GList!!!            #####
    #===============================================#
    
    observeEvent(input$confirm2, {
        rv$run_mode = "glist"
        
        # reset RVs
        rv$kegg_yes=NULL;rv$kegg_confirm=NULL;rv$reactome_yes=NULL;rv$reactome_confirm=NULL
        rv$wp_yes = NULL;rv$wp_confirm=NULL;rv$vis=NULL
        
        # read in parameters
        
        genelist = toupper(rv$gene_lists)
        
        rv$run = NULL
        rv$fgseagg=NULL
        rv$gmts=NULL
        
        rv$no_up_01 = 0;rv$no_up_05 = 0
        
        rv$gmin=isolate(input$mymin)
        rv$gmax=isolate(input$mymax)

        rv$bar_pathway = rv$dbs
        rv$bubble_pathway = rv$dbs

        species <- isolate(input$selected_species)

        withProgress(message = "Running overrepresentation analysis...",value = 0.2, {
            
            # ------ read GMTs into list ------ #
            # gmts are temporarily stored in list and will be looped over during gsea run.
            
            # initialize
            gmts=list()
            catnames=list()
            all_genes=list()
            
            for(collection in sort(names(gmt_collections_paths[[species]]))){
                db_id = paste0(species,gsub(" ","_",collection))
                if(is.null(input[[db_id]])==F){
                    for(cat_name in input[[db_id]]){
                        gmt_path = gmt_collections_paths[[species]][[collection]][[cat_name]]
                        m_list <- gmtPathways(gmt_path)
                        
                        # get all genes
                        a_genes = toupper(unname(unlist(m_list,recursive = T)))
                        all_genes <- c(all_genes, list(a_genes))
                        
                        gmts = c(gmts,list(m_list))
                        catnames <- c(catnames, cat_name)
                        incProgress(0.1)
                    }
                }
                
            }
            
            # compile gmts from msigdb
            # if(is.null(input$selected_db)==F){
            #     for(cat_name in isolate(input$selected_db)) {
            #         # define cat name and sub category name
            #         cat = unlist(strsplit(cat_name, "_"))
            #         subcat = cat[2]
            #         cat = cat[1]
            #         if(is.na(subcat)==TRUE){subcat=""}
            #         
            #         # subset database
            #         # m_df_s = m_df[((m_df$gs_cat==cat)&(m_df$gs_subcat==subcat)),]
            #         spc <- species_translate(input$selected_species)  # translate from abbreviation to full name
            #         m_df_s = msigdbr(species = spc, category = cat, subcategory = subcat)
            #         if(nrow(m_df_s)==0){next}
            #         m_list = m_df_s %>% split(x = .$gene_symbol, f = .$gs_name)
            # 
            #         m_df_s = NULL
            #         
            #         # get all genes
            #         a_genes = unname(unlist(m_list,recursive = T))
            #         all_genes <- c(all_genes, list(a_genes))
            #         
            #         # put all loaded gmts in a list
            #         gmts = c(gmts,list(m_list))
            #         # gmts = c(gmts,m_list)
            #         catnames <- c(catnames, cat_name)
            #         incProgress(0.1)
            #     }
            # }
            
            # save GMTs to rv
            rv$gmts = unlist(gmts,recursive = F)

            # ------ run fgsea ------ #
            for (i in seq_along(gmts)){
                # uppercase genes
                m_list = lapply(gmts[[i]], function(x) toupper(x))
                
                fgseaRes <- fora(pathways = m_list,
                                  genes    = genelist[genelist %in% all_genes[[i]]],
                                  universe = all_genes[[i]],
                                  minSize  = rv$gmin,
                                  maxSize  = rv$gmax
                                  )
                

                db <- rep(catnames[[i]], nrow(fgseaRes))
                fgseaRes <- cbind(db,fgseaRes)
                # print(str(head(fgseaRes)))
                rv$fgseagg <- rbind(rv$fgseagg, fgseaRes)
                # rv$fgseagg <- c(rv$fgseagg, list(fgseaRes))
                rv$no_up_01 = rv$no_up_01 + sum(fgseaRes$padj<0.01,na.rm=TRUE)
                rv$no_up_05 = rv$no_up_05 + sum(fgseaRes$padj<0.05,na.rm=TRUE)
                # rv$fgseagg <- c(rv$fgseagg, list(catnames[[i]] = fgseaRes))
                incProgress(0.2)
            }
            # print(str(head(rv$fgseagg)))
            # # flatten list of gmts to single list, for future use
            # rv$gmts = do.call(c, unlist(gmts,recursive = F))
            l = unlist(lapply(rv$gmts, function(x){return(length(x)>=rv$gmin && length(x)<=rv$gmax)}))
            rv$gmts_length = sum(l)
            incProgress(0.1)
            
            # determine if success or warnings
            if(is.null(rv$fgseagg)==FALSE){
                rv$run = "success"
            } else {
                rv$run = "failed"
            }
            incProgress(0.1)
        })
    })