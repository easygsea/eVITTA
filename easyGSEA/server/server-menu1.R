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

# UI select databases ------------------
    
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
        
        fluidRow(
            column(
                width = 12,
                tags$script(HTML('$(document).ready(function(){
      $("#showdbs_collapsible").on("hide.bs.collapse", function(){
        $("#showdbs").html("<span class=\\\"glyphicon glyphicon-collapse-down\\\"></span> Advanced database options ...");
      });
      $("#showdbs_collapsible").on("show.bs.collapse", function(){
        $("#showdbs").html("<span class=\\\"glyphicon glyphicon-collapse-up\\\"></span> Advanced database options ...");
      });
    });')),
                bsButton("showdbs", "Advanced database options ...", 
                         icon = icon("collapse-down", lib = "glyphicon"),
                         style = "default",
                         type = "toggle",
                         # class = "btn-primary btn-sm", 
                         `data-toggle`="collapse", 
                         `data-target` = "#showdbs_collapsible"
                         ),
                br(),
                conditionalPanel('input.showdbs % 2 == 1',
                                 rv$v[[species]],
                                 uiOutput("bs_reset_db")
                ),
                br()
            )
        )
    })
    
    
    #-------------- button control of gmt selection ----------------
    
    # write selected databases into RV
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
    
    # reset species, at the same time reset rnk/glist
    observeEvent(input$add_db_modify, {
      rv$db_status <- "modify"
      
      # clear RVs
      rv$run = NULL
      rv$glist_check = NULL
      rv$rnk_check = NULL
      rv$infile_check = NULL
      rv$example_file = NULL
      rv$infile_confirm = NULL
      rv$infile_name = NULL
      rv$infile_path = NULL
      rv$file_upload_status = NULL
      rv$rnk_or_deg = NULL
      rv$gene_lists_mat = NULL
      
      # rest glist UIs
      shinyjs::reset("gene_list")
      shinyjs::enable("gene_list")
      shinyjs::reset("glist_name")
      shinyjs::enable("glist_name")
      
      # reset gsea UIs
      shinyjs::reset("rnkfile")
      shinyjs::enable("rnkfile")  
        
    })
    
    # add / modify button
    output$bs_add_db <- renderUI({
        req(input$selected_species != "")
        if ((is.null(rv$db_status)==TRUE) || (rv$db_status=="modify")){
          fluidRow(
            column(
              width = 12,
              bsButton(
                inputId = "add_db", 
                label = "Confirm selection",
                style = "primary",
                type = "button"),
              br(),br()
              )
          )
        }
        else if (rv$db_status == "selected"){
          fluidRow(
            column(
              width = 12,
              bsButton(
                inputId = "add_db_modify", 
                label = "Modify selection",
                style = "default",
                type = "button"),
              br(),br()
            )
          )
        }
        
    })
    
    # reset button
    observeEvent(input$reset_db, {
        rv$run = NULL
        
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
                      label = p("3. Upload RNK file:",
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
            inputId = "reset",
            label = "Reset file",
            style = "default",
            type = "button")
    })
    
    # reset RNK input widget
    observeEvent(input$reset, {
      rv$file_upload_status = "reset"
      
      rv$run = NULL
      rv$rnk_check = NULL
      rv$infile_check = NULL
      rv$example_file = NULL
      rv$infile_confirm = NULL
      rv$infile_name = NULL
      rv$infile_path = NULL
      rv$file_upload_status = NULL
      rv$rnk_or_deg = NULL
      rv$gene_lists_mat = NULL
      
      shinyjs::reset("rnkfile")
      shinyjs::enable("rnkfile")
              
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
        rv$gene_lists_mat = NULL
        rv$run = NULL
        
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
                rv$rnkgg <- ranks 
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
                rv$rnkgg <- ranks 
            }else{
                rv$infile_check = "wrong_deg"
            }
        }
        
        if(is.null(rv$rnkgg)==F){
          if(input$gene_identifier=="other"){
            # autodetect and convert into SYMBOL (if applicable) using gprofiler2
            species = isolate(input$selected_species)
            
            withProgress(message = "Autodetecting and converting gene IDs...",{
              Sys.sleep(0.1)
              incProgress(1)
              lst = convert_rank_id(species,rv$rnkgg)
              
              if(is.null(lst)){
                # no ID detected in database
                rv$rnk_check = "none"
                rv$rnkgg = NULL
              }else{
                # check percentage of IDs found in database
                g_perc = lst[[1]]
                
                # if <30%, reports error
                if(g_perc < 0.5){
                  rv$rnk_check = "low"
                }else{
                  rv$rnk_check = "pass"
                }
                
                # convert ID and save converted IDs & conversion table into RVs
                rv$rnkgg = lst[[2]]
                rv$gene_lists_mat = lst[[3]]
                
                # count # of genes after conversion
                rv$total_genes_after = length(rv$rnkgg)
                
              }
            })
          }else{
            rv$rnk_check = "pass"
            rv$total_genes_after = length(rv$rnkgg)
          }
            
            
            
        }
    })
    
#====================================================#
######       GList mode: input gene lists       ######
#====================================================#
    output$ui_glist <- renderUI({
        req(input$selected_mode == "glist")
        # req(input$selected_species != "")
        # req(is.null(rv$dbs)==F)
        div(
            textAreaInput(
                inputId = "gene_list",
                label = p("3. Input your genes (",
                          tags$style(type = "text/css", "#load_example_glist {display: inline-block;height: 20px;padding: 0;vertical-align: baseline;}"),
                          actionLink("load_example_glist", label = tags$u("example data")),
                          "):"
                          ),
                placeholder = "Paste your genes here ...",
                height = 110
            ),
            fluidRow(
                column(
                    width = 6,
                    textInput(
                        "glist_name",
                        NULL,
                        placeholder = 'Name your list ...'
                    )
                ),
                column(
                    width = 2, #offset = 6,
                    bsButton(
                        inputId = "gene_list_clear",
                        label = "Reset",
                        style = "default"
                    )
                ),
                column(
                    width = 3, #offset = 1,
                    uiOutput("glist_add_button")
                )
                
            )
        )
    })
    
    # Glist add button
    output$glist_add_button <- renderUI({
      req(is.null(rv$gene_lists)|is.null(rv$glist_check))
      bsButton(
        inputId = "gene_list_add",
        label = "Submit",
        style = "primary"
      )
    })
    
    #----------------read in GList-----------------
    # from input field
    observeEvent(input$gene_list_add,{
        species = isolate(input$selected_species)  
        # print(input$gene_list)
        if(nchar(species)>2){
            if(input$gene_list != ""){
                shinyjs::disable("gene_list")
                shinyjs::disable("glist_name")
                
                genelist = as.character(input$gene_list)
                genelist = gsub("\"","",genelist)
                genelist = strsplit(genelist,"\n")
                genelist = unlist(lapply(genelist, function(x) strsplit(x,'\\s*,\\s*')))
                genelist = unlist(lapply(genelist, function(x) strsplit(x,'\\s*;\\s*')))
                genelist = unlist(strsplit(genelist," "))
                genelist = unique(genelist)
                
                if(is.null(genelist)==F){
                    # save original gene lists into RV
                    rv$gene_lists = genelist
                    
                    # autodetect and convert into SYMBOL (if other/mixed identifier) using gprofiler2
                    if(input$gene_identifier == "other"){
                      withProgress(message = "Autodetecting and converting gene IDs...",{
                        Sys.sleep(0.1)
                        incProgress(1)
                        lst = convert_gene_id(species,genelist)
                        
                        if(is.null(lst)){
                          # no ID detected in database
                          rv$glist_check = "none"
                        }else{
                          # check percentage of IDs found in database
                          g_perc = lst[[1]]
                          
                          # if <30%, reports error
                          if(g_perc < 0.5){
                            rv$glist_check = "low"
                          }else{
                            rv$glist_check = "pass"
                          }
                          
                          # convert ID and save converted IDs & conversion table into RVs
                          rv$gene_lists_after = lst[[2]]
                          rv$gene_lists_mat = lst[[3]]
                          
                        }
                      })
                    }else{
                      rv$gene_lists_after = rv$gene_lists
                      rv$glist_check = "pass"
                    }

                    # name the analysis
                    if(input$glist_name == ""){
                        rv$rnkll = "unamed"
                    }else{
                        rv$rnkll = input$glist_name
                    }
                }
            }else{
                showNotification("Please input your query. Click example data for a trial run.",type="error",duration=3)
            }
        }else{
            showNotification("Please select species that matches your query.",type="error",duration=3)
        }
        # }else{
        #     rv$gene_lists = rv$data_glist
        # 
        #     # successfully submitted
        #     rv$glist_status = "submitted"
        # }
    })
    
    # clear GList input ------------------------------
    observeEvent(input$gene_list_clear, {
        rv$run = NULL
        
        rv$glist_check = NULL
        rv$gene_lists = NULL
        rv$gene_lists_after = NULL
        
        shinyjs::reset("gene_list")
        shinyjs::enable("gene_list")
        shinyjs::reset("glist_name")
        shinyjs::enable("glist_name")
        # updateTextAreaInput(session,
        #                     inputId = "gene_list",
        #                     value = "",
        #                     placeholder = "Type to input.."
        # )
    })
    
    #----------- Example GList --------------
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
        rv$gene_lists_after = NULL
    })
    
    # UI GSEA parameter
    output$ui_gsea_par <- renderUI({
      req(rv$db_status == "selected")
      
      if(input$selected_mode == "gsea"){
        req(is.null(rv$rnk_check)==F)
        req(is.null(rv$rnkgg)==F)
        
      }else if(input$selected_mode == "glist"){
        req(is.null(rv$glist_check)==F)
        req(is.null(rv$gene_lists_after)==F)
        
      }
      
      fluidRow(
        box(
            width = 12,
            shiny::HTML("<p style='font-style:italic'>Run parameters</p>"),
            splitLayout(
                numericInput("mymin", "Min:",15),
                numericInput("mymax", "Max:",200),
                uiOutput("ui_nperm")
            )
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
      req(rv$db_status == "selected")
      
      if(input$selected_mode == "gsea"){
        req(is.null(rv$rnk_check)==F)
        req(is.null(rv$rnkgg)==F)
        
      }else if(input$selected_mode == "glist"){
        req(is.null(rv$glist_check)==F)
        req(is.null(rv$gene_lists_after)==F)
        
      }
        
      bsButton(inputId = "confirm1", 
               label = "RUN GSEA",
               # block = TRUE,
               icon = icon("play-circle"), 
               style = "danger")
    })
    
    # UI confirm GList
    output$run_GList <- renderUI({
      req(input$selected_mode == "glist")
      req(rv$db_status == "selected")
      
      if(input$selected_mode == "gsea"){
        req(is.null(rv$rnk_check)==F)
        req(is.null(rv$rnkgg)==F)
        
      }else if(input$selected_mode == "glist"){
        req(is.null(rv$glist_check)==F)
        req(is.null(rv$gene_lists_after)==F)
        
      }
      
      bsButton(inputId = "confirm2", 
               label = "RUN GLIST", 
               # block = TRUE,
               icon = icon("play-circle"), 
               style = "danger")
        
    })

    
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

        rv$bar_pathway = rv$dbs
        rv$bubble_pathway = rv$dbs
        rv$volcano_pathway = rv$dbs
        
        species <- isolate(input$selected_species)
        
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
            
            
            # count number of filtered GSs in GMTs
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
        
        genelist = toupper(rv$gene_lists_after)
        
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
                rv$fgseagg <- rbind(rv$fgseagg, fgseaRes)
                rv$no_up_01 = rv$no_up_01 + sum(fgseaRes$padj<0.01,na.rm=TRUE)
                rv$no_up_05 = rv$no_up_05 + sum(fgseaRes$padj<0.05,na.rm=TRUE)
                incProgress(0.2)
            }
            
            # count number of filtered GSs in GMTs
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